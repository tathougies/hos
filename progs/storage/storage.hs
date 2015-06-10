module Main where

import Hos.User.SysCall
import Hos.User.IPC
import Hos.Init.Msg
import Hos.Storage.Msg
import Hos.Common.Bundle
import Hos.Common.Types

import Control.Monad

import Data.Binary
import Data.Word
import Data.Bits
import Data.List
import Data.Elf

import Foreign.Ptr
import Foreign.Storable

import Numeric

import Hos.Storage

pageSize :: Word64
pageSize = 0x1000

main :: IO ()
main = do hosDebugLog "[storage] registering as 'hos.storage'..."
          initResponse <- transmitMsg (ChanId 0) (ServerName "hos.init") (ChanId 0) (InitRegisterProvider "hos.storage")
--          forever $ return ()
          case initResponse of
            Right InitSuccess -> hosDebugLog "[storage] successfully registered!"
            Right resp -> hosDebugLog ("[storage] init gave error: " ++ show resp)
            Left (CouldNotParseResponse err) -> hosDebugLog ("[storage] could not decode response: "++ err)
          modCount <- hosModuleCount
          if modCount < 3
             then hosDebugLog "[storage] [FATAL] no boot bundle found"
             else do loadBootBundle

alignToPage :: Word64 -> Word64
alignToPage a = a .&. complement (pageSize - 1)

alignUpToPage :: Word64 -> Word64
alignUpToPage a = alignToPage $ a + pageSize - 1

hasTag :: String -> (TagValue -> Bool) -> BundleItem l -> Bool
hasTag tag checkValue bi = case findTag tag (biTags bi) of
                             Just tagV -> checkValue tagV
                             Nothing -> False

findTag :: String -> [Tag] -> Maybe TagValue
findTag _ [] = Nothing
findTag needle ((Tag (TagName name) v):tags)
    | needle == name = Just v
    | otherwise = findTag needle tags

tagValueToStr :: TagValue -> String
tagValueToStr (TextV x) = x
tagValueToStr x = show x

intercalate a xs = concat (intersperse a xs)

parse :: Binary a => Ptr () -> Word64 -> (a -> Ptr () -> IO ()) -> IO ()
parse p sz f = doGet get p sz >>=
             \res -> case res of
                       Left err -> hosDebugLog ("[storage] no parse: " ++ show err)
                       Right (a, p, sz) -> f a p

loadBootBundle :: IO ()
loadBootBundle =
    do ModuleInfo _ start end <- hosGetModuleInfo 2
       x <- hosDebugLog ("[storage] found bundle at " ++ showHex start (" - " ++ showHex end ""))
       -- Now we want to map in the third multiboot module
       -- This module is an image in the standard hos bundle format,
       -- which the storage server can read natively. The bundle should
       -- built using the hos-build-bundle command line tool, included
       -- in the Hos distribution
       let vBase = 0xC000000000 :: Word64
           vEnd = (fromIntegral (end - start)) + vBase
       hosAddMappingToCurTask vBase vEnd (FromPhysical RetainInParent (UserSpace ReadOnly) (fromIntegral start))
       hosDebugLog "[storage] mapped boot bundle. Going to read..."
       parse (wordToPtr vBase) (fromIntegral (end - start)) $
             \(tags, bundle) afterHdrPtr ->
                  do let modOffsetPtr = alignUpToPage (ptrToWord afterHdrPtr)
                         physModOffsetPtr = (modOffsetPtr - vBase) + fromIntegral start
                         isAutobootable = hasTag "com.hos.autoboot" (\v -> case v of { BooleanV True -> True; _ -> False })
                         autobootItems = filter isAutobootable (bundleContents (bundle :: Bundle (Word64, Word64)))
                         autobootServiceName = maybe "" tagValueToStr . findTag "com.hos.service-name" . biTags

                         bTags :: [TagDescriptor]
                         bTags = tags
                     forM_ autobootItems $ \item ->
                         let serviceName = autobootServiceName item
                             (offset, size) = biLocation item
                         in loadElf serviceName (wordToPtr (modOffsetPtr + offset)) (physModOffsetPtr + offset) size
                     -- Now we're going to wait and listen for messages on (ChanId 0)
                     hosDebugLog "[storage] listening..."
                     forever $ serveBundle modOffsetPtr physModOffsetPtr bundle

serveBundle :: Word64 -> Word64 -> Bundle (Word64, Word64) -> IO ()
serveBundle modOffsetPtr physModOffsetPtr bundle =
    do x <- hosAddMappingToCurTask 0x10000002000 0x10000003000 (Message (Incoming (MessageFrom (ChanId 0))) undefined)
       let msgPtr = wordToPtr 0x10000002000
           replyPtr = wordToPtr 0x10000003000
       hosUnmaskChannel (ChanId 0)
--       hosUnmaskChannel (ChanId 0xBADBEEF)
       hosWaitOnChannels waitForever 100
       msg <- getRoutedMsg "hos.storage" msgPtr 0x1000
       case msg of
         Right (OurMsg msg) ->
             case msg of
               StoragePerformQuery _ (TagIs (TagName name) value) ->
                   do hosAddMappingToCurTask 0x10000003000 0x10000004000 (Message (Outgoing (ReplyTo (ChanId 0))) undefined)
                      let matchesTag = hasTag name (== value)
                      case filter (matchesTag . snd) (zip [0..] (bundleContents bundle)) of
                        (i, _):_ -> do serializeTo replyPtr 0x1000 (StorageQueryResult (ObId i))
                                       hosReplyTo (ChanId 0)
                                       return ()
                        _ -> do serializeTo replyPtr 0x1000 (StorageQueryDone)
                                hosReplyTo (ChanId 0)
                                return ()
               StorageExecute (ObId obId) args ->
                   do let (offset, size) = biLocation (bundleContents bundle !! (fromIntegral obId))
                      hosAddMappingToCurTask 0x10000003000 0x10000004000 (Message (Outgoing (ReplyTo (ChanId 0))) undefined)
                      childId <- loadElf (show obId) (wordToPtr (modOffsetPtr + offset)) (physModOffsetPtr + offset) size
                      serializeTo replyPtr 0x1000 (StorageStartedTask childId)
                      hosReplyTo (ChanId 0)

                      -- Now send the arguments to the child
                      initSendArgs childId args
                      return ()
               _ -> hosDebugLog "[storage] got unrecognized message"
         Left err -> hosDebugLog ("[storage] error decoding: " ++ show err)
         Right _ -> hosDebugLog "[storage] got message that was not for us.."
