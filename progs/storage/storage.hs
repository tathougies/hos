module Main where

import Hos.User.SysCall
import Hos.Common.Bundle
import Hos.Common.Types

import Control.Monad

import Data.Binary
import Data.Word
import Data.Bits
import Data.List
import Data.Elf

import Foreign.Ptr

import Numeric

import Hos.Storage

pageSize :: Word64
pageSize = 0x1000

main :: IO ()
main = do hosDebugLog "[storage] starting..."
          modCount <- hosModuleCount
          if modCount < 3
             then hosDebugLog "[storage] [FATAL] no boot bundle found"
             else loadBootBundle

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
