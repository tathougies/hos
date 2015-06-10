{-# LANGUAGE BangPatterns #-}
module Hos.User.IPC where

import Hos.Common.Types
import Hos.User.SysCall

import Control.Monad

import Data.Binary
import Data.Word
import Data.Monoid
import Data.Bits
import Data.Char

import Foreign.Ptr

newtype ServerName = ServerName String
    deriving (Show, Eq, Ord)

data RoutedMessage a = RoutedMessage ServerName ChanId a
                       deriving (Show, Eq, Ord)

data TransmitError = CouldNotParseResponse String
                   | CouldNotMapMsg | CouldNotMapReply
                     deriving Show

replyChanFor :: ChanId -> ChanId
replyChanFor (ChanId c) = ChanId (c `setBit` 31)

transmitMsg' :: (Binary a, Binary res) => ChanId -> ServerName -> ChanId -> a -> (Ptr (), Word64) -> (Ptr (), Word64) -> IO (Either TransmitError res)
transmitMsg' srcChan server dstChan msg (msgPtr, msgSz) (replyPtr, replySz) =
    do serializeTo msgPtr msgSz (RoutedMessage server dstChan msg)
       i <- hosDeliverMessage srcChan (TaskId 0) dstChan
       replyRes <- hosAddMappingToCurTask (ptrToWord replyPtr) (ptrToWord replyPtr + replySz) (Message (Incoming (ReplyTo (ChanId 0))) undefined)
       if replyRes /= 0
          then return (Left CouldNotMapReply)
           else do hosUnmaskChannel (replyChanFor srcChan)
                   hosWaitOnChannels waitForever 0
                   initResponse <- getFrom replyPtr replySz
                   case initResponse of
                     Right resp -> return (Right resp)
                     Left err -> return (Left (CouldNotParseResponse err))

transmitMsg :: (Binary msg, Binary res) => ChanId -> ServerName -> ChanId -> msg -> IO (Either TransmitError res)
transmitMsg srcChan server dstChan msg =
    do let msgPtr = wordToPtr 0x10000000000
           replyPtr = wordToPtr 0x10000001000
       msgRes <- hosAddMappingToCurTask 0x10000000000 0x10000001000 (Message (Outgoing (MessageFrom (ChanId 0))) undefined)
       if msgRes /= 0 then return (Left CouldNotMapMsg) else transmitMsg' srcChan server dstChan msg (msgPtr, 0x1000) (replyPtr, 0x1000)

instance Binary ServerName where
    put (ServerName x)
        | length x >= 248 = mconcat $
                            map put (take 248 x)
        | otherwise = mconcat (map put x) <> mconcat (replicate (248 - length x) (put '\0'))
    get = go 0 id
        where go 248 a = gReturn (ServerName (a ""))
              go !n !a = (get :: Get Word8) `gBind` \c ->
                         if c /= 0
                           then go (n + 1) (a . (chr (fromIntegral c) :))
                           else foldr (\a b -> a `gBind` (const b)) (gReturn (ServerName (a ""))) (replicate (248 - n - 1) (get :: Get Word8))

instance Binary a => Binary (RoutedMessage a) where
    put (RoutedMessage name (ChanId chanId) a) =
        put (0xFFFFFFFF :: Word32) <>
        put chanId <>
        put name <>
        put a
    get = (get :: Get Word32) `gBind` \tag ->
          if tag /= 0xFFFFFFFF then gFail "RoutedMessage: expected tag"
             else get `gBind` \chanId -> get `gBind` \name -> get `gBind` \a -> gReturn (RoutedMessage name (ChanId chanId) a)

data RouteResult a = OurMsg a
                   | InTransitMsg ServerName ChanId

getRoutedMsg :: Binary a => String -> Ptr () -> Word64 -> IO (Either String (RouteResult a))
getRoutedMsg ourName p sz = go
    where go = doGet routedMsgHdr p sz >>= \res ->
               case res of
                 Left err -> return (Left err)
                 Right ((name@(ServerName rawName), chanId), p', sz')
                     | rawName == ourName ->
                         getFrom p' sz' >>= \aRes ->
                         case aRes of
                           Left err -> return (Left ("Looked at " ++ show p' ++ " " ++ err))
                           Right a -> return (Right (OurMsg a))
                     | otherwise -> return (Right (InTransitMsg name (ChanId chanId)))

          routedMsgHdr = (get :: Get Word32) `gBind` \tag ->
                         if tag /= 0xFFFFFFFF then gFail "RoutedMessage: expected tag"
                            else get `gBind` \chanId -> get `gBind` \name -> gReturn (name, chanId)

hosReadEnvironment :: IO [(String, String)]
hosReadEnvironment =
    do hosAddMappingToCurTask 0xC000000000 0xC000001000 (Message (Incoming (MessageFrom (ChanId 0))) undefined)
       hosUnmaskChannel (ChanId 0)
       hosWaitOnChannels waitForever 100
       res <- getFrom (wordToPtr 0xC000000000) 0x1000
       case res of
         Left err -> hosDebugLog ("Could not read environment") >> return []
         Right x ->
             do hosAddMappingToCurTask 0xC000001000 0xC000002000 (Message (Outgoing (ReplyTo (ChanId 0))) undefined)
                hosReplyTo (ChanId 0xBADBEEF)
                return x
