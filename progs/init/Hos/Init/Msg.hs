module Hos.Init.Msg where

import Hos.Common.Types
import Hos.User.SysCall
import Hos.User.IPC

import Data.Binary
import Data.Word

data InitMessage = InitRegisterProvider String
                 | InitFindProvider String
                 | InitSendArgs TaskId [(String, String)]
                   deriving (Show, Eq)

data InitResponse = InitSuccess
                  | InitFoundProvider TaskId
                  | InitAlreadyRegistered
                  | InitNotFound
                    deriving (Show, Eq)

instance Binary InitMessage where
    put (InitRegisterProvider name) = put (0x0 :: Word16) <> put name
    put (InitFindProvider name) = put (0x1 :: Word16) <> put name
    put (InitSendArgs (TaskId tId) args) = put (0x2 :: Word16) <> put tId <> put args

    get = (get :: Get Word16) `gBind` \tag ->
          case tag of
            0x0 -> get `gBind` (gReturn . InitRegisterProvider)
            0x1 -> get `gBind` (gReturn . InitFindProvider)
            0x2 -> get `gBind` \tid ->
                   get `gBind` \args ->
                   gReturn (InitSendArgs (TaskId tid) args)
            _ -> gFail ("Unknown InitMessage type: " ++ show tag)

instance Binary InitResponse where
    put InitSuccess = put (0x0 :: Word8)
    put (InitFoundProvider (TaskId id)) = put (0x1 :: Word8) <> put id
    put InitAlreadyRegistered = put (0xFF :: Word8)
    put InitNotFound = put (0xFE :: Word8)

    get = (get :: Get Word8) `gBind` \tag ->
          case tag of
            0x0 -> gReturn InitSuccess
            0x1 -> get `gBind` (gReturn . InitFoundProvider . TaskId)
            0xFE -> gReturn InitNotFound
            0xFF -> gReturn InitAlreadyRegistered
            _ -> gFail ("InitResponse: unknown type: " ++ show tag)

initRegisterProvider :: String -> IO (Maybe InitResponse)
initRegisterProvider svrName =
    transmitMsg (ChanId 0) (ServerName "hos.init") (ChanId 0) (InitRegisterProvider svrName) >>= \res ->
    case res of
      Right x -> return (Just x)
      Left err -> hosDebugLog ("initRegisterProvider: " ++ show err) >> return Nothing

initFindProvider :: ServerName -> IO (Maybe TaskId)
initFindProvider (ServerName serverName) =
    transmitMsg (ChanId 0) (ServerName "hos.init") (ChanId 0) (InitFindProvider serverName) >>= \res ->
    case res of
      Right (InitFoundProvider id) -> return (Just id)
      Right x -> hosDebugLog ("Could not find provider: " ++ show x) >> return Nothing
      Left err -> hosDebugLog ("Error on findprovider: " ++ show err) >> return Nothing

initSendArgs :: TaskId -> [(String, String)] -> IO ()
initSendArgs tid args =
    transmitMsg (ChanId 0) (ServerName "hos.init") (ChanId 0) (InitSendArgs tid args) >>= \res ->
    case res of
      Right InitSuccess -> return ()
      _ -> hosDebugLog "initSendArgs: got funny response"
