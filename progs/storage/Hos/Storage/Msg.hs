module Hos.Storage.Msg where

import Hos.Common.Types
import Hos.Common.Bundle
import Hos.User.SysCall
import Hos.User.IPC

import Data.Binary
import Data.Word

newtype TxId = TxId Word8
    deriving Show
newtype QId = QId Word8
    deriving Show
newtype ObId = ObId Word32
    deriving Show

data StorageQuery = AndQ StorageQuery StorageQuery
                  | OrQ StorageQuery StorageQuery
                  | NotQ StorageQuery

                  | TagIs TagName TagValue
                    deriving Show

data StorageMessage = StorageBeginTransaction
                    | StorageCommitTransaction TxId
                    | StorageAbortTransaction TxId
                    | StorageTransactionStatus TxId

                    | StoragePerformQuery TxId StorageQuery
                    | StorageNextResult TxId QId
                    | StorageFinishQuery QId

                    | StorageExecute ObId [(String, String)]
                      deriving Show

data StorageResponse = StorageSuccess
                     | StorageTransaction TxId TxStatus

                     | StorageQueryResult ObId
                     | StorageQueryDone

                     | StorageStartedTask TaskId

                     | StorageInsufficientPrivilege
                       deriving Show

data TxStatus = TxInProgress
              | TxAbortedByUser
              | TxAbortedForcefully
                deriving Show

instance Binary TxId where
    put (TxId a) = put a
    get = get `gBind` (gReturn . TxId)

instance Binary QId where
    put (QId a) = put a
    get = get `gBind` (gReturn . QId)

instance Binary ObId where
    put (ObId a) = put a
    get = get `gBind` (gReturn . ObId)

instance Binary TxStatus where
    put TxInProgress = put (0x0 :: Word8)
    put TxAbortedByUser = put (0x1 :: Word8)
    put TxAbortedForcefully = put (0x2 :: Word8)

    get = (get :: Get Word8) `gBind` \tag ->
          case tag of
            0x0 -> gReturn TxInProgress
            0x1 -> gReturn TxAbortedByUser
            0x2 -> gReturn TxAbortedForcefully

instance Binary StorageResponse where
    put StorageSuccess = put (0x0 :: Word8)
    put (StorageTransaction id status) = put (0x1 :: Word8) <> put id <> put status
    put (StorageStartedTask (TaskId id)) = put (0x2 :: Word8) <> put id
    put (StorageQueryResult id) = put (0x10 :: Word8) <> put id
    put StorageQueryDone = put (0x11 :: Word8)
    put StorageInsufficientPrivilege = put (0xff :: Word8)

    get = (get :: Get Word8) `gBind` \tag ->
          case tag of
            0x0 -> gReturn StorageSuccess
            0x1 -> get `gBind` \id ->
                   get `gBind` \status ->
                   gReturn (StorageTransaction id status)
            0x2 -> get `gBind` (gReturn . StorageStartedTask . TaskId)
            0x10 -> get `gBind` (gReturn . StorageQueryResult)
            0x11 -> gReturn StorageQueryDone
            0xFF -> gReturn StorageInsufficientPrivilege

instance Binary StorageQuery where
    put (AndQ a b) = put (0x1 :: Word8) <> put a <> put b
    put (OrQ a b) = put (0x2 :: Word8) <> put a <> put b
    put (NotQ a) = put (0x3 :: Word8) <> put a
    put (TagIs name val) = put (0x0 :: Word8) <> put name <> put val

    get = (get :: Get Word8) `gBind` \tag ->
          case tag of
            0x0 -> get `gBind` \name ->
                   get `gBind` \val ->
                   gReturn (TagIs name val)
            0x1 -> get `gBind` \a ->
                   get `gBind` \b ->
                   gReturn (AndQ a b)
            0x2 -> get `gBind` \a ->
                   get `gBind` \b ->
                   gReturn (OrQ a b)
            0x3 -> get `gBind` (gReturn . NotQ)

instance Binary StorageMessage where
    put StorageBeginTransaction = put (0x0 :: Word8)
    put (StorageCommitTransaction txId) = put (0x1 :: Word8) <> put txId
    put (StorageAbortTransaction txId) = put (0x2 :: Word8) <> put txId
    put (StorageTransactionStatus txId) = put (0x3 :: Word8) <> put txId
    put (StoragePerformQuery txId query) = put (0x4 :: Word8) <> put txId <> put query

    put (StorageExecute obId args) = put (0x5 :: Word8) <> put obId <> put args

    get = (get :: Get Word8) `gBind` \tag ->
          case tag of
            0 -> gReturn StorageBeginTransaction
            1 -> get `gBind` (gReturn . StorageCommitTransaction)
            2 -> get `gBind` (gReturn . StorageAbortTransaction)
            3 -> get `gBind` (gReturn . StorageTransactionStatus)
            4 -> get `gBind` \txId ->
                 get `gBind` \query ->
                 gReturn (StoragePerformQuery txId query)
            5 -> get `gBind` \obId ->
                 get `gBind` \args ->
                 gReturn (StorageExecute obId args)

storageQuery :: StorageQuery -> IO (Maybe ObId)
storageQuery q = do let msg = StoragePerformQuery (TxId 0) q
                    res <- transmitMsg (ChanId 0) (ServerName "hos.storage") (ChanId 0) msg
                    case res of
                      Left err -> hosDebugLog ("storageQuery: " ++ show err) >> return Nothing
                      Right (StorageQueryResult x) -> return (Just x)
                      Right StorageQueryDone -> return Nothing
                      Right _ -> hosDebugLog ("storageQuery: strange return value") >> return Nothing

storageExecute :: ObId -> [(String, String)] -> IO (Maybe TaskId)
storageExecute obId args =
    do let msg = StorageExecute obId args
       res <- transmitMsg (ChanId 0) (ServerName "hos.storage") (ChanId 0) msg
       case res of
         Left err -> hosDebugLog ("storageExecute: " ++ show err) >> return Nothing
         Right (StorageStartedTask id) -> return (Just id)
         Right _ -> hosDebugLog ("storageExecute: strange return value") >> return Nothing

withStorageTransaction :: IO () -> IO ()
withStorageTransaction = id
