{-# LANGUAGE ForeignFunctionInterface #-}
module Hos.User.SysCall where

import Control.Exception
import Control.Applicative

import Data.Word
import Data.Char
import Data.Bits

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Hos.Common.Types

foreign import ccall "syscall.h syscall" syscall :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> IO Word64
foreign import ccall "hos.h ptr_to_word" ptrToWord :: Ptr a -> Word64
foreign import ccall "hos.h word_to_ptr" wordToPtr :: Word64 -> Ptr a

writeCString :: String -> (Ptr Char -> Int -> IO a) -> IO a
writeCString string f =
    bracket (mallocBytes sLength) (free) $ \p ->
      wrstr (castPtr p) string >> f p sLength
    where wrstr p "" = return ()
          wrstr p (x:xs) = poke p (fromIntegral (ord x) :: Word8) >> wrstr (p `plusPtr` 1) xs

          sLength = length string

readCStringN :: Ptr Char -> Int -> IO String
readCStringN p n = go (castPtr p) n ""
    where go _ 0 a = return a
          go p n a = do c <- peek (p :: Ptr Word8)
                        if c == 0 then return a else go (p `plusPtr` 1) (n - 1) (a  ++ [chr (fromIntegral c)])

hosRequestIO :: IO Word64
hosRequestIO = syscall 0x300 0 0 0 0 0

hosDebugLog :: String -> IO ()
hosDebugLog x = writeCString x $ \xp len -> syscall 0 (ptrToWord xp) (fromIntegral len) 0 0 0 >> return ()

hosPhysAddrFor :: Word64 -> IO Word64
hosPhysAddrFor x = bracket malloc free $ \wP -> syscall 8 x (ptrToWord wP) 0 0 0 >> peek wP

hosFork :: IO Int
hosFork = syscall 0x402 0 0 0 0 0 >>= return . fromIntegral

hosYield :: IO ()
hosYield = do x <- syscall 0x403 0 0 0 0 0
              x `seq` return ()

hosModuleCount :: IO Word8
hosModuleCount = do x <- syscall 0xff00 0 0 0 0 0
                    return (fromIntegral x)

-- * Task management

hosCurrentTask :: IO TaskId
hosCurrentTask = TaskId . fromIntegral <$> syscall 0x401 0 0 0 0 0

-- * Memory management

hosCurrentAddressSpace :: TaskId -> IO AddressSpaceRef
hosCurrentAddressSpace (TaskId tId) = AddressSpaceRef . fromIntegral <$> syscall 0x001 (fromIntegral tId) 0 0 0 0

hosEmptyAddressSpace :: IO AddressSpaceRef
hosEmptyAddressSpace = AddressSpaceRef . fromIntegral <$> syscall 0x006 0 0 0 0 0

hosEnterAddressSpace :: AddressSpaceRef -> Word64 -> IO ()
hosEnterAddressSpace (AddressSpaceRef aRef) entry = syscall 0x007 (fromIntegral aRef) entry 0 0 0 >> return ()

hosAddMapping :: AddressSpaceRef -> Word64 -> Word64 -> Mapping -> IO Word64
hosAddMapping (AddressSpaceRef aRef) start end mapping =
    bracket malloc free $ \mappingP ->
        do poke mappingP mapping
           syscall 0x002 (fromIntegral aRef) start end (ptrToWord mappingP) 0

hosSwitchToAddressSpace :: TaskId -> AddressSpaceRef -> IO Word64
hosSwitchToAddressSpace (TaskId tId) (AddressSpaceRef aRef) =
    syscall 0x005 (fromIntegral tId) (fromIntegral aRef) 0 0 0

hosCloseAddressSpace :: AddressSpaceRef -> IO ()
hosCloseAddressSpace (AddressSpaceRef aRef) =
    syscall 0x004 (fromIntegral aRef) 0 0 0 0 >>
    return ()

hosAddMappingToCurTask :: Word64 -> Word64 -> Mapping -> IO Word64
hosAddMappingToCurTask begin end mapping =
    hosAddMapping curAddressSpaceRef begin end mapping

data ModuleInfo = ModuleInfo String Word32 Word32
                  deriving Show

instance Storable ModuleInfo where
    sizeOf _ = 128
    alignment _ = 8

    peek p = do start <- peek (castPtr p :: Ptr Word32)
                end <- peek (castPtr p `plusPtr` 4:: Ptr Word32)
                name <- readCStringN (castPtr p `plusPtr` 8) 120
                return (ModuleInfo name start end)

hosGetModuleInfo :: Word8 -> IO ModuleInfo
hosGetModuleInfo i =
    bracket (mallocBytes 500) free $ \p ->
        do x <- syscall 0xff01 (fromIntegral i) (ptrToWord p) 0 0 0
           x `seq` peek p

-- IPC
hosUnmaskChannel :: ChanId -> IO ()
hosUnmaskChannel (ChanId i) = syscall 0x104 (fromIntegral i) 0 0 0 0 >> return ()

data WaitOnChannelsError = WaitOnChannelsNoMessages
                         | WaitOnChannelsMessageTruncated
                           deriving Show

hosWaitOnChannels :: WaitOnChannelsFlags -> Word64 -> IO (Either WaitOnChannelsError (ChanId, TaskId))
hosWaitOnChannels (WaitOnChannelsFlags waitForever dontTruncate noMask) timeout =
    bracket malloc free $ \(taskP :: Ptr Word32) ->
     do status <- syscall 0x103 flags timeout (ptrToWord taskP) 0 0
        if status .&. 0x100000000 /= 0
          then case (status :: Word64) .&. 0xffffffff of
                 0x10301 -> return (Left WaitOnChannelsNoMessages)
                 0x10300 -> return (Left WaitOnChannelsMessageTruncated)
                 _ -> fail "hosWaitOnChannels: Returned bizarre status"
          else do let chanId = ChanId (fromIntegral (status .&. 0xffffffff))
                  taskId <- TaskId <$> peek taskP
                  return (Right (chanId, taskId))
    where flags :: Word64
          flags = (if waitForever then bit 0 else 0) .|.
                  (if dontTruncate then bit 1 else 0) .|.
                  (if noMask then bit 2 else 0)

hosDeliverMessage :: ChanId -> TaskId -> ChanId -> IO Word8
hosDeliverMessage (ChanId srcChan) (TaskId dst) (ChanId dstChan) =
    fromIntegral <$> syscall 0x100 (fromIntegral srcChan) (fromIntegral dst) (fromIntegral dstChan) 0 0

hosReplyTo :: ChanId -> IO Word8
hosReplyTo (ChanId onChan) = fromIntegral <$> syscall 0x102 (fromIntegral onChan) 0 0 0 0

hosRouteMsg :: ChanId -> TaskId -> ChanId -> IO Word8
hosRouteMsg (ChanId srcChan) (TaskId dst) (ChanId dstChan) =
    fromIntegral <$> syscall 0x101 (fromIntegral srcChan) (fromIntegral dst) (fromIntegral dstChan) 0 0
