{-# LANGUAGE ForeignFunctionInterface #-}
module Hos.User.SysCall where

import Control.Exception

import Data.Word
import Data.Char

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "syscall.h syscall" syscall :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> IO Word64
foreign import ccall "hos.h ptr_to_word" ptrToWord :: Ptr a -> Word64

writeCString :: String -> (Ptr Char -> IO a) -> IO a
writeCString string f =
    bracket (mallocBytes (length string + 1)) (free) $ \p ->
      wrstr (castPtr p) string >> f p
    where wrstr p "" = poke p (0 :: Word8)
          wrstr p (x:xs) = poke p (fromIntegral (ord x) :: Word8) >> wrstr (p `plusPtr` 1) xs

readCStringN :: Ptr Char -> Int -> IO String
readCStringN p n = go (castPtr p) n ""
    where go _ 0 a = return a
          go p n a = do c <- peek (p :: Ptr Word8)
                        if c == 0 then return a else go (p `plusPtr` 1) (n - 1) (a  ++ [chr (fromIntegral c)])

hosDebugLog :: String -> IO ()
hosDebugLog x = writeCString x $ \xp -> syscall 0 (ptrToWord xp) 0 0 0 0 >> return ()

hosFork :: IO Int
hosFork = syscall 0x402 0 0 0 0 0 >>= return . fromIntegral

hosYield :: IO ()
hosYield = do x <- syscall 0x403 0 0 0 0 0
              x `seq` return ()

hosModuleCount :: IO Word8
hosModuleCount = do x <- syscall 0xff00 0 0 0 0 0
                    return (fromIntegral x)

data ModuleInfo = ModuleInfo String Word32 Word32
                  deriving Show

instance Storable ModuleInfo where
    sizeOf _ = 128
    alignment _ = 8

    peek p = do start <- peek (castPtr p :: Ptr Word32)
                end <- peek (castPtr p `plusPtr` 4:: Ptr Word32)
                name <- readCStringN (castPtr p `plusPtr` 8) 40
                return (ModuleInfo name start end)

hosGetModuleInfo :: Word8 -> IO ModuleInfo
hosGetModuleInfo i =
    bracket malloc free $ \p ->
        do x <- syscall 0xff01 (fromIntegral i) (ptrToWord p) 0 0 0
           x `seq` peek p
