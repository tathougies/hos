module Hos.User.IO where

import Data.Word

#if TARGET==x86_64
foreign import ccall "x64.h arch_in_32" hosIn32 :: Word16 -> IO Word32
foreign import ccall "x64.h arch_out_32" hosOut32 :: Word16 -> Word32 -> IO ()
foreign import ccall "x64.h arch_in_16" hosIn16 :: Word16 -> IO Word16
foreign import ccall "x64.h arch_out_16" hosOut16 :: Word16 -> Word16 -> IO ()
foreign import ccall "x64.h arch_in_8" hosIn8 :: Word16 -> IO Word8
foreign import ccall "x64.h arch_out_8" hosOut8 :: Word16 -> Word8 -> IO ()
#else
#error "No architecture given"
#endif
