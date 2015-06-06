module Hos.User.IO where

import Data.Word

#if TARGET==x86_64
foreign import ccall "x64.h arch_in_32" hosIn32 :: Word16 -> IO Word32
foreign import ccall "x64.h arch_out_32" hosOut32 :: Word16 -> Word32 -> IO ()
#else
#error "No architecture given"
#endif
