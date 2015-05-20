module Hos.Memory where

import Hos.Types
import Hos.CBits

import Control.Monad
import Control.Exception

import Data.Word

import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

withMapping :: Arch r m e -> MemoryPermissions ->
               Word64 -> Word64 -> (Ptr a -> IO b) -> IO b
withMapping a perms v p f =
    bracket (archMapPage a v p perms)
            (\_ -> archUnmapPage a v)
            (\_ -> f (wordToPtr v))

memset :: Ptr a -> Word8 -> Word64 -> IO ()
memset p c sz = forM_ [0..(sz-1)] $ \i -> pokeElemOff (castPtr p) (fromIntegral i) c
