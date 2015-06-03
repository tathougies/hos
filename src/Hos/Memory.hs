module Hos.Memory where

import Hos.Types
import Hos.CBits

import Control.Monad
import Control.Exception

import Data.Word
import Data.Char
import qualified Data.IntervalMap as IntervalMap

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
memset !p !c 0 = return ()
memset !p !c sz = poke (castPtr p) c >> memset (p `plusPtr` 1) c (sz - 1)

memcpy :: Ptr a -> Ptr a -> Word64 -> IO ()
memcpy !dst !src 0 = return ()
memcpy !dst !src sz = peek (castPtr src :: Ptr Word8) >>= poke (castPtr dst) >>
                      memcpy (dst `plusPtr` 1) (src `plusPtr` 1) (sz - 1)

addrSpaceWithMapping :: Word64 -> Word64 -> Mapping -> AddressSpace -> AddressSpace
addrSpaceWithMapping start end = IntervalMap.insert (start, end)
