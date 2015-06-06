module Hos.Dev.Pci.Arch where

import Hos.User.IO

import Hos.Dev.Pci.Types

import Data.Word
import Data.Bits

#if TARGET==x86_64
archPciRead8 :: PciSlot -> Word8 -> IO Word8
archPciRead8 slot offset =
    let aligned = offset .&. complement 0x3
        alignedOffset = fromIntegral (offset - aligned)
    in archPciRead32 slot aligned >>= \alignedWord ->
       return (fromIntegral ((alignedWord `shiftR` (8 * alignedOffset)) .&. 0xFF))

archPciRead16 :: PciSlot -> Word8 -> IO Word16
archPciRead16 slot offset =
    let aligned = offset .&. complement 0x3
        alignedOffset = fromIntegral (offset - aligned)
    in archPciRead32 slot aligned >>= \alignedWord ->
       return (fromIntegral ((alignedWord `shiftR` (8 * alignedOffset)) .&. 0xFFFF))

archPciRead32 :: PciSlot -> Word8 -> IO Word32
archPciRead32 (PciSlot bus dev func) off =
    do let addr :: Word32
           addr = (fromIntegral 1 `shiftL` 31) .|. (fromIntegral bus `shiftL` 16) .|.
                  (fromIntegral dev `shiftL` 11) .|. (fromIntegral func `shiftL` 8) .|.
                  fromIntegral off
       hosOut32 0xCF8 addr
       hosIn32 0xCFC
#endif

archPciIO :: PciIO
archPciIO = PciIO
          { pciRead8 = archPciRead8
          , pciRead16 = archPciRead16
          , pciRead32 = archPciRead32 }
