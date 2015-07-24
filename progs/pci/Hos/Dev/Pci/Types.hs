module Hos.Dev.Pci.Types where

import Data.Word
import Data.Bits

data PciDevice = PciDevice
               { pciDevSlot      :: !PciSlot
               , pciDevBaseClass :: !Word8
               , pciDevSubClass  :: !Word8
               , pciDevInfClass  :: !Word8
               , pciDevVid       :: !Word16
               , pciDevDid       :: !Word16
               , pciDevSubVid    :: !Word16
               , pciDevSubDid    :: !Word16

               , pciDevIrq       :: !Word8

               , pciDevBars      :: [PciBar] }
                 deriving Show

data PciBar = PciBarMemory
            { pciBarBase :: !Word64
            , pciBarSize :: !Word64 }
            | PciBarIO
            { pciBarIOBase :: !Word32
            , pciBarIOSize :: !Word32 }
            | PciBarPleaseAllocateIO Word32
            | PciBarPleaseAllocateMem32 Word32
            | PciBarPleaseAllocateMem64 Word64
              deriving Show

data PciSlot = PciSlot
             { pciSlotBusInd  :: !Word8
             , pciSlotDevInd  :: !Word8
             , pciSlotFuncInd :: !Word8 }
               deriving Show

data PciStatus = PciStatus
               { pciStatusSignaledSystemError       :: !Bool
               , pciStatusReceivedMasterAbortStatus :: !Bool
               , pciStatusReceivedTargetAbortStatus :: !Bool
               , pciStatusHasCapabalitiesList       :: !Bool }
                 deriving Show

data PciProbeResult = NoDeviceInSlot
                      deriving Show

data PciIO = PciIO
           { pciRead8 :: PciSlot -> Word8 -> IO Word8
           , pciRead16 :: PciSlot -> Word8 -> IO Word16
           , pciRead32 :: PciSlot -> Word8 -> IO Word32

           , pciWrite32 :: PciSlot -> Word8 -> Word32 -> IO ()}

-- PCI field indices
pciVendorField, pciDeviceField, pciHeaderTypeField, pciSubVendorField, pciSubDeviceField, pciBaseClassField, pciSubClassField, pciInfClassField, pciIrqField :: Word8
pciVendorField = 0x0
pciDeviceField = 0x2
pciHeaderTypeField = 0xE
pciSubVendorField = 0x2C
pciSubDeviceField = 0x2E
pciBaseClassField = 0xB
pciSubClassField = 0xA
pciInfClassField = 0x9
pciIrqField = 0x3C

pciBar0Field, pciBar1Field, pciBar2Field, pciBar3Field, pciBar4Field, pciBar5Field :: Word8
pciBar0Field = 0x10
pciBar1Field = 0x14
pciBar2Field = 0x18
pciBar3Field = 0x1C
pciBar4Field = 0x20
pciBar5Field = 0x24

probingStatus :: PciStatus
probingStatus = PciStatus True True True False

statusToWord :: PciStatus -> Word16
statusToWord (PciStatus sse rmas rtas capptr) =
    (if sse then 0x4000 else 0) .|.
    (if rmas then 0x2000 else 0) .|.
    (if rtas then 0x1000 else 0) .|.
    (if capptr then 0x10 else 0)

wordToStatus :: Word16 -> PciStatus
wordToStatus w = PciStatus (testBit w 14) (testBit w 13) (testBit w 12) (testBit w 4)
