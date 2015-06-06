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
               , pciDevIlr       :: !Word8

               , pciDevBars      :: [PciBar] }
                 deriving Show

data PciBar = PciBar
            { pciBarBase :: !Word32
            , pciBarSize :: !Word32 }
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
           , pciRead32 :: PciSlot -> Word8 -> IO Word32 }

-- PCI field indices
pciVendorField, pciDeviceField, pciHeaderTypeField, pciSubVendorField, pciSubDeviceField, pciBaseClassField, pciSubClassField, pciInfClassField :: Word8
pciVendorField = 0x0
pciDeviceField = 0x2
pciHeaderTypeField = 0xE
pciSubVendorField = 0x2C
pciSubDeviceField = 0x2E
pciBaseClassField = 0xB
pciSubClassField = 0xA
pciInfClassField = 0x9

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
