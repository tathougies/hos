module Hos.Dev.Pci.Probe where

import Hos.Dev.Pci.Types
import Hos.Dev.Pci.Arch

import Data.Bits

probeDeviceAndFunction :: PciIO -> PciSlot -> IO (Either PciProbeResult PciDevice)
probeDeviceAndFunction pci slot =
    do vendorId <- pciRead16 pci slot pciVendorField
       deviceId <- pciRead16 pci slot pciDeviceField

       case (vendorId, deviceId) of
         (0xFFFF, 0xFFFF) -> return (Left NoDeviceInSlot)
         _ -> do subVendorId <- pciRead16 pci slot pciSubVendorField
                 subDeviceId <- pciRead16 pci slot pciSubDeviceField

                 baseClass <- pciRead8 pci slot pciBaseClassField
                 subClass <- pciRead8 pci slot pciSubClassField
                 infClass <- pciRead8 pci slot pciInfClassField

                 let dev = PciDevice slot baseClass subClass infClass vendorId deviceId subVendorId subDeviceId 0 []
                 return (Right dev)

probeDevice :: PciIO -> PciSlot -> [PciDevice] -> IO [PciDevice]
probeDevice pci slot a = go
    where go = do headerType <- pciRead16 pci slot pciHeaderTypeField
                  if headerType .&. 0x80 == 0 then probeOneFunction slot else continueProbe 0 a

          probeOneFunction slot = probeDeviceAndFunction pci slot >>= \res ->
                                  case res of
                                    Left _ -> return a
                                    Right x -> return (x : a)

          continueProbe 8 a = return a
          continueProbe n a = probeDeviceAndFunction pci (PciSlot (pciSlotBusInd slot) (pciSlotDevInd slot) n) >>= \res ->
                              case res of
                                Left _ -> continueProbe (n + 1) a
                                Right dev -> continueProbe (n + 1) (dev : a)

probeMainBus :: PciIO -> IO [PciDevice]
probeMainBus pci = go pci 0 []
    where go _ 32 a = return a
          go pci devInd a =
              probeDevice pci (PciSlot 0 devInd 0) a >>= go pci (devInd + 1)
