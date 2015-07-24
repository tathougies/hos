module Hos.Dev.Pci.Probe where

import Hos.Dev.Pci.Types
import Hos.Dev.Pci.Arch

import Data.Bits
import Data.Word

pciReadBar :: PciIO -> PciSlot -> Word8 -> IO PciBar
pciReadBar pci slot field =
    pciRead32 pci slot field >>= \bar ->
    if bar `testBit` 0
       then do sz <- findBarSz False
               let bar' = bar .&. 0xFFFFFFFC
               if bar' == 0
                 then return (PciBarPleaseAllocateIO sz)
                 else return (PciBarIO bar' sz)
       else if ((bar `shiftR` 1) .&. 0x3) == 0
               then do sz <- findBarSz True
                       let bar' = fromIntegral (bar .&. 0xFFFFFFF0)
                       if bar' == 0
                          then return (PciBarPleaseAllocateMem32 sz)
                          else return (PciBarMemory bar' (fromIntegral sz))
               else do sz64 <- findBarSz64
                       bar1 <- pciRead32 pci slot (field + 4)
                       let bar' = (fromIntegral bar .|. (fromIntegral bar1 `shiftL` 32))
                       if bar' == 0
                          then return (PciBarPleaseAllocateMem64 sz64)
                          else return (PciBarMemory bar' sz64)

    where findBarSz isMem =
              do bar <- pciRead32 pci slot field
                 pciWrite32 pci slot field 0xFFFFFFFF
                 szMask <- pciRead32 pci slot field
                 let szMask' = if isMem
                               then szMask .&. 0xFFFFFFF0
                               else szMask .&. 0xFFFFFFFC
                 pciWrite32 pci slot field bar
                 return (complement szMask' + 1)
          findBarSz64 =
              do bar <- pciRead32 pci slot field
                 bar1 <- pciRead32 pci slot (field + 4)

                 pciWrite32 pci slot field 0xFFFFFFFF
                 pciWrite32 pci slot (field + 4) 0xFFFFFFFF

                 szMask <- pciRead32 pci slot field
                 szMask1 <- pciRead32 pci slot (field + 4)
                 let szMask' = szMask .&. 0xFFFFFFF0

                     szMask64 = fromIntegral szMask' .|. (fromIntegral szMask1 `shiftL` 32)
                 pciWrite32 pci slot field bar
                 pciWrite32 pci slot (field + 4) bar1
                 return (complement szMask64 + 1)

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

                 irq <- pciRead8 pci slot pciIrqField

                 headerType <- pciRead8 pci slot pciHeaderTypeField
                 case (headerType .&. 0x7f) of
                   0x0 -> do bar0 <- pciReadBar pci slot pciBar0Field
                             bar1 <- pciReadBar pci slot pciBar1Field
                             bar2 <- pciReadBar pci slot pciBar2Field
                             bar3 <- pciReadBar pci slot pciBar3Field
                             bar4 <- pciReadBar pci slot pciBar4Field
                             bar5 <- pciReadBar pci slot pciBar5Field
                             return $ Right $
                                      PciDevice slot baseClass subClass infClass vendorId deviceId subVendorId subDeviceId irq
                                                [bar0, bar1, bar2, bar3, bar4, bar5]
                   _ -> return $ Right $
                        PciDevice slot baseClass subClass infClass vendorId deviceId subVendorId subDeviceId irq []

probeDevice :: PciIO -> PciSlot -> [PciDevice] -> IO [PciDevice]
probeDevice pci slot a = go
    where go = do headerType <- pciRead8 pci slot pciHeaderTypeField
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
