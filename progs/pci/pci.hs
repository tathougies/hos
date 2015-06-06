module Main where
import Hos.User.SysCall

import Hos.Dev.Pci.Arch
import Hos.Dev.Pci.Types
import Hos.Dev.Pci.Probe

import Control.Monad

import Numeric

pciDevName :: PciDevice -> String
pciDevName pciD =
    case (pciDevBaseClass pciD, pciDevSubClass pciD, pciDevInfClass pciD) of
      (0, 1, 0) -> "VGA-compatible device"
      (0, sub, inf) -> "Prehistoric - " ++ showHex sub (", " ++ showHex inf "")

      (1, 1, _) -> "IDE controller"
      (1, 2, 0) -> "Floppy disk controller"
      (1, 5, _) -> "ATA controller"
      (1, 6, 0) -> "SATA controller"
      (1, sub, inf) -> "Other storage controller - " ++ showHex sub (", " ++ showHex inf "")

      (2, 0, 0) -> "Ethernet controller"
      (2, sub, inf) -> "Other network controller - " ++ showHex sub (", " ++ showHex inf "")

      (3, 0, 0) -> "VGA controller"
      (3, 2, 0) -> "3D controller"
      (3, sub, inf) -> "Other display controller - " ++ showHex sub (", " ++ showHex inf "")

      (4, sub, inf) -> "Multimedia device -"  ++ showHex sub (", " ++ showHex inf "")

      (5, sub, inf) -> "Memory controller -"  ++ showHex sub (", " ++ showHex inf "")

      (6, sub, inf) -> "Bridge device - " ++ showHex sub (", " ++ showHex inf "")

      (7, 0, inf) -> "Serial controller(" ++ showHex inf ")"
      (7, sub, inf) -> "Other communications device - " ++ showHex sub (", " ++ showHex inf "")

      (8, sub, inf) -> "System Peripheral - " ++ showHex sub (", " ++ showHex inf "")

      (9, 0, 0) -> "Keyboard controller"
      (9, 2, 0) -> "Mouse controller"
      (9, sub, inf) -> "Input controller - " ++ showHex sub (", " ++ showHex inf "")

      (0xC, 0, _) -> "FireWire controller"
      (0xC, 3, _) -> "USB controller"
      (0xC, _, _) -> "Other bus controller"

      (0xD, _, _) -> "Other wireless controller"

      (base, sub, inf) -> "Other - " ++ showHex base (", " ++ showHex sub (", " ++ showHex inf ""))

main = do hosDebugLog "[pci] probing devices..."
          hosRequestIO
          devs <- probeMainBus archPciIO
          forM_ devs $ \dev ->
              hosDebugLog ("[pci] Found: Vendor Id = " ++ showHex (pciDevVid dev) (", Device id = " ++ showHex (pciDevDid dev) (", " ++ pciDevName dev)))
