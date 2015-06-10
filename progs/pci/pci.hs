module Main where

import Hos.User.SysCall
import Hos.User.IPC
import Hos.Init.Msg
import Hos.Storage.Msg
import Hos.Common.Types
import Hos.Common.Bundle

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

strDevId dev = let vendorIdS = showHex (pciDevVid dev) ""
                   deviceIdS = showHex (pciDevDid dev) ""
                   vendorIdS' = replicate (4 - length vendorIdS) '0' ++ vendorIdS
                   deviceIdS' = replicate (4 - length deviceIdS) '0' ++ deviceIdS
               in vendorIdS' ++ ":" ++ deviceIdS'

pciBarEnv :: PciDevice -> [(String, String)]
pciBarEnv dev = concat $ zipWith barEnv [0..] (pciDevBars dev)
    where barEnv i (PciBarMemory base sz)
              | base /= 0 || sz /= 0 = [("com.hos.dev.pci.bar." ++ show i, "m:" ++ show base ++ ":" ++ show sz)]
          barEnv i (PciBarIO base sz)
              | base /= 0 || sz /= 0 = [("com.hos.dev.pci.bar." ++ show i, "i:" ++ show base ++ ":" ++ show sz)]
          barEnv _ _ = []

main = do hosDebugLog "[pci] probing devices..."
          initResponse <- transmitMsg (ChanId 0) (ServerName "hos.init") (ChanId 0) (InitRegisterProvider "hos.dev.pci")
--          forever $ return ()
          case initResponse of
            Right InitSuccess -> hosDebugLog "[pci] successfully registered!"
            Right resp -> hosDebugLog ("[pci] init gave error: " ++ show resp)
            Left (CouldNotParseResponse err) -> hosDebugLog ("[pci] could not decode response: "++ err)
          storageProvider <- initFindProvider (ServerName "hos.storage")
          hosDebugLog ("Found provider " ++ show storageProvider)
          hosRequestIO
          devs <- probeMainBus archPciIO
          forM_ devs $ \dev ->
              do hosDebugLog ("[pci] Found: Vendor Id = " ++ showHex (pciDevVid dev) (", Device id = " ++ showHex (pciDevDid dev) (", " ++ pciDevName dev)))
                 withStorageTransaction $
                   do driver <- storageQuery (TagIs (TagName "com.hos.dev.pci.supported-device") (TextV (strDevId dev)))
                      case driver of
                        Nothing -> return ()
                        Just obId -> do taskId <- storageExecute obId ([ ("com.hos.driver.type", "com.hos.dev.pci")
                                                                       , ("com.hos.dev.pci.device", strDevId dev)
                                                                       , ("com.hos.dev.pci.progIf", show (pciDevInfClass dev))
                                                                       , ("com.hos.dev.pci.busInd", show (pciSlotBusInd (pciDevSlot dev)))
                                                                       , ("com.hos.dev.pci.devInd", show (pciSlotDevInd (pciDevSlot dev)))
                                                                       , ("com.hos.dev.pci.funcInd", show (pciSlotFuncInd (pciDevSlot dev))) ] ++
                                                                       pciBarEnv dev)
                                        case taskId of
                                          Just _ -> return ()
                                          Nothing -> hosDebugLog ("[pci] couldn't start driver...")
