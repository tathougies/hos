module Main where

import Hos.User.SysCall
import Hos.User.IPC
import Hos.User.IO
import Hos.Common.Types

import Hos.Init.Msg

import Control.Applicative
import Control.Monad
import Control.Exception

import Data.List
import Data.Word
import Data.Bits
import Data.Maybe

import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

import Numeric

data AtaController = AtaController !Word16 !Word16 !Word16
                     deriving Show

data DriveSelect = Slave | Master deriving Show
data AtaReadWrite = Read | Write deriving Show
data AtaCommand = AtaCommand
                { cmdDrive :: DriveSelect
                , cmdCommand :: Word8
                , cmdPrecomp :: Word8
                , cmdSecCnt  :: Word8
                , cmdSecNum  :: Word8
                , cmdCylLo   :: Word8
                , cmdCylHi   :: Word8 }
                  deriving Show

data AtaCapabilities = AtaCapabilities
                     { ataLbaSupport :: !Bool
                     , ataDmaSupport :: !Bool
                     , ataUdmaSupport :: !Bool }
                       deriving Show

data AtaDevice = AtaDevice
               { ataController :: !AtaController
               , ataDrive      :: !DriveSelect
               , ataSectors    :: !Word64
               , ataIrq        :: !Word8
               , ataCapabilities :: !AtaCapabilities }
               | AtapiDevice
               { ataController :: !AtaController
               , ataDrive      :: !DriveSelect
               , ataIrq        :: !Word8
               , ataCapabilities :: !AtaCapabilities }
                 deriving Show

pciCtlOff :: Word16
pciCtlOff = 2

ataIdentifyCmd, atapiIdentifyCmd :: Word8
ataIdentifyCmd = 0xEC
atapiIdentifyCmd = 0xA1
ataPacketCmd = 0xA0

main :: IO ()
main = do env <- hosReadEnvironment
          hosDebugLog ("[ata] starting ")
          forM_ env $ \v -> hosDebugLog ("[ata] " ++ show v)
          case lookup "com.hos.driver.type" env of
            Just "com.hos.dev.pci" -> pciStart env
            Nothing -> return ()

readBar :: String -> Maybe (Word16, Word16)
readBar ('i':':':spec) =
    let (addr, sz) = break (==':') spec
    in case (reads addr, reads (tail sz)) of
         ([(addr, "")], [(sz, "")]) ->
             Just (addr, sz)
         _ -> Nothing
readBar _ = Nothing

pciStart :: [(String, String)] -> IO ()
pciStart env =
    do let devName = case lookup "com.hos.dev.pci.device" env of
                       Just "8086:7111" -> "VMWare IDE Device"
                       _ -> "Other"
           busInd = read <$> lookup "com.hos.dev.pci.busInd" env :: Maybe Word8
           devInd = read <$> lookup "com.hos.dev.pci.devInd" env :: Maybe Word8
           funcInd = read <$> lookup "com.hos.dev.pci.funcInd" env :: Maybe Word8
           progIf = fromJust (read <$> lookup "com.hos.dev.pci.progIf" env) :: Word8
           irq = fromJust (read <$> lookup "com.hos.dev.pci.irq" env) :: Word8
           (dmaBase, dmaSz) = fromJust (lookup "com.hos.dev.pci.bar.4" env >>= readBar)
       case lookup "com.hos.dev.pci.device" env of
         Just x -> do initResponse <- initRegisterProvider ("hos.dev.ata.pci." ++ x)
                      case initResponse of
                        Just InitSuccess -> hosDebugLog "[ata] Registered..."
                        _ -> hosDebugLog ("[ata] could not register...")
         _ -> return ()
       hosDebugLog ("[ata] We have a " ++ devName ++ " at " ++ show (busInd, devInd, funcInd))
       hosDebugLog ("[ata] The device uses DMA I/O ports " ++ showHex dmaBase (" - " ++ showHex (dmaBase + dmaSz) (" and IRQ " ++ show irq)))
       hosRequestIO
       let (ataController0, irq0) = if progIf `testBit` 0 then error "ATA primary in native mode"
                                        else (AtaController 0x1f0 0x3f6 dmaBase, 14)
           (ataController1, irq1) = if progIf `testBit` 2 then error "ATA secondary in native mode"
                                        else (AtaController 0x170 0x376 (dmaBase + 8), 15)
       hosDebugLog ("[ata] Identify primary...")
       priMaster <- ataIdentify ataController0 Master
       priSlave <- ataIdentify ataController0 Slave
       hosDebugLog ("[ata] Identify secondary...")
       secMaster <- ataIdentify ataController1 Master
       secSlave <- ataIdentify ataController1 Slave

       let primaryDrives = catMaybes [priMaster, priSlave]
           secondaryDrives = catMaybes [secMaster, secSlave]
           drives = map (\a -> a { ataIrq = irq0 }) primaryDrives ++ map (\a -> a { ataIrq = irq1 }) secondaryDrives
       forM_ drives $ \drive -> hosDebugLog ("[ata] got drive " ++ show drive)

       hosDebugLog ("[ata] Now we're going to allocate some DMA space for us...")
       hosAddMappingToCurTask 0x18000000000 0x18000009000 (AllocateOnDemand (UserSpace ReadWrite))
       (prdtPhys:dmaAddrs) <- mapM hosPhysAddrFor [0x18000000000,0x18000001000..0x18000008000]
       setupPrdt (wordToPtr 0x18000000000) dmaAddrs
       readBlock prdtPhys (drives !! 1) 0x8000
--       hosOut8 (ioBase + pciCtlOff) (ataIdentify)
--          pciReadBAR
       return ()

withDma :: AtaController -> Word64 -> AtaReadWrite -> IO a -> IO a
withDma (AtaController _ _ dmaBase) prdt rw f =
    do hosOut32 (dmaBase + 4) (fromIntegral (prdt .&. 0xFFFFFFFF))
       let cmd = 0x1 .|. case rw of
                           Read -> 0x8
                           Write -> 0x0
       hosOut8 dmaBase cmd
       hosOut8 (dmaBase + 2) 6 -- clear int and error
       x <- f
       hosOut8 dmaBase 0x0 -- Disable dma after the operation...
       return x

setupPrdt :: Ptr Word32 -> [Word64] -> IO ()
setupPrdt prdtPtr dmaPages =
    forM_ (zip [0..] dmaPages) $ \(i, dmaPageAddr) ->
        do pokeElemOff prdtPtr (i * 2) (fromIntegral (dmaPageAddr .&. 0xFFFFFFFF))
           pokeElemOff prdtPtr (i * 2 + 1) (0x1000 .|. if i == (length dmaPages - 1) then bit 31 else 0)

readBlock :: Word64 -> AtaDevice -> Word64 -> IO ()
readBlock prdtPhys (AtapiDevice ctlr@(AtaController cmdBase ctlBase dmaBase) drv irq caps) blkno =
    do -- issue read packet command
       ataSimpleCommand ctlr (AtaCommand drv ataPacketCmd 0x1 0 0 0 0)
       hosDebugLog ("[ata] PACKET issued...")
       sts <- ataWaitForStatus ctlr notBusy
       hosDebugLog ("[ata] waiting for packet...")
       if not (drqReady sts)
          then hosDebugLog ("[ata] Error with packet command")
          else do hosOut16 (ataDataReg cmdBase)     0x28
                  hosOut16 (ataDataReg cmdBase)     0x0
                  hosOut16 (ataDataReg cmdBase)     0x1000
                  hosOut16 (ataDataReg cmdBase)     0x0
                  hosOut16 (ataDataReg cmdBase)     0x10
                  hosOut16 (ataDataReg cmdBase)    0x0
                  hosOut8 0xA1 0x7f
                  withDma ctlr prdtPhys Read $
                          forever $ return ()

ataStatusReg = (+ 7)
ataStatusBusy = 7

ataPrecompReg = (+ 1)
ataSecCntReg = (+ 2)
ataSecNumReg = (+ 3)
ataCylLoReg = (+ 4)
ataCylHiReg = (+ 5)
ataDriveSelectReg = (+ 6)
ataCmdReg = (+ 7)

ataDataReg = id

drqReady sts = sts `testBit` 3
statusError sts = sts `testBit` 0
statusWriteFault sts = sts `testBit` 5
notBusy sts = not (sts `testBit` ataStatusBusy)

ataWaitForStatus :: AtaController -> (Word8 -> Bool) -> IO Word8
ataWaitForStatus ctlr@(AtaController cmdBase ctlBase _) pred =
    do sts <- hosIn8 (ataStatusReg cmdBase)
       if pred sts then return sts else ataWaitForStatus ctlr pred

ataSimpleCommand :: AtaController -> AtaCommand -> IO ()
ataSimpleCommand ctlr@(AtaController cmdBase ctlBase _) (AtaCommand drv cmd precomp secCount secNum cylLo cylHi) =
    do ataWaitForStatus ctlr notBusy
       hosOut8 (ataDriveSelectReg cmdBase) (case drv of
                                              Master -> 0xA0
                                              _ -> 0xB0)
       ataWaitForStatus ctlr notBusy
       hosOut8 (ataPrecompReg cmdBase) precomp
       hosOut8 (ataSecCntReg cmdBase) secCount
       hosOut8 (ataSecNumReg cmdBase) secNum
       hosOut8 (ataCylLoReg cmdBase) cylLo
       hosOut8 (ataCylHiReg cmdBase) cylHi
       hosOut8 (ataCmdReg cmdBase) cmd

ataIdentify :: AtaController -> DriveSelect -> IO (Maybe AtaDevice)
ataIdentify ctlr@(AtaController cmdBase ctlBase _) drv =
    do hosDebugLog ("[ata] Attempting to identify " ++ show drv)
       ataSimpleCommand ctlr (AtaCommand drv ataIdentifyCmd 0 0 0 0 0)
       sts <- ataWaitForStatus ctlr (\sts -> notBusy sts || sts == 0)
       if sts == 0 || statusError sts || statusWriteFault sts
          then atapiIdentify ctlr drv
          else bracket (mallocBytes 512) (free) $ \tmpBuf ->
               do ataWaitForStatus ctlr drqReady
                  forM_ [0..255] $ \i -> hosIn16 (ataDataReg cmdBase) >>= pokeElemOff tmpBuf i
                  hosDebugLog ("[ata]   found ATA drive!")
                  caps <- idRead tmpBuf
                  sz <- idCheckSize tmpBuf
                  return (Just (AtaDevice ctlr drv sz 0 caps))

atapiIdentify :: AtaController -> DriveSelect -> IO (Maybe AtaDevice)
atapiIdentify ctlr@(AtaController cmdBase ctlBase _) drv =
    do ataSimpleCommand ctlr (AtaCommand drv atapiIdentifyCmd 0 0 0 0 0)
       sts <- ataWaitForStatus ctlr (\sts -> notBusy sts || sts == 0)
       if sts == 0 || statusError sts || statusWriteFault sts
         then hosDebugLog ("[ata]   not found") >> return Nothing
         else bracket (mallocBytes 512) (free) $ \tmpBuf ->
              do ataWaitForStatus ctlr drqReady
                 forM_ [0..255] $ \i -> hosIn16 (ataDataReg cmdBase) >>= pokeElemOff tmpBuf i
                 hosDebugLog ("[ata]   found ATAPI drive!")
                 caps <- idRead tmpBuf
                 return (Just (AtapiDevice ctlr drv 0 caps))

-- IDENTIFY fields
idCapabilities = 0x31
idFieldValidity = 0x35
idMultiwordDma = 0x3f
idUltraDma = 0x58

idRead :: Ptr Word16 -> IO AtaCapabilities
idRead p =
  do caps <- peekElemOff p idCapabilities
     let lbaSupport = caps `testBit` 9
         dmaSupport = caps `testBit` 8
     udmaSupport <- if dmaSupport
                      then do fieldValidity <- peekElemOff p idFieldValidity
                              let hasUdmaField = caps `testBit` 2
                              if hasUdmaField
                                then do udma <- peekElemOff p idUltraDma
                                        return (udma .&. 0x1f > 0)
                                else return False
                      else return False
     return (AtaCapabilities lbaSupport dmaSupport udmaSupport)

idCheckSize :: Ptr Word16 -> IO Word64
idCheckSize p =
    do cyl <- peekElemOff p 1
       heads <- peekElemOff p 3
       sectors <- peekElemOff p 6
       caps <- peekElemOff p idCapabilities
       let size = fromIntegral cyl * fromIntegral heads * fromIntegral sectors :: Word64
       if size > (1024 * 1024) && caps `testBit` 9
          then fromIntegral <$> peekElemOff (castPtr p :: Ptr Word32) 30
          else return (fromIntegral size)
