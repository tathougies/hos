module Main where

import Hos.User.SysCall
import Hos.User.IPC
import Hos.User.IO

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

data AtaController = AtaController !Word16 !Word16
                     deriving Show

data DriveSelect = Slave | Master deriving Show
data AtaCommand = AtaCommand
                { cmdDrive :: DriveSelect
                , cmdCommand :: Word8
                , cmdPrecomp :: Word8
                , cmdSecCnt  :: Word8
                , cmdSecNum  :: Word8
                , cmdCylLo   :: Word8
                , cmdCylHi   :: Word8 }
                  deriving Show

pciCtlOff :: Word16
pciCtlOff = 2

ataIdentifyCmd, atapiIdentifyCmd :: Word8
ataIdentifyCmd = 0xEC
atapiIdentifyCmd = 0xA1

main :: IO ()
main = do env <- hosReadEnvironment
          hosDebugLog ("[ata] starting")
          case lookup "com.hos.driver.type" env of
            Just "com.hos.dev.pci" -> pciStart env
            Nothing -> return ()

readPciBars :: [(String, String)] -> (Word16, Word16)
readPciBars env = case find (isBar . fst) env of
                    Nothing -> error "No BARS found"
                    Just (_, barSpec) -> readBarSpec barSpec
    where isBar = isPrefixOf "com.hos.dev.pci.bar"

          readBarSpec :: String -> (Word16, Word16)
          readBarSpec ('i':':':spec) =
              let (addr, sz) = break (==':') spec
              in case (reads addr, reads (tail sz)) of
                   ([(addr, "")], [(sz, "")]) ->
                       (addr, sz)
                   _ -> error ("Could not parse addr:sz - " ++ show spec)
          readBarSpec _ = error "Expected IO spec"

pciStart :: [(String, String)] -> IO ()
pciStart env =
    do let devName = case lookup "com.hos.dev.pci.device" env of
                       Just "8086:7111" -> "VMWare IDE Device"
                       _ -> "Other"
           busInd = read <$> lookup "com.hos.dev.pci.busInd" env :: Maybe Word8
           devInd = read <$> lookup "com.hos.dev.pci.devInd" env :: Maybe Word8
           funcInd = read <$> lookup "com.hos.dev.pci.funcInd" env :: Maybe Word8
           progIf = fromJust (read <$> lookup "com.hos.dev.pci.progIf" env) :: Word8
           (ioBase, ioSz) = readPciBars env
       hosDebugLog ("[ata] We have a " ++ devName ++ " at " ++ show (busInd, devInd, funcInd))
       hosDebugLog ("[ata] The device uses IO ports " ++ showHex ioBase (" - " ++ showHex (ioBase + ioSz) ""))
       hosDebugLog ("[ata] uses prog if " ++ showHex progIf "")
       hosRequestIO
       let ataController0 = if progIf `testBit` 0 then error "ATA primary in native mode"
                                       else AtaController 0x1f0 0x3f6
           ataController1 = if progIf `testBit` 2 then error "ATA secondary in native mode"
                                       else AtaController 0x170 0x376
       hosDebugLog ("[ata] Identify primary...")
       ataIdentify ataController0 Master
       ataIdentify ataController0 Slave
       hosDebugLog ("[ata] Identify secondary...")
       ataIdentify ataController1 Master
       ataIdentify ataController1 Slave
--       hosOut8 (ioBase + pciCtlOff) (ataIdentify)
--          pciReadBAR
       return ()

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
ataWaitForStatus ctlr@(AtaController cmdBase ctlBase) pred =
    do sts <- hosIn8 (ataStatusReg cmdBase)
       if pred sts then return sts else ataWaitForStatus ctlr pred

ataSimpleCommand :: AtaController -> AtaCommand -> IO ()
ataSimpleCommand ctlr@(AtaController cmdBase ctlBase) (AtaCommand drv cmd precomp secCount secNum cylLo cylHi) =
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

ataIdentify :: AtaController -> DriveSelect -> IO ()
ataIdentify ctlr@(AtaController cmdBase ctlBase) drv =
    do hosDebugLog ("[ata] Attempting to identify " ++ show drv)
       ataSimpleCommand ctlr (AtaCommand drv ataIdentifyCmd 0 0 0 0 0)
       sts <- ataWaitForStatus ctlr (\sts -> notBusy sts || sts == 0)
       if sts == 0 || statusError sts || statusWriteFault sts
          then atapiIdentify ctlr drv
          else bracket (mallocBytes 512) (free) $ \tmpBuf ->
               do ataWaitForStatus ctlr drqReady
                  forM_ [0..255] $ \i -> hosIn16 (ataDataReg cmdBase) >>= pokeElemOff tmpBuf i
                  hosDebugLog ("[ata]   found ATA drive!")
                  idRead tmpBuf
                  idCheckSize tmpBuf

atapiIdentify :: AtaController -> DriveSelect -> IO ()
atapiIdentify ctlr@(AtaController cmdBase ctlBase) drv =
    do ataSimpleCommand ctlr (AtaCommand drv atapiIdentifyCmd 0 0 0 0 0)
       sts <- ataWaitForStatus ctlr (\sts -> notBusy sts || sts == 0)
       if sts == 0 || statusError sts || statusWriteFault sts
         then hosDebugLog ("[ata]   not found")
         else bracket (mallocBytes 512) (free) $ \tmpBuf ->
              do ataWaitForStatus ctlr drqReady
                 forM_ [0..255] $ \i -> hosIn16 (ataDataReg cmdBase) >>= pokeElemOff tmpBuf i
                 hosDebugLog ("[ata]   found ATAPI drive!")
                 idRead tmpBuf

-- IDENTIFY fields
idCapabilities = 0x31
idFieldValidity = 0x35
idMultiwordDma = 0x3f
idUltraDma = 0x58

idRead :: Ptr Word16 -> IO ()
idRead p =
  do caps <- peekElemOff p idCapabilities
     if caps `testBit` 9 then hosDebugLog ("[ata]   LBA supported") else return ()
     if caps `testBit` 8
       then do hosDebugLog ("[ata]   DMA supported")

               fieldValidity <- peekElemOff p idFieldValidity
               let hasUdmaField = caps `testBit` 2

               multiwordDma <- peekElemOff p idMultiwordDma
               if multiwordDma .&. 0x7 > 0 then hosDebugLog ("[ata] Supports multword dma modes: " ++ show (filter (multiwordDma `testBit`) [0, 1, 2])) else return ()
               if hasUdmaField
                  then do udma <- peekElemOff p idUltraDma
                          if udma .&. 0x1f > 0 then hosDebugLog ("[ata] Supports ultra dma modes: " ++ show (filter (multiwordDma `testBit`) [0, 1, 2, 3, 4])) else return ()
                  else return ()
       else return ()

idCheckSize :: Ptr Word16 -> IO ()
idCheckSize p =
    do cyl <- peekElemOff p 1
       heads <- peekElemOff p 3
       sectors <- peekElemOff p 6
       caps <- peekElemOff p idCapabilities
       let size = fromIntegral cyl * fromIntegral heads * fromIntegral sectors :: Word64
       if size > (1024 * 1024) && caps `testBit` 9
          then do sz <- peekElemOff (castPtr p :: Ptr Word32) 30
                  hosDebugLog ("[ata]   Size is " ++ showHex sz "(LBA 48)")
          else hosDebugLog ("[ata]   Size is " ++ showHex size "")
