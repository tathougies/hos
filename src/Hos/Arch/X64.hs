module Hos.Arch.X64 where

import Hos.Arch.Types
import Hos.Arch.X64.Types
import Hos.Arch.X64.Interrupt
import Hos.CBits
import Hos.Types
import Hos.Memory

import Control.Monad

import Data.Word
import Data.Char
import Data.IORef
import Data.Monoid
import Data.Bits

import Foreign.Ptr
import Foreign.Storable

import Numeric

import System.IO.Unsafe

type RealPtr = Word64
data X64PageTable = X64PageTable RealPtr
                  deriving (Show, Eq, Ord)
data X64PageTableIndex = X64PageTableIndex
                       { x64Pml4I :: !Int
                       , x64PdptI :: !Int
                       , x64PdtI  :: !Int
                       , x64PtI   :: !Int }
                       deriving (Show, Eq, Ord)

-- In Hos, our task abstraction is basically kernel level only
--
-- Each architecture provides the mechanisms necessary to enable
-- cooperative kernel-level context switching via forkIO.

x64VideoBuffer :: IORef (Ptr Word8)
x64VideoBuffer = unsafePerformIO $ newIORef (wordToPtr 0xffffff7f800b8000)

x64DebugLog:: String -> IO ()
x64DebugLog s = let go (!ptr) [] = writeIORef x64VideoBuffer ptr
                    go (!ptr) (!x:xs) = do poke ptr (fromIntegral (ord x))
                                           poke (ptr `plusPtr` 1) (0x0f :: Word8)
                                           go   (ptr `plusPtr` 2) xs
                in readIORef x64VideoBuffer >>= \buf -> go buf s

x64 :: Arch X64Registers X64PageTable X64Exception
x64 = Arch
    { archPageSize = x64PageSize
    , archStackDirection = StackGrowsDown

    , archNewVirtMemTbl = x64NewVirtMemTbl
    , archMapKernel = x64MapKernel
    , archMapPage = x64MapPage
    , archUnmapPage = x64UnmapPage

    , archGetCurVirtMemTbl = x64GetCurVirtMemTbl

    , archReadyForUserspace = x64ReadyForUserspace
    , archSwitchToUserspace = x64SwitchToUserspace

    , archDebugLog = x64DebugLog }

instance Registers X64Registers where
    registersForTask = x64TaskRegisters

x64KernelPDPTEntry :: Int
x64KernelPDPTEntry = 0x1ff

x64CurPt :: Int -> Int -> Int -> Ptr Word64
x64CurPt pml4I pdptI pdtI = wordToPtr
                          $ 0xffffff8000000000
                            + (fromIntegral pml4I * 0x40000000)
                            + (fromIntegral pdptI * 0x200000)
                            + (fromIntegral pdtI * 0x1000)

x64CurPdt :: Int -> Int -> Ptr Word64
x64CurPdt pml4I pdptI = x64CurPt 511 pml4I pdptI

x64CurPdpt :: Int -> Ptr Word64
x64CurPdpt pml4I = x64CurPdt 511 pml4I

x64CurPml4 :: Ptr Word64
x64CurPml4 = x64CurPdpt 511


x64TaskRegisters :: StackPtr -> InstructionPtr -> X64Registers
x64TaskRegisters (StackPtr stk) (InstructionPtr ip) =
    x64EmptyRegs { x64GPRegisters = gpRegisters' }
    where gpRegisters' = (x64GPRegisters x64EmptyRegs)
                       { x64GpRsp = stk
                       , x64GpRip = ip }

x64NewVirtMemTbl :: IO X64PageTable
x64NewVirtMemTbl = do
  newPageTbl <- cPageAlignedPhysAlloc (fromIntegral (archPageSize x64))
  withMapping x64 (Privileged ReadWrite) miscPage1 newPageTbl $ \mapping ->
    do memset (mapping :: Ptr Word64) 0 (fromIntegral (archPageSize x64))
       pokeElemOff mapping 511 (newPageTbl .|. 3)
  return (X64PageTable newPageTbl)

x64MapKernel :: X64PageTable -> IO ()
x64MapKernel (X64PageTable pgTbl) =
    do withMapping x64 (Privileged ReadWrite) miscPage1 pgTbl $ \mapping ->
           -- We map the kernel by copying over the current pdpt mapping
         do pdpt <- peekElemOff x64CurPml4 x64KernelPDPTEntry
            pokeElemOff mapping x64KernelPDPTEntry pdpt

x64PTIndex :: Word64 -> X64PageTableIndex
x64PTIndex addr =
    let ptI = fromIntegral $ (addr `shiftR` 12) .&. 0x1ff
        pdtI = fromIntegral $ (addr `shiftR` 21) .&. 0x1ff
        pdptI = fromIntegral $ (addr `shiftR` 30) .&. 0x1ff
        pml4I = fromIntegral $ (addr `shiftR` 39) .&. 0x1ff
    in X64PageTableIndex pml4I pdptI pdtI ptI

x64PageEntry :: Word64 -> MemoryPermissions -> Word64
x64PageEntry a perms = (a .|. 3)

x64MapPage :: Word64 -> Word64 -> MemoryPermissions -> IO ()
x64MapPage virt phys perms =
    do let X64PageTableIndex pml4I pdptI pdtI ptI = x64PTIndex virt
           entry = x64PageEntry phys perms

           pdptPtr = x64CurPdpt pml4I
           pdtPtr = x64CurPdt pml4I pdptI
           ptPtr = x64CurPt pml4I pdptI pdtI

           ensurePageTable pdAddr index pAddr = do
             pEntry <- peekElemOff pdAddr index
             when (pEntry == 0) $ do
               newPd <- cPageAlignedAlloc (fromIntegral (archPageSize x64))
               pokeElemOff pdAddr index (newPd .|. 0x3)
               memset pAddr 0 (fromIntegral (archPageSize x64))
       ensurePageTable x64CurPml4 pml4I pdptPtr
       ensurePageTable pdptPtr pdptI pdtPtr
       ensurePageTable pdtPtr pdtI ptPtr
       x64DebugLog ("PT at " ++ show ptPtr)
       pokeElemOff ptPtr ptI entry
       archInvalidatePage virt

x64UnmapPage :: Word64 -> IO ()
x64UnmapPage virt =
    do let X64PageTableIndex pml4I pdptI pdtI ptI = x64PTIndex virt

           pdptPtr = x64CurPdpt pml4I
           pdtPtr = x64CurPdt pml4I pdptI
           ptPtr = x64CurPt pml4I pdptI pdtI

       pmlEntry <- peekElemOff x64CurPml4 pml4I
       when (pmlEntry /= 0) $ do
         pdptEntry <- peekElemOff pdptPtr pdptI
         when (pdptEntry /= 0) $ do
           pdtEntry <- peekElemOff pdtPtr pdtI
           when (pdtEntry /= 0) $ do
             pokeElemOff ptPtr ptI 0
             archInvalidatePage virt

x64GetCurVirtMemTbl :: IO X64PageTable
x64GetCurVirtMemTbl = do pgTblAddr <- peekElemOff x64CurPml4 0x1ff
                         let pgTblAddrAligned = pgTblAddr .&. (complement (fromIntegral (archPageSize x64) - 1))
                         return (X64PageTable pgTblAddrAligned)

foreign import ccall "arch.h &tssArea" x64TssArea :: Ptr Word64
foreign import ccall "arch.h setupSysCalls" x64SetupSysCalls :: IO ()
x64ReadyForUserspace :: IO ()
x64ReadyForUserspace =
    -- Call the assembler code to enable the syscall/sysret function
    do x64SetupSysCalls

       -- Next set RSP0 of our task segment to use our temporary stack
       pokeElemOff (x64TssArea `plusPtr` 4) 0 (ptrToWord x64TempKernelStackTop)

       -- Then set IST1 to use the temporary stack as well. Routines that enter the kernel will automatically set and unset IST1 to use the emergency stack
       pokeElemOff (x64TssArea `plusPtr` 4) 4 (ptrToWord x64TempKernelStackTop)

       -- Now set up fault handling interrupts
       idtPtr <- x64NewIDT
       x64LoadIdt idtPtr

foreign import ccall "arch.h &curUserSpaceState" x64TaskStateTmp :: Ptr ()
foreign import ccall "arch.h &kernelState" x64KernelState :: Ptr ()
foreign import ccall "arch.h x64SwitchToUserspace" x64SwitchToUserspaceAsm :: Ptr () -> Ptr () -> IO Int
x64SwitchToUserspace :: Task X64Registers X64PageTable -> IO (InterruptReason X64Exception)
x64SwitchToUserspace t = do x64PokeTaskState t x64TaskStateTmp
                            reason <- x64SwitchToUserspaceAsm (castPtr x64TaskStateTmp) (castPtr x64KernelState)
                            case reason of
                              256 -> return (SysCallInterrupt 0)
                              _ | reason < 256 -> return (TrapInterrupt (x64MapFault reason))
                                | otherwise    -> return (DeviceInterrupt 0)

x64MapFault :: Int -> CPUException X64Exception
x64MapFault 0 = DivideByZero
x64MapFault 1 = DebugTrap
x64MapFault 2 = ArchException X64NonMaskableInterrupt
x64MapFault 3 = BreakpointTrap
x64MapFault 4 = Overflow
x64MapFault 5 = BoundsCheckFailed
x64MapFault 6 = InvalidOpcode
x64MapFault 7 = ArchException X64DeviceNotAvailable
x64MapFault 8 = ArchException X64DoubleFault
x64MapFault 9 = ArchException X64LegacyCoprocessorOverrun
x64MapFault 10 = ArchException X64InvalidTSS
x64MapFault 11 = ArchException X64SegmentNotPresent
x64MapFault 12 = ArchException X64StackSegmentFault
x64MapFault 13 = ProtectionException
x64MapFault 14 = VirtualMemoryFault
x64MapFault 15 = UnknownException
x64MapFault 16 = FloatingPointException
x64MapFault 17 = AlignmentCheck
x64MapFault 18 = MachineCheck
x64MapFault 19 = SIMDException
x64MapFault 20 = VirtualMachineException
x64MapFault 30 = ArchException X64SecurityException
x64MapFault _ = UnknownException

-- Switches tasks. If the function returns, it will be because another task
-- switched to this one. In that case, the return value is the task that
-- switched to us
