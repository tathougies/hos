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
import qualified Data.Map.Base as M

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

type X64HosState = HosState X64Registers X64PageTable X64Exception

x64VideoBuffer :: IORef (Ptr Word8)
x64VideoBuffer = unsafePerformIO $ newIORef (wordToPtr 0xffffff7f800b8000)

foreign import "write_serial" writeSerial :: Word8 -> IO ()
x64DebugLog:: String -> IO ()
x64DebugLog s = let go {- ptr -} [] = writeSerial (fromIntegral (ord '\n')) -- 
                    go {- ptr -} (!x:xs) = do writeSerial (fromIntegral (ord x))
                                              --poke ptr (fromIntegral (ord x))
                                              --poke (ptr `plusPtr` 1) (0x0f :: Word8)
                                              go   {- (ptr `plusPtr` 2) -} xs
                in {- readIORef x64VideoBuffer >>= \buf -> -} go {- buf -} s

foreign import ccall "arch.h arch_unmap_init_task" x64UnmapInitTask :: IO ()
foreign import ccall "arch.h &g_module_count" x64ModuleCount :: Ptr Int
x64 :: Arch X64Registers X64PageTable X64Exception
x64 = Arch
    { archPageSize = x64PageSize
    , archStackDirection = StackGrowsDown
    , archInitProcessPhysBase = 0
    , archUnmapInitTask = x64UnmapInitTask

    , archNewVirtMemTbl = x64NewVirtMemTbl
    , archMapKernel = x64MapKernel
    , archMapPage = x64MapPage
    , archUnmapPage = x64UnmapPage
    , archTestPage = x64TestPage
    , archCopyPhysPage = x64CopyPhysPage

    , archGetCurVirtMemTbl = x64GetCurVirtMemTbl

    , archHandleException = x64HandleException

    , archReadyForUserspace = x64ReadyForUserspace
    , archSwitchTasks = x64SwitchTasks
    , archSwitchToUserspace = x64SwitchToUserspace
    , archReturnToUserspace = x64ReturnToUserspace
    , archUserPanic = x64UserPanic

    , archBootModuleCount = peek x64ModuleCount >>= return . fromIntegral
    , archGetBootModule = x64GetBootModule

    , archDebugLog = x64DebugLog }

instance Registers X64Registers where
    registersForTask = x64TaskRegisters

x64KernelPDPTEntry :: Int
x64KernelPDPTEntry = 0x1fe

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
x64PageEntry a (Privileged ReadWrite) = (a .|. 3)
x64PageEntry a (Privileged ReadOnly) = (a .|. 1)
x64PageEntry a (UserSpace ReadWrite) = (a .|. 7)
x64PageEntry a (UserSpace ReadOnly) = (a .|. 5)

x64PageEntryPermissions :: Word64 -> MemoryPermissions
x64PageEntryPermissions x = case x .&. 7 of
                              1 -> Privileged ReadOnly
                              3 -> Privileged ReadWrite
                              5 -> UserSpace ReadOnly
                              7 -> UserSpace ReadWrite
                              _ -> Privileged ReadOnly

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
               newPd <- cPageAlignedPhysAlloc (fromIntegral (archPageSize x64))
               pokeElemOff pdAddr index (x64PageEntry newPd perms)
               archInvalidatePage (ptrToWord pAddr)
               memset pAddr 0 (fromIntegral (archPageSize x64))
             pEntry <- peekElemOff pdAddr index
             -- Now we want to ensure that the permissions are weaker than the permissions we were given
             let newPermissions = case (oldPermissions, perms) of
                                    (Privileged cur, Privileged new) -> Privileged (newPermsRW cur new)
                                    (Privileged cur, UserSpace new) -> UserSpace (newPermsRW cur new)
                                    (UserSpace cur, Privileged new) -> UserSpace (newPermsRW cur new)
                                    (UserSpace cur, UserSpace new) -> UserSpace (newPermsRW cur new)
                 newPermsRW ReadOnly x = x
                 newPermsRW ReadWrite _ = ReadWrite

                 oldPermissions = x64PageEntryPermissions pEntry

                 newEntry = x64PageEntry (pEntry .&. (complement (fromIntegral (archPageSize x64) - 1))) newPermissions

             when (newPermissions /= oldPermissions) $ do
                  pokeElemOff pdAddr index newEntry
                  archInvalidatePage (ptrToWord pAddr)
       ensurePageTable x64CurPml4 pml4I pdptPtr
       ensurePageTable pdptPtr pdptI pdtPtr
       ensurePageTable pdtPtr pdtI ptPtr

       pokeElemOff ptPtr ptI entry
       archInvalidatePage virt

x64GetPhysPageEntry :: Word64 -> IO Word64
x64GetPhysPageEntry virt =
    do let X64PageTableIndex pml4I pdptI pdtI ptI = x64PTIndex virt

           pdptPtr = x64CurPdpt pml4I
           pdtPtr = x64CurPdt pml4I pdptI
           ptPtr = x64CurPt pml4I pdptI pdtI

       pmlEntry <- peekElemOff x64CurPml4 pml4I
       if pmlEntry /= 0
         then do
           pdptEntry <- peekElemOff pdptPtr pdptI
           if pdptEntry /= 0
             then do
               pdtEntry <- peekElemOff pdtPtr pdtI
               if pdtEntry /= 0
                 then peekElemOff ptPtr ptI
                 else return 0
             else return 0
         else return 0

x64GetPhysPage :: Word64 -> IO Word64
x64GetPhysPage virt = do ptr <- x64GetPhysPageEntry virt
                         return (ptr .&. 0xfffffffffffff000)

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

x64TestPage :: Word64 -> MemoryPermissions -> IO Bool
x64TestPage vAddr reqPerms =
    do pEntry <- x64GetPhysPageEntry vAddr
       case pEntry .&. 0x1 of
         0x1 -> let curPerms = x64PageEntryPermissions pEntry
                in case (curPerms, reqPerms) of
                     (UserSpace ReadWrite, _) -> return True
                     (UserSpace ReadOnly, UserSpace ReadOnly) -> return True
                     (UserSpace ReadOnly, Privileged ReadOnly) -> return True
                     (UserSpace ReadOnly, UserSpace ReadWrite) -> return False
                     (UserSpace ReadOnly, Privileged ReadWrite) -> return False
                     (Privileged _, UserSpace _) -> return False
                     (Privileged ReadOnly, Privileged ReadWrite) -> return False
                     _ -> return True
         _ -> return False

x64CopyPhysPage :: Word64 -> Word64 -> IO ()
x64CopyPhysPage src dest =
    withMapping x64 (Privileged ReadOnly) miscPage1 src $ \srcP ->
    withMapping x64 (Privileged ReadWrite) miscPage2 dest $ \destP ->
    memcpy destP srcP (fromIntegral x64PageSize)

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
       x64DebugLog ("Our kernel stack top is " ++ showHex (ptrToWord x64TempKernelStackTop) "")
       -- Next set RSP0 of our task segment to use our temporary stack
       poke (x64TssArea `plusPtr` 4) (ptrToWord x64TempKernelStackTop)

       -- Then set IST1 to use the temporary stack as well. Routines that enter the kernel will automatically set and unset IST1 to use the emergency stack
       poke (x64TssArea `plusPtr` 0x24) (ptrToWord x64TempKernelStackTop)

       -- Now set up fault handling interrupts
       idtPtr <- x64NewIDT
       x64LoadIdt idtPtr

foreign import ccall "arch.h &curUserSpaceState" x64TaskStateTmp :: Ptr ()
foreign import ccall "arch.h &kernelState" x64KernelState :: Ptr ()
foreign import ccall "arch.h x64SwitchToUserspace" x64SwitchToUserspaceAsm :: Ptr () -> Ptr () -> IO Int
foreign import ccall "arch.h x64WriteCR3" x64WriteCR3 :: Word64 -> IO ()
x64SwitchTasks :: Task X64Registers X64PageTable -> Task X64Registers X64PageTable -> IO (Task X64Registers X64PageTable)
x64SwitchTasks oldTask newTask =
    do oldTask' <- x64PeekTaskState oldTask x64TaskStateTmp
       x64PokeTaskState newTask x64TaskStateTmp
       let X64PageTable newPml4 = taskVirtMemTbl newTask

       x64WriteCR3 newPml4
       return oldTask'

x64UserPanic :: IO ()
x64UserPanic = do regs <- peek (castPtr x64TaskStateTmp `plusPtr` 8 :: Ptr X64GPRegisters)
                  x64DebugLog "Panic in init task: "
                  x64DebugLog (show regs)

foreign import ccall "arch.h &g_mboot_modules" x64BootModules :: Ptr ()
x64GetBootModule :: Word8 -> Ptr a -> IO ()
x64GetBootModule i p = do let moduleInfo = x64BootModules `plusPtr` (fromIntegral i * 128)
                          memcpy (castPtr p) (castPtr moduleInfo) 128

x64SwitchToUserspace :: IO (InterruptReason X64Exception)
x64SwitchToUserspace = do -- We're going to be returning to some location in userspace, but we
                          -- don't know if the location is mapped into memory or not. If it is not
                          -- we will simulate a VirtualMemoryFault so that the location can be mapped in

                          rip <- x64GetUserRIP
                          returnPhysPage <- x64GetPhysPageEntry rip
                          let returnPageIsPresent = testBit returnPhysPage x64_PAGE_PRESENT_BIT
                          if returnPageIsPresent
                             then do
                               reason <- x64SwitchToUserspaceAsm (castPtr x64TaskStateTmp) (castPtr x64KernelState)
                               case reason of
                                 256 -> x64GetUserSyscall >>= return . SysCallInterrupt
                                 _ | reason < 256 -> x64MapFault reason >>= return . TrapInterrupt
                                   | otherwise    -> return (DeviceInterrupt 0)
                             else -- Simulate the VM fault
                                 return (TrapInterrupt (VirtualMemoryFault FaultOnInstructionFetch rip))

x64ReturnToUserspace :: Word64 -> IO ()
x64ReturnToUserspace = poke ((castPtr x64TaskStateTmp) `plusPtr` 8)

x64GetUserRIP :: IO Word64
x64GetUserRIP = peek ((castPtr x64TaskStateTmp) `plusPtr` (15 * 8))

x64GetRSP :: IO Word64
x64GetRSP = peek ((castPtr x64KernelState) `plusPtr` (15 * 8))

x64GetUserSyscall :: IO SysCall
x64GetUserSyscall =
    do sysCallNumW <- peek ((castPtr x64TaskStateTmp :: Ptr Word64) `plusPtr` 8)
       -- We allow up to a maximum of five arguments, passed in rdi, rsi, rdx, r8, r9
       sysCallArg1W <- peek ((castPtr x64TaskStateTmp :: Ptr Word64) `plusPtr` (6 * 8))
       sysCallArg2W <- peek ((castPtr x64TaskStateTmp :: Ptr Word64) `plusPtr` (5 * 8))
       sysCallArg3W <- peek ((castPtr x64TaskStateTmp :: Ptr Word64) `plusPtr` (4 * 8))
       sysCallArg4W <- peek ((castPtr x64TaskStateTmp :: Ptr Word64) `plusPtr` (7 * 8))
       sysCallArg5W <- peek ((castPtr x64TaskStateTmp :: Ptr Word64) `plusPtr` (8 * 8))
       sysCallInfoPtrW <- peek ((castPtr x64TaskStateTmp :: Ptr Word64) `plusPtr` 16)
       case sysCallNumW of
         0 -> return (DebugLog (wordToPtr sysCallArg1W) (fromIntegral sysCallArg2W))
         1 -> return (CurrentAddressSpace (TaskId (fromIntegral sysCallArg1W)))
         2 -> do mapping <- peek (wordToPtr sysCallArg4W)
                 return (AddMapping (AddressSpaceRef (fromIntegral sysCallArg1W))
                                    (AR (fromIntegral sysCallArg2W) (fromIntegral sysCallArg3W))
                                    mapping)
         4 -> return (CloseAddressSpace (AddressSpaceRef (fromIntegral sysCallArg1W)))
         5 -> return (SwitchToAddressSpace (TaskId (fromIntegral sysCallArg1W)) (AddressSpaceRef (fromIntegral sysCallArg2W)))
         0x400 -> return (KillTask (TaskId (fromIntegral sysCallArg1W)))
         0x401 -> return CurrentTask
         0x402 -> return Fork
         0x403 -> return Yield
         0xff00 -> return ModuleCount
         0xff01 -> return (GetModuleInfo (fromIntegral sysCallArg1W) (wordToPtr sysCallArg2W))
         _ -> return (MalformedSyscall (fromIntegral sysCallNumW))

foreign import ccall "arch.h &x64TrapErrorCode" x64TrapErrorCode :: Ptr Word64
foreign import ccall "arch.h x64ReadCR2" x64ReadCR2 :: IO Word64
x64MapFault :: Int -> IO (CPUException X64Exception)
x64MapFault 0 = return DivideByZero
x64MapFault 1 = return DebugTrap
x64MapFault 2 = return $ ArchException X64NonMaskableInterrupt
x64MapFault 3 = return BreakpointTrap
x64MapFault 4 = return Overflow
x64MapFault 5 = return BoundsCheckFailed
x64MapFault 6 = return InvalidOpcode
x64MapFault 7 = return $ ArchException X64DeviceNotAvailable
x64MapFault 8 = return $ ArchException X64DoubleFault
x64MapFault 9 = return $ ArchException X64LegacyCoprocessorOverrun
x64MapFault 10 = return $ ArchException X64InvalidTSS
x64MapFault 11 = return $ ArchException X64SegmentNotPresent
x64MapFault 12 = return $ ArchException X64StackSegmentFault
x64MapFault 13 = return $ ProtectionException
x64MapFault 14 = do location <- x64ReadCR2
                    errCode <- peek x64TrapErrorCode
                    let faultReason
                            | testBit errCode 1 = FaultOnWrite
                            | testBit errCode 4 = FaultOnInstructionFetch
                            | otherwise = FaultOnRead
                    return (VirtualMemoryFault faultReason location)
x64MapFault 15 = return UnknownException
x64MapFault 16 = return FloatingPointException
x64MapFault 17 = return AlignmentCheck
x64MapFault 18 = return MachineCheck
x64MapFault 19 = return SIMDException
x64MapFault 20 = return VirtualMachineException
x64MapFault 30 = return $ ArchException X64SecurityException
x64MapFault _ = return UnknownException

x64HandleException :: X64Exception -> X64HosState -> IO (Either String X64HosState)
x64HandleException x _ = return (Left ("Unhandled Exception " ++ show x))
