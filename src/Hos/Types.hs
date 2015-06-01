module Hos.Types where

import Hos.CBits

import Control.Applicative

import Data.Monoid
import Data.Bits
import Data.Word
import Data.Set (Set)
import Data.Map.Base (Map)
import Data.IntervalMap (IntervalMap)
import qualified Data.Map.Base as M
import qualified Data.Set as S
--import Data.PQueue.Prio.Min (MinPQueue)

import Foreign.Ptr
import Foreign.Storable

type StackSize = Word64
newtype StackPtr = StackPtr { unStackPtr :: Word64}
newtype InstructionPtr = InstructionPtr { unInstructionPtr :: Word64 }

data VMFaultCause = FaultOnRead
                  | FaultOnWrite
                  | FaultOnInstructionFetch
                    deriving (Show, Read, Eq, Ord)

data CPUException archE = ArchException archE
                        | DivideByZero
                        | DebugTrap | BreakpointTrap
                        | Overflow  | BoundsCheckFailed| AlignmentCheck
                        | InvalidOpcode
                        | VirtualMemoryFault VMFaultCause Word64
                        | FloatingPointException | SIMDException
                        | MachineCheck
                        | VirtualMachineException
                        | ProtectionException
                        | UnknownException
                          deriving (Show, Read, Eq, Ord)

(<>) :: Monoid a => a -> a -> a
a <> b = mappend a b
infixr 6 <>

data StackDirection = StackGrowsUp
                    | StackGrowsDown
                      deriving (Show, Read, Eq, Ord)

data MemoryPermissions = Privileged ReadWrite
                       | UserSpace ReadWrite
                         deriving (Show, Eq, Ord)

data ReadWrite = ReadOnly | ReadWrite
               deriving (Show, Eq, Ord, Enum)

data Arch regs vMemTbl archE =
    Arch
    { archPageSize :: Word
    , archStackDirection :: StackDirection
    , archInitProcessPhysBase :: Word64
    , archUnmapInitTask :: IO ()

    , archNewVirtMemTbl :: IO vMemTbl
    , archMapKernel :: vMemTbl -> IO ()
    , archMapPage :: Word64 -> Word64 -> MemoryPermissions -> IO ()
    , archUnmapPage :: Word64 -> IO ()
    , archCopyPhysPage :: Word64 -> Word64 -> IO ()

    , archGetCurVirtMemTbl :: IO vMemTbl

    , archHandleException :: archE -> HosState regs vMemTbl archE -> IO (Either String (HosState regs vMemTbl archE))

    , archReadyForUserspace :: IO ()
    -- | Given the old task structure and the new task structure, serialize
    -- the current userspace as an update to the old task structure and switch
    -- to the new userspace
    , archSwitchTasks :: Task regs vMemTbl -> Task regs vMemTbl -> IO (Task regs vMemTbl)
    , archSwitchToUserspace :: IO (InterruptReason archE)
    , archReturnToUserspace :: Word64 -> IO ()
    , archUserPanic :: IO ()

    , archBootModuleCount :: IO Word8
    , archGetBootModule :: Word8 -> Ptr () -> IO ()

    , archDebugLog :: String -> IO () }

type AddressSpace = IntervalMap Word64 Mapping

data TaskReasonLeft = SysCall | Trap | IRQ
                    deriving (Show, Read, Eq, Ord, Enum)

newtype TaskId = TaskId Word32
    deriving (Show, Read, Eq, Ord)
newtype TaskPriority = TaskPriority Int
    deriving (Show, Eq, Ord)
newtype AddressSpaceRef = AddressSpaceRef Word32
    deriving (Show, Read, Eq, Ord)

data Privileges = Privileges
                { canCreateAddressSpace :: Bool
                , canModifyAddressSpace :: Maybe AddressRange
                , canAddFromPhysicalMapping :: Maybe AddressRange
                , canReplaceAddressSpaces :: Set TaskId
                , canKill :: Set TaskId
                , canGrantPrivileges :: Bool
                , canRevokePrivileges :: Bool }
                  deriving (Show, Eq)

data AddressRange = AR Word64 Word64
                    deriving (Show, Eq, Ord)

data FaultError = UnknownMapping
                  deriving (Show, Read, Eq, Ord)

-- | Some mappings, such as FromPhysical, do not have any sensible forking behavior.
--
--   This data type lets processes specify what should happen to these sorts of mapping
--   when a fork occurs.
data MappingTreatmentOnFork = RetainInParent -- ^ On fork, the parent will keep the region, while the region will be invalid in the child
                            | GiveToChild    -- ^ On fork, the child will be given the region, while the region will be invalid in the parent
                              deriving (Show, Eq, Ord)

data Mapping = AllocateOnDemand MemoryPermissions
             | AllocateImmediately MemoryPermissions (Maybe Word64)
             | FromPhysical MappingTreatmentOnFork MemoryPermissions Word64

               -- These can only be added or manipulated by the kernel
             | Mapped MemoryPermissions Word64
             | CopyOnWrite MemoryPermissions Word64
               deriving (Show, Eq, Ord)

data SysCall = DebugLog String

             | CreateAddressSpace
             | CurrentAddressSpace TaskId
             | AddMapping AddressSpaceRef AddressRange Mapping
             | DeleteMapping AddressSpaceRef AddressRange
             | CloseAddressSpace AddressSpaceRef
             | SwitchToAddressSpace TaskId AddressSpaceRef

             | AllocateRegion Word64
             | FreeRegion Word64 Word64

             | GrantPrivilege TaskId Privileges
             | RevokePrivilege TaskId Privileges

             | ReplaceTaskAddressSpace TaskId AddressSpaceRef
             | TaskAddressSpaceRef TaskId

               -- Process management (section 4)
             | KillTask TaskId
             | CurrentTask
             | Fork
             | Yield

               -- boot modules (section 0xFF)
             | ModuleCount
             | GetModuleInfo Word8 (Ptr ())

             | MalformedSyscall
               deriving Show

data Task archRegisters archVirtMemTbl =
    Task
    { taskSavedRegisters :: archRegisters
    , taskAddressSpace   :: AddressSpace
    , taskVirtMemTbl     :: archVirtMemTbl
    , taskReasonLeft     :: TaskReasonLeft

    , taskPrivileges     :: Privileges

    , taskAddressSpaces  :: Map AddressSpaceRef AddressSpace }
    deriving Show

data TaskDescriptor = TaskDescriptor
                    { tdStackSize    :: StackSize
                    , tdAddressSpace :: AddressSpace }


-- | A type that amounts to a zipper over the priority queue of tasks
data HosSchedule = HosSchedule
                 { hscUpcomingTasks :: [(TaskPriority, TaskId)]
                 , hscScheduledTasks :: [(TaskPriority, TaskId)]
                 , hscCurrentTask   :: TaskId
                 , hscCurrentTaskPrio :: TaskPriority }

data HosState regs vMem archE =
    HosState
    { hosSchedule :: HosSchedule

    , hosTasks :: Map TaskId (Task regs vMem) }

data InterruptReason archE = SysCallInterrupt SysCall
                           | TrapInterrupt (CPUException archE)
                           | DeviceInterrupt Int
                             deriving (Show)

data SysCallResult a = Success a
                     | Error SysCallError
                       deriving Show

data SysCallError = NoSuchAddressSpace
                  | NoSuchTask
                  | InsufficientPrivileges
                    deriving Show

class Registers regs where
    registersForTask :: StackPtr -> InstructionPtr -> regs

withPtr :: Word64 -> (Ptr a -> IO b) -> IO b
withPtr a f = f (wordToPtr a)

alignToPage :: Arch r v e -> Word64 -> Word64
alignToPage arch addr = addr .&. (complement (fromIntegral (archPageSize arch - 1)))

emptySchedule :: TaskId -> HosSchedule
emptySchedule tId = HosSchedule [] [] tId (TaskPriority 0)

hosPanic :: IO a
hosPanic = fail "Hos Panic"

currentTask :: HosState r v e -> IO (Task r v)
currentTask (HosState { hosSchedule = HosSchedule { hscCurrentTask = curTaskId }, hosTasks = ts }) =
    case M.lookup curTaskId ts of
      Nothing -> hosPanic
      Just task -> return task

modifyCurrentTaskIO :: HosState r v e -> (Task r v -> IO (a, Task r v)) -> IO (a, HosState r v e)
modifyCurrentTaskIO st@(HosState { hosSchedule = HosSchedule { hscCurrentTask = curTaskId }, hosTasks = ts}) f =
    case M.lookup curTaskId ts of
      Nothing -> hosPanic
      Just task -> do (a, task') <- f task
                      return (a, st { hosTasks = M.insert curTaskId task' (hosTasks st) })

modifyCurrentTask :: HosState r v e -> (Task r v -> (a, Task r v)) -> IO (a, HosState r v e)
modifyCurrentTask state f = modifyCurrentTaskIO state (return . f)

invalidAddressSpaceRef :: AddressSpaceRef
invalidAddressSpaceRef = AddressSpaceRef maxBound

unAddressSpaceRef :: AddressSpaceRef -> Word32
unAddressSpaceRef (AddressSpaceRef x) = x

nextRef :: (Enum a, Bounded a, Ord b) => (a -> b) -> (b -> a) -> Map b v -> b
nextRef mk unMk allBs
    | M.null allBs = mk minBound
    | otherwise = let (maxB, _) = M.findMax allBs
                  in mk (succ (unMk maxB))

class SysCallReturnable a where
    fromSysCallReturnable :: a -> Word64

instance SysCallReturnable AddressSpaceRef where
    fromSysCallReturnable (AddressSpaceRef x) = fromIntegral x
instance SysCallReturnable TaskId where
    fromSysCallReturnable (TaskId x) = fromIntegral x

instance SysCallReturnable SysCallError where
    fromSysCallReturnable InsufficientPrivileges = maxBound
    fromSysCallReturnable NoSuchAddressSpace = maxBound - 1
    fromSysCallReturnable NoSuchTask = maxBound - 2

instance SysCallReturnable () where
    fromSysCallReturnable () = 0

instance SysCallReturnable Word8 where
    fromSysCallReturnable = fromIntegral

instance Storable MemoryPermissions where
    sizeOf _ = 1
    alignment _ = 1

    poke p (Privileged ReadWrite) = poke (castPtr p :: Ptr Word8) 0x1
    poke p (Privileged ReadOnly) = poke (castPtr p :: Ptr Word8) 0x0
    poke p (UserSpace ReadWrite) = poke (castPtr p :: Ptr Word8) 0x3
    poke p (UserSpace ReadOnly) = poke (castPtr p :: Ptr Word8) 0x2

    peek p = do tag <- peek (castPtr p :: Ptr Word8)
                return ((if testBit tag 1 then UserSpace else Privileged) (if testBit tag 0 then ReadWrite else ReadOnly))

instance Storable MappingTreatmentOnFork where
    sizeOf _ = 1
    alignment _ = 1

    poke p RetainInParent = poke (castPtr p :: Ptr Word8) 0x0
    poke p GiveToChild = poke (castPtr p :: Ptr Word8) 0x1

    peek p = do tag <- peek (castPtr p :: Ptr Word8)
                case tag of
                  0x0 -> return RetainInParent
                  0x1 -> return GiveToChild

instance Storable Mapping where
    sizeOf _ = 11
    alignment _ = 1

    poke p (AllocateOnDemand perms) =
        poke (castPtr p :: Ptr Word8) 0x1 >>
        poke (castPtr p `plusPtr` 1) perms
    poke p (AllocateImmediately perms Nothing) =
        poke (castPtr p :: Ptr Word8) 0x2 >>
        poke (castPtr p `plusPtr` 1) perms >>
        poke (castPtr p `plusPtr` 2) (0 :: Word64)
    poke p (AllocateImmediately perms (Just alignment)) =
        poke (castPtr p :: Ptr Word8) 0x3 >>
        poke (castPtr p `plusPtr` 1) perms >>
        poke (castPtr p `plusPtr` 2) alignment
    poke p (FromPhysical forkTreatment perms physBase) =
        poke (castPtr p :: Ptr Word8) 0x4 >>
        poke (castPtr p `plusPtr` 1) forkTreatment >>
        poke (castPtr p `plusPtr` 2) perms >>
        poke (castPtr p `plusPtr` 3) physBase

    poke p (Mapped perms pageAddr) =
        poke (castPtr p :: Ptr Word8) 0xFF >>
        poke (castPtr p `plusPtr` 1) perms >>
        poke (castPtr p `plusPtr` 2) pageAddr
    poke p (CopyOnWrite perms pageAddr) =
        poke (castPtr p :: Ptr Word8) 0x7F >>
        poke (castPtr p `plusPtr` 1) perms >>
        poke (castPtr p `plusPtr` 2) pageAddr

    peek p = do tag <- peek (castPtr p :: Ptr Word8)
                case tag of
                  0x1 -> AllocateOnDemand <$> peek (castPtr p `plusPtr` 1)
                  0x2 -> AllocateImmediately <$> peek (castPtr p `plusPtr` 1)
                                             <*> pure Nothing
                  0x3 -> AllocateImmediately <$> peek (castPtr p `plusPtr` 1)
                                             <*> (Just <$> peek (castPtr p `plusPtr` 2))
                  0x4 -> FromPhysical <$> peek (castPtr p `plusPtr` 1)
                                      <*> peek (castPtr p `plusPtr` 2)
                                      <*> peek (castPtr p `plusPtr` 3)

                  0xFF -> Mapped <$> peek (castPtr p `plusPtr` 1)
                                 <*> peek (castPtr p `plusPtr` 2)
                  0x7F -> CopyOnWrite <$> peek (castPtr p `plusPtr` 1)
                                      <*> peek (castPtr p `plusPtr` 2)

fullAddressSpace :: AddressRange
fullAddressSpace = AR minBound maxBound

-- sometimes, JHC doesn't like invoking monadic actions whose values are ignored
-- this works around that
jhcWorkaround :: Monad m => m Int -> m Int
jhcWorkaround act = act >>= \x -> x `seq` return x

readOnlyPerms :: MemoryPermissions -> MemoryPermissions
readOnlyPerms (Privileged ReadWrite) = Privileged ReadOnly
readOnlyPerms (UserSpace ReadWrite) = UserSpace ReadOnly
readOnlyPerms x = x
