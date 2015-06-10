module Hos.Types
    ( module Hos.Common.Types
    , StackSize, StackPtr(..), InstructionPtr(..)
    , VMFaultCause(..)

    -- * Multi-arch support
    , StackDirection(..), Arch(..), Registers(..)
    , CPUException(..)

    -- * Task structures
    , AddressSpace(..), TaskReasonLeft(..)
    , Privileges(..), AddressRange(..), FaultError(..)
    , Task(..), TaskDescriptor(..)
    , invalidAddressSpaceRef, unAddressSpaceRef, nextRef
    , fullAddressSpace, baseAddressSpace
    , addrSpaceRegions

    -- * System calls
    , SysCall(..), SysCallResult(..), SysCallError(..)
    , SysCallReturnable(..)

    -- * IPC
    , MessageInDelivery(..), MessageEndpoint(..), PleaseWakeUp(..)

    -- * Scheduling
    , InterruptReason(..), HosSchedule(..), emptySchedule
    , currentTask, modifyCurrentTaskIO, modifyCurrentTask

    -- * Kernel state
    , HosState(..)

    -- * Utilities
    , (<>),  withPtr, alignToPage, hosPanic, jhcWorkaround, readOnlyPerms )
    where

import Hos.CBits
import Hos.Common.Types

import Control.Applicative

import Data.Monoid
import Data.Bits
import Data.Word
import Data.List (intersperse)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Map (Map)
import Data.IntervalMap (IntervalMap)
import qualified Data.IntervalMap as IntervalMap
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
--import Data.PQueue.Prio.Min (MinPQueue)

import Foreign.Ptr
import Foreign.Storable

import Numeric

type StackSize = Word64
newtype StackPtr = StackPtr { unStackPtr :: Word64}
newtype InstructionPtr = InstructionPtr { unInstructionPtr :: Word64 }
newtype PhysAddr = PhysAddr { paToWord64 :: Word64 } deriving (Show, Eq, Ord)

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

data StackDirection = StackGrowsUp
                    | StackGrowsDown
                      deriving (Show, Read, Eq, Ord)

data Arch regs vMemTbl archE =
    Arch
    { archPageSize :: Word
    , archStackDirection :: StackDirection
    , archInitProcessPhysBase :: Word64
    , archUnmapInitTask :: IO ()

    , archNewVirtMemTbl :: IO vMemTbl
    , archReleaseVirtMemTbl :: vMemTbl -> IO ()
    , archMapKernel :: vMemTbl -> IO ()
    , archMapPage :: Word64 -> Word64 -> MemoryPermissions -> IO ()
    , archTestPage :: Word64 -> MemoryPermissions -> IO Bool
    , archUnmapPage :: Word64 -> IO ()
    , archCopyPhysPage :: Word64 -> Word64 -> IO ()
    , archWalkVirtMemTbl :: vMemTbl -> Word64 -> Word64 -> (Word64 -> Word64 -> IO ()) -> IO ()

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
    , archEnableIO :: IO ()

    , archBootModuleCount :: IO Word8
    , archGetBootModule :: Word8 -> Ptr () -> IO ()

    , archDebugLog :: String -> IO () }

newtype AddressSpace = AddressSpace (IntervalMap Word64 Mapping)

data TaskReasonLeft = SysCall | Trap | IRQ
                    deriving (Show, Read, Eq, Ord, Enum)

data Privileges = Privileges
                { canCreateAddressSpace :: Bool
                , canModifyAddressSpace :: Maybe AddressRange
                , canAddFromPhysicalMapping :: Maybe AddressRange
                , canReplaceAddressSpaces :: Set TaskId
                , canKill :: Set TaskId
                , canGrantPrivileges :: Bool
                , canRevokePrivileges :: Bool
                , canRequestIO :: Bool }
                  deriving (Show, Eq)

data AddressRange = AR Word64 Word64
                    deriving (Show, Eq, Ord)

data FaultError = UnknownMapping
                | NoMessageInMapping
                  deriving (Show, Read, Eq, Ord)

data SysCall = DebugLog (Ptr Word8) Int -- special code 0x0

             -- Address space management (section 0)
             | EmptyAddressSpace
             | CurrentAddressSpace TaskId
             | AddMapping AddressSpaceRef AddressRange Mapping
             | DeleteMapping AddressSpaceRef AddressRange
             | CloseAddressSpace AddressSpaceRef
             | SwitchToAddressSpace TaskId AddressSpaceRef
             | EnterAddressSpace AddressSpaceRef Word64

               -- IPC (section 1)
             | DeliverMessage ChanId MessageEndpoint
             | RouteMessage ChanId MessageEndpoint
             | ReplyToMessage ChanId
             | WaitOnChannels WaitOnChannelsFlags Word64 (Ptr TaskId)
             | UnmaskChannel ChanId

               -- Input/output (section 3)
             | RequestIO

               -- Process management (section 4)
             | KillTask TaskId
             | CurrentTask
             | Fork
             | Yield
             | GrantPrivilege TaskId Privileges
             | RevokePrivilege TaskId Privileges

               -- boot modules (section 0xFF)
             | ModuleCount
             | GetModuleInfo Word8 (Ptr ())

             | MalformedSyscall Word16
               deriving Show

data MessageEndpoint = MessageEndpoint !TaskId !ChanId
                       deriving Show

data MessageInDelivery = MessageInDelivery
                       { midSource      :: !MessageEndpoint
                       , midDestination :: !MessageEndpoint
                       , midPages       :: Seq (Maybe Word64) }
                         deriving Show

data PleaseWakeUp = NoThanks
                  | FromWaitOnChannels WaitOnChannelsFlags (Ptr TaskId) TaskPriority
                    deriving Show

data Task archRegisters archVirtMemTbl =
    Task
    { taskSavedRegisters :: archRegisters
    , taskAddressSpace   :: AddressSpace
    , taskVirtMemTbl     :: archVirtMemTbl
    , taskReasonLeft     :: TaskReasonLeft

    , taskPrivileges     :: Privileges

    , taskOutgoingMessages :: Map ChanId MessageInDelivery
    , taskIncomingMessages :: Map ChanId (Seq MessageInDelivery)

    -- | Messages that have been received through a WaitOnChannel but which have not been replied to. Enables RouteMessage
    , taskInProcessMessages :: Map ChanId MessageInDelivery
    , taskUnmaskedChannels :: Set ChanId

    , taskPleaseWakeUp :: PleaseWakeUp

    -- | The router is the task that receives any message sent to TaskId 0. This allows processes to intercept messages, and allows the creation of things like emulatory servers
    , taskRouter         :: TaskId

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

                  | SourceChanIsFull
                  | DestChanIsFull
                  | ChannelIsEmpty
                  | WouldBeTruncated
                  | NoMessagesAvailable
                    deriving Show

class Registers regs where
    registersForTask :: StackPtr -> InstructionPtr -> regs
    registersWithIP :: InstructionPtr -> regs -> regs

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

    fromSysCallReturnable SourceChanIsFull = maxBound - 3
    fromSysCallReturnable DestChanIsFull = maxBound - 4
    fromSysCallReturnable ChannelIsEmpty = maxBound - 5

    fromSysCallReturnable WouldBeTruncated = 0x100000000 .|. 0x10300
    fromSysCallReturnable NoMessagesAvailable = 0x100000000 .|. 0x10301

instance SysCallReturnable () where
    fromSysCallReturnable () = 0

instance SysCallReturnable Word8 where
    fromSysCallReturnable = fromIntegral

fullAddressSpace :: AddressRange
fullAddressSpace = AR minBound maxBound

baseAddressSpace :: AddressSpace
baseAddressSpace = AddressSpace IntervalMap.empty

-- sometimes, JHC doesn't like invoking monadic actions whose values are ignored
-- this works around that
jhcWorkaround :: Monad m => m Int -> m Int
jhcWorkaround act = act >>= \x -> x `seq` return x

readOnlyPerms :: MemoryPermissions -> MemoryPermissions
readOnlyPerms (Privileged ReadWrite) = Privileged ReadOnly
readOnlyPerms (UserSpace ReadWrite) = UserSpace ReadOnly
readOnlyPerms x = x

instance Show AddressSpace where
    show (AddressSpace i) = concat (intersperse "\n" (map showRegion (IntervalMap.assocs i)))
        where showRegion ((start, end), mapping) =
                  showHex start (" - " ++ showHex end (": " ++ show mapping))

addrSpaceRegions :: AddressSpace -> [((Word64, Word64), Mapping)]
addrSpaceRegions (AddressSpace i) = IntervalMap.assocs i
