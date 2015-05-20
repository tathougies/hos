module Hos.Types where

import Hos.CBits

import Data.Monoid
import Data.Word
import Data.Set (Set)

import Foreign.Ptr

type StackSize = Word64
newtype StackPtr = StackPtr { unStackPtr :: Word64}
newtype InstructionPtr = InstructionPtr { unInstructionPtr :: Word64 }

data CPUException archE = ArchException archE
                        | DivideByZero
                        | DebugTrap | BreakpointTrap
                        | Overflow  | BoundsCheckFailed| AlignmentCheck
                        | InvalidOpcode
                        | VirtualMemoryFault
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

    , archNewVirtMemTbl :: IO vMemTbl
    , archMapKernel :: vMemTbl -> IO ()
    , archMapPage :: Word64 -> Word64 -> MemoryPermissions -> IO ()
    , archUnmapPage :: Word64 -> IO ()

    , archGetCurVirtMemTbl :: IO vMemTbl

    , archReadyForUserspace :: IO ()
    , archSwitchToUserspace :: Task regs vMemTbl -> IO (InterruptReason archE)

    , archDebugLog :: String -> IO () }

data AddressSpace = AddressSpace
                  deriving Show

data TaskReasonLeft = SysCall | Trap | IRQ
                    deriving (Show, Read, Eq, Ord, Enum)

data Privilege = CreateAddressSpaceP
               | ModifyAddressSpaceP

               | AllocatePagesP Word64
                 deriving (Show, Eq, Ord)

data SysCall = Create

data Task archRegisters archVirtMemTbl =
    Task
    { taskSavedRegisters :: archRegisters
    , taskAddressSpace   :: AddressSpace
    , taskVirtMemTbl     :: archVirtMemTbl
    , taskReasonLeft     :: TaskReasonLeft

    , taskPrivileges     :: Set Privilege }
    deriving Show

data TaskDescriptor = TaskDescriptor
                    { tdStackSize    :: StackSize
                    , tdAddressSpace :: AddressSpace }

data InterruptReason archE = SysCallInterrupt Int
                           | TrapInterrupt (CPUException archE)
                           | DeviceInterrupt Int
                             deriving (Show, Read, Eq, Ord)

class Registers regs where
    registersForTask :: StackPtr -> InstructionPtr -> regs

withPtr :: Word64 -> (Ptr a -> IO b) -> IO b
withPtr a f = f (wordToPtr a)
