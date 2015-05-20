module Hos.Task where

import Hos.CBits
import Hos.Types

import Data.Word
import qualified Data.Set as S

mapAddressSpace :: Arch regs vMemTbl e -> vMemTbl -> AddressSpace -> IO ()
mapAddressSpace arch vMemTbl addrSpace =
    do archMapKernel arch vMemTbl

newTask :: Registers regs => Arch regs vMemTbl e -> TaskDescriptor -> IO () -> IO (Task regs vMemTbl)
newTask arch td fn =
    do vMemTbl <- archNewVirtMemTbl arch
       mapAddressSpace arch vMemTbl (tdAddressSpace td)

       -- Now allocate stack space
       stkPtr <- cPageAlignedAlloc (tdStackSize td)
       let newStk = case archStackDirection arch of
                      StackGrowsUp -> stkPtr
                      StackGrowsDown -> stkPtr + (tdStackSize td)

       return (Task
               { taskSavedRegisters = registersForTask (StackPtr newStk) (InstructionPtr 0)
               , taskReasonLeft = SysCall
               , taskAddressSpace = tdAddressSpace td
               , taskVirtMemTbl = vMemTbl

               , taskPrivileges = S.empty })

mkInitTask :: Registers regs => Arch regs vMemTbl e -> Word64 -> IO (Task regs vMemTbl)
mkInitTask arch initMainFuncAddr =
    do vMemTbl <- archGetCurVirtMemTbl arch
       return (Task
               { taskSavedRegisters = registersForTask (StackPtr 0) (InstructionPtr initMainFuncAddr)
               , taskReasonLeft = SysCall
               , taskAddressSpace = AddressSpace
               , taskVirtMemTbl = vMemTbl

               , taskPrivileges = S.fromList [ CreateAddressSpace
                                             , ModifyAddressSpace
                                             , AllocatePages maxBound ]})
