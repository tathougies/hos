module Hos.Task where

import Hos.CBits
import Hos.Types
import Hos.Privileges
import Hos.Memory

import Data.Word
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Base as M
import qualified Data.IntervalMap as IntervalMap

import Numeric

import Util

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

               , taskPrivileges = noPrivileges

               , taskAddressSpaces = M.empty })

mkInitTask :: Registers regs => Arch regs vMemTbl e -> Word64 -> IO (Task regs vMemTbl)
mkInitTask arch initMainFuncAddr =
    do vMemTbl <- archGetCurVirtMemTbl arch
       return (Task
               { taskSavedRegisters = registersForTask (StackPtr 0) (InstructionPtr initMainFuncAddr)
               , taskReasonLeft = SysCall
               , taskAddressSpace = IntervalMap.empty
               , taskVirtMemTbl = vMemTbl

               , taskPrivileges = initPrivileges

               , taskAddressSpaces = M.empty })

taskWithMapping :: Word64 -> Word64 -> Mapping -> Task regs vMemTbl -> Task regs vMemTbl
taskWithMapping vAddr endVAddr mapping task =
    task {
      taskAddressSpace = IntervalMap.insert' splitMapping (vAddr, endVAddr) mapping (taskAddressSpace task)
    }
    where splitMapping (vAddr, _) a =
            case a of
              FromPhysical forkTreatment perms physBase ->
                  (a, FromPhysical forkTreatment perms (physBase + (endVAddr - vAddr)))
              _ -> (a, a)

taskWithModifiedAddressSpace :: AddressSpaceRef -> AddressSpace -> Task r v -> Task r v
taskWithModifiedAddressSpace aRef aSpace task = task { taskAddressSpaces = M.insert aRef aSpace (taskAddressSpaces task) }

taskWithDeletedAddressSpace :: AddressSpaceRef -> Task r v -> Task r v
taskWithDeletedAddressSpace aRef task = task { taskAddressSpaces = M.delete aRef (taskAddressSpaces task) }

taskWithAddressSpace :: AddressSpace -> Task r v -> Task r v
taskWithAddressSpace aSpc task = task { taskAddressSpace = aSpc }

handleFaultAt :: Arch r v e -> VMFaultCause -> Word64 -> Task r v -> IO (Either FaultError (Task r v))
handleFaultAt arch faultCause vAddr task =
    case IntervalMap.lookup vAddr (taskAddressSpace task) of
      Just (lowVAddr, FromPhysical _ perms physBase) ->
          do let pageOffset = alignToPage arch (vAddr - lowVAddr)
             archMapPage arch (alignToPage arch vAddr) (physBase + pageOffset) perms
             return (Right task)
      Just (lowVAddr, AllocateOnDemand perms) ->
          do phys <- cPageAlignedPhysAlloc (fromIntegral (archPageSize arch))
             let task' = taskWithMapping (alignToPage arch vAddr) (alignToPage arch (vAddr + fromIntegral (archPageSize arch))) (Mapped perms phys) task
             archMapPage arch (alignToPage arch vAddr) phys perms
             return (Right task')

      -- Since we were already mapped, just map the page...
      Just (lowVAddr, Mapped perms physBase) ->
          do let pageOffset = alignToPage arch (vAddr - lowVAddr)
             archMapPage arch (alignToPage arch vAddr) (physBase + pageOffset) perms
             return (Right task)
      Just (lowVAddr, CopyOnWrite perms physBase) ->
          case faultCause of
            FaultOnWrite ->
              do let pageOffset = alignToPage arch (vAddr - lowVAddr)
                     pageToCopyBase = physBase + pageOffset
                 newPage <- cPageAlignedPhysAlloc (fromIntegral (archPageSize arch))
                 let task' = taskWithMapping (alignToPage arch vAddr) (alignToPage arch (vAddr + fromIntegral (archPageSize arch))) (Mapped perms newPage) task
                 archCopyPhysPage arch pageToCopyBase newPage
                 archMapPage arch (alignToPage arch vAddr) newPage perms
                 return (Right task')
            _ ->
              do let pageOffset = alignToPage arch (vAddr - lowVAddr)
                 archMapPage arch (alignToPage arch vAddr) (physBase + pageOffset) (readOnlyPerms perms)
                 return (Right task)
      _ -> return (Left UnknownMapping)


taskFork :: Arch r v e -> Task r v -> IO (Task r v, Task r v)
taskFork a t =
    do parentMemTbl <- archNewVirtMemTbl a
       childMemTbl <- archNewVirtMemTbl a
       archMapKernel a parentMemTbl
       archMapKernel a childMemTbl

       let parentT = t { taskAddressSpace = parentAddressSpace
                       , taskVirtMemTbl = parentMemTbl }
           childT = t { taskAddressSpace = childAddressSpace
                      , taskVirtMemTbl = childMemTbl }

           allMappings = IntervalMap.assocs (taskAddressSpace t)
           parentAddressSpace = IntervalMap.fromList (mapMaybe (makeCow True) allMappings)
           childAddressSpace = IntervalMap.fromList (mapMaybe (makeCow False) allMappings)

           makeCow isParent (r, x@(FromPhysical behavior _ _)) =
               case (isParent, behavior) of
                 (True, RetainInParent) -> Just (r, x)
                 (False, GiveToChild) -> Just (r, x)
                 _ -> Nothing
           makeCow _ (r, Mapped perms base) = Just (r, CopyOnWrite perms base)
           makeCow _ x = Just x
       return (parentT, childT)
