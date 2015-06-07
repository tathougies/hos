module Hos.SysCall where

import Hos.Types
import Hos.Privileges ()
import Hos.Task
import Hos.Memory
import Hos.CBits

import Control.Applicative
import Control.Monad

import Data.Functor
import Data.Char
import Data.Word
import qualified Data.Map.Base as M

import Foreign.Ptr
import Foreign.Storable

import Numeric

newtype SysCallM r v e a = SysCallM { runSysCallM :: Arch r v e -> HosState r v e -> IO (SysCallResult (a, HosState r v e)) }

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO (SysCallM r v e) where
    liftIO f = SysCallM $ \arch hos -> do x <- f
                                          return (Success (x, hos))

instance Functor (SysCallM r v e) where
    fmap f a = do x <- a
                  return (f x)

instance Applicative (SysCallM r v e) where
    a <*> b = do f <- a
                 x <- b
                 return (f x)
    pure = return

instance Monad (SysCallM r v e) where
    return a = SysCallM $ \_ hos -> return (Success (a, hos))
    a >>= b = SysCallM $ \arch hos ->
                runSysCallM a arch hos >>= \res ->
                  case res of
                    Error e -> do return (Error e)
                    Success (x, hos') -> runSysCallM (b x) arch hos'
    fail x = scDebugLog x >>= (\x1 -> x1 `seq` liftIO cHalt >>= (\x2 -> x2 `seq` undefined))

getHosState :: SysCallM r v e (HosState r v e)
getHosState = SysCallM $ \_ hos -> return (Success (hos, hos))

getArch :: SysCallM r v e (Arch r v e)
getArch = SysCallM $ \arch hos -> return (Success (arch, hos))

setHosState :: HosState r v e -> SysCallM r v e ()
setHosState st = SysCallM $ \_ _ -> return (Success ((), st))

throwSysCallError :: SysCallError -> SysCallM r v e a
throwSysCallError e = SysCallM $ \_ _ -> return (Error e)

getCurrentTask :: SysCallM r v e (Task r v)
getCurrentTask = do st <- getHosState
                    case M.lookup (hscCurrentTask (hosSchedule st)) (hosTasks st) of
                      Nothing -> liftIO hosPanic
                      Just curTask -> return curTask

getCurrentTaskPriority :: SysCallM r v e TaskPriority
getCurrentTaskPriority = do st <- getHosState
                            return (hscCurrentTaskPrio (hosSchedule st))

getCurrentTaskId :: SysCallM r v e TaskId
getCurrentTaskId = do st <- getHosState
                      return (hscCurrentTask (hosSchedule st))

getTask :: TaskId -> SysCallM r v e (Task r v)
getTask taskId = do st <- getHosState
                    case M.lookup taskId (hosTasks st) of
                      Nothing -> throwSysCallError NoSuchTask
                      Just task -> return task

getAddressSpace :: Task r v -> AddressSpaceRef -> SysCallM r v e AddressSpace
getAddressSpace (Task { taskAddressSpaces = aSpaces }) aRef =
    case M.lookup aRef aSpaces of
      Nothing -> throwSysCallError NoSuchAddressSpace
      Just spc -> return spc

setCurrentTask :: Task r v -> SysCallM r v e ()
setCurrentTask task =  do st <- getHosState
                          setTask (hscCurrentTask (hosSchedule st)) task

setTask :: TaskId -> Task r v -> SysCallM r v e ()
setTask taskId task = do st <- getHosState
                         let st' = st { hosTasks = M.insert taskId task (hosTasks st) }
                         setHosState st'

deleteTask :: TaskId -> SysCallM r v e ()
deleteTask taskId = do st <- getHosState
                       let st' = st { hosTasks = M.delete taskId (hosTasks st) }
                       x <- setHosState st'
                       x `seq` case M.lookup taskId (hosTasks st) of
                         Just task -> do arch <- getArch
                                         liftIO (releaseAddressSpace arch (taskAddressSpace task) (taskVirtMemTbl task))
                         Nothing -> return ()

scDebugLog :: String -> SysCallM r v e ()
scDebugLog msg = do a <- getArch
                    liftIO (archDebugLog a msg)

ensuringPrivileges :: Privileges -> SysCallM r v e a -> SysCallM r v e a
ensuringPrivileges privileges action =
    do curTask <- getCurrentTask
       if privileges <= taskPrivileges curTask
         then action
         else throwSysCallError InsufficientPrivileges

newTaskId :: SysCallM r v e TaskId
newTaskId = do st <- getHosState
               let (lastId, _) = M.findMax (hosTasks st)
                   nextAvailableId i@(TaskId x) =
                       case M.lookup i (hosTasks st) of
                         Nothing -> i
                         Just _ -> nextAvailableId (TaskId (x + 1))
               return (nextAvailableId lastId)

scheduleTask :: TaskPriority -> TaskId -> SysCallM r v e ()
scheduleTask prio task = do st <- getHosState
                            let sched = hosSchedule st
                                sched' = sched { hscUpcomingTasks = upcomingTasks' }

                                upcomingTasks = hscUpcomingTasks sched
                                upcomingTasks' = (prio, task):upcomingTasks

                                st' = st { hosSchedule = sched' }
                            setHosState st'

switchToNextTask :: SysCallM r v e (Task r v)
switchToNextTask = do st <- getHosState
                      case hscScheduledTasks (hosSchedule st) of
                        [] -> case hscUpcomingTasks (hosSchedule st) of
                                [] -> fail "No more tasks..."
                                _ -> do x <- setHosState (st { hosSchedule = (hosSchedule st) { hscScheduledTasks = hscUpcomingTasks (hosSchedule st), hscUpcomingTasks = [] } })
                                        x `seq` switchToNextTask
                        ((newPrio, newTaskId):tasks) ->
                            do let schedule' = (hosSchedule st) { hscScheduledTasks = tasks, hscCurrentTask = newTaskId, hscCurrentTaskPrio = newPrio }
                                   st' = st { hosSchedule = schedule' }
                               curTask <- getCurrentTask
                               case M.lookup newTaskId (hosTasks st) of
                                 Nothing -> fail ("couldn't find task: " ++ show newTaskId)
                                 Just newTask ->
                                     do arch <- getArch
                                        curTask' <- liftIO $ archSwitchTasks arch curTask newTask
                                        x <- setHosState st'
                                        x `seq` return curTask'

ensurePtr :: Ptr a -> ReadWrite -> SysCallM r v e ()
ensurePtr p rw = do a <- getArch
                    res <- liftIO (archTestPage a (ptrToWord p) (Privileged rw))
                    case res of
                      False -> do curTask <- getCurrentTask
                                  let faultCause = case rw of
                                                     ReadOnly -> FaultOnRead
                                                     ReadWrite -> FaultOnWrite
                                  res <- liftIO (handleFaultAt a faultCause (ptrToWord p) curTask)
                                  case res of
                                    Left err -> do x <- scDebugLog "Bad ptr"
                                                   x `seq` liftIO cHalt
                                    Right curTask' -> setCurrentTask curTask'
                      _ -> return ()

readCString :: Ptr Word8 -> Int -> IO String
readCString p n = go p n ""
    where go _ 0 a = return a
          go p !n a = do c <- peek p
                         let a' = a ++ (c' `seq` c')
                             c' = [chr (fromIntegral c)]
                         go (p `plusPtr` 1) (n - 1) (a' `seq` a')
