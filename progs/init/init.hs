module Main where

import Hos.User.SysCall

import Control.Monad

main :: IO ()
main = do hosDebugLog "Going to try (++)"
          doChild
          childId <- hosFork
          case childId of
            0 -> doParent
            _ -> doChild

doParent :: IO ()
doParent = do hosDebugLog "Hello from the parent!"
              hosYield
              hosDebugLog "Hello parent 1"
              hosYield
              hosDebugLog "Hello parent 2"
              hosYield
              hosDebugLog "Hello parent 3"

doChild :: IO ()
doChild = do -- Now, we want to launch all the modules that were given to us on the command line
             hosDebugLog "hello from child"
             modCount <- hosModuleCount
             modsInfo <- mapM hosGetModuleInfo [1..(modCount - 1)]
             hosDebugLog "Init is going to start modules..."
             forM_ modsInfo $ \modInfo -> hosDebugLog ("Found module Module Info info info) -- " ++ show modInfo)
-- import Hos.User.SysCall
-- import Hos.User.Modulem

-- import Data.Elf

-- data InitConfiguration = InitConfiguration
--                        { icPhysDrivers :: [String]
--                        , icFsServer    :: String
--                        , icFsDrivers   :: [String] }

-- getModuleInfo :: IO [ModuleInfo]
-- getModuleInfo = do modCount <- hosGetModuleCount
--                    mapM hosGetModuleInfo [1..modCount]

-- main :: IO ()
-- main = do hosDebugLog "Init starting..."

--           -- Init's job is two-fold: launch the system and provide services
--           -- we cannot launch the system without providing services at the same time
--           -- so we need to fork into two processes. One will provide services best it
--           -- can until the system starts. The other will actually launch the processes.

--           res <- hosFork
--           case res of
--             Parent childId -> do privileges <- hosGetPrivileges (TaskId 0)

--                                  -- Grant our child the same privileges we do
--                                  hosGrantPrivileges childId privileges

--                                  -- Init has the special privilege of switching directly to any other task
--                                  hosSwitchToTask childId
--                                  provideInitServices
--             Child parentId -> initializeSystem >> hosDebugLog "Done initializing system..."

-- initializeSystem :: IO ()
-- initializeSystem =
--     do -- We want to initialize the system by reading the elf files as modules...
--        -- Normally, we'd interface with the VFS driver, but since we don't have that
--        -- right now, we're going to use a pseudo-VFS interface.
--        --
--        -- We're going to read in our command line, which will tell us how to load the servers

--        modCmdLine <- hosGetModuleCommandLine 0
--        let configuration = parseModCmdLine modCmdLine

--        modules <- getModuleInfo

--        -- Now, load the physical drivers
--        forM_ (icPhysDrivers configuration) $ \driverName ->
--            case findModule driverName modules of
--              Nothing -> hosDebugLog ("Can't find physical driver " ++ driverName)
--              Just modInfo -> do taskId <- loadELF modInfo
--                                 syncToInit (NewProcess physDriverPrivileges taskId "")
--                                 hosSwitchToTask taskId

--        case findModule (icFsServer configuration) modules of
--          Nothing -> hosDebugLog "Can't find filesystem server. Exiting..."
--          Just fsServer ->
--              do fsServerTaskId <- loadELF fsServer
--                 syncToInit (NewProcess systemServerPrivileges fsServerTaskId "")
--                 hosSwitchToTask fsServerTaskId -- let the filesystem server start

--                 forM_ (icFsDrivers configuration) $ \driverName ->
--                     case findModule driverName modules of
--                       Nothing -> hosDebugLog ("Can't find filesystem driver " ++ driverName)
--                       Just modInfo <- do taskId <- loadELF modInfo
--                                          syncToInit (NewProcess systemServerPrivileges taskId "")
--                                          hosSwitchToTask taskId

--                 -- Now we should be able to start up like any other process
--                 hosInitStandardLibrary

--                 -- run the initialization script
--                 system "$root:/scripts/init"

