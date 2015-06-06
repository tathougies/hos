module Main where

import Hos.User.SysCall
import Hos.Common.Types

import Control.Monad

import Data.Word
import Data.Elf

import Numeric

main :: IO ()
main = do childId <- hosFork
          case childId of
            0 -> doChild
            _ -> doParent

doParent :: IO ()
doParent = do hosDebugLog "Hello from the parent!"
              hosYield
              hosDebugLog "Hello parent 1"
              hosYield
              hosDebugLog "Hello parent 2"
              hosYield
              hosDebugLog "Hello parent 3"

doChild :: IO ()
doChild = do -- Now, we want to launch the second module given to us on the command line, which should be the storage server
             hosDebugLog "[init] checking for storage server..."
             modCount <- hosModuleCount
             if modCount < 2
                then hosDebugLog "[init] no storage.elf module given..."
                else do ModuleInfo _ start end <- hosGetModuleInfo 1
                        let vBase = 0xC000000000 :: Word64
                            vEnd = (fromIntegral (end - start)) + vBase

                        hosAddMappingToCurTask vBase vEnd (FromPhysical RetainInParent (UserSpace ReadOnly) (fromIntegral start))
                        hosDebugLog "[init] mapped storage module. going to read ELF information..."
                        (elfHdr, progHdrs) <- elf64ProgHdrs (wordToPtr vBase)
                        aRef <- hosEmptyAddressSpace
                        forM_ progHdrs $ \progHdr ->
                            case ph64Type progHdr of
                              PtLoad -> hosAddMapping aRef (ph64VAddr progHdr) (ph64VAddr progHdr + ph64MemSz progHdr) (CopyOnWrite (UserSpace ReadWrite) (ph64Offset progHdr + fromIntegral start))
                              _ -> return 0
                        hosDebugLog ("[init] exec storage.elf(entry at " ++ showHex (e64Entry elfHdr) "" ++ ")...")
                        hosEnterAddressSpace aRef (e64Entry elfHdr)


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

