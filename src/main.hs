module Main where

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Data.Char
import Data.IORef
import System.IO.Unsafe

import Numeric

import Hos.CBits
import Hos.Types
import Hos.Task

import Hos.Arch.Types
#if TARGET==x86_64
import Hos.Arch.X64
#endif

strict :: a -> a
strict !x = x

main :: IO ()
main = do
  -- Yay! We're now in Haskel(!!) land
  --
  -- There are a few steps here before we can really get into the nitty gritty of being
  -- an operating system.
  --
  -- We want to initialize the C task scheduling system. This will let us forcibly context
  -- switch between processes we manage.
  --
  -- We also will want to set up interrupts (again using C). We want interrupts for all
  -- faults and for at least the timer IRQ for now
  --
  -- Then we will want to create our resource catalog so that we can dish them out to the
  -- init process
  --
  -- Finally, we will want to load the init process, enable preemption, and enter user mode.
#if TARGET==x86_64
    hosMain x64
#endif

hosMain :: (Show regs, Show vTbl, Show e, Registers regs) => Arch regs vTbl e -> IO ()
hosMain a = do archDebugLog a ("Starting in Haskell land!")

               -- initTask <- newTask a (TaskDescriptor 0x4000 AddressSpace) (return ())
               initTask <- mkInitTask a 0x400078
               archDebugLog a ("Created init task with privileges " ++ show (taskPrivileges initTask))

               -- Now we want to map the init userspace stuff
               -- loadMultibootModule "hosInit"
               -- Now we want to get ready for userspace. This should also enable interrupts if necessary
               archReadyForUserspace a

               -- Now we will build our resource catalog
               -- archGetArchSpecificResources a

               -- Now we switch into the new task
               kernelize a initTask

kernelize :: (Registers regs, Show e) => Arch regs vMemTbl e -> Task regs vMemTbl -> IO ()
kernelize a t = do rsn <- archSwitchToUserspace a t
                   archDebugLog a ("Got back from userspace because of a " ++ show rsn)
