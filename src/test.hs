module Main where

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Data.Char
import Data.IORef
import System.IO.Unsafe

strict :: a -> a
strict !x = x

videoBuffer :: IORef (Ptr Word8)
videoBuffer = unsafePerformIO $ newIORef (nullPtr `plusPtr` (0xffffff7f800b8000))

myPutStrLn :: [Char] -> IO ()
myPutStrLn s = let go (!ptr) [] = writeIORef videoBuffer ptr
                   go (!ptr) (!x:xs) = do poke ptr (fromIntegral (ord x))
                                          poke (ptr `plusPtr` 1) 0x04
                                          go   (ptr `plusPtr` 2) xs
               in readIORef videoBuffer >>= \buf -> go buf s

fib = 1:1:zipWith (+) fib (tail fib)

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

  


--  myPutStrLn ("This is now haskell... at your service!"  ++ "blah")
  myPutStrLn ("look ma! I can show numbemrs " ++ show (take 100 fib))
