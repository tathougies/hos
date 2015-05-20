module Main where

    foreign import ccall "syscall.h syscall" syscall :: Int -> IO ()

    main :: IO ()
    main = syscall 20
