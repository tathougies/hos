module Hos.Arch.X64.Interrupt where

import Hos.Arch.X64
import Hos.Arch.X64.Types
import Hos.CBits
import Hos.Memory
import Hos.Types

import Data.Word
import Data.Bits

import Foreign.Ptr
import Foreign.Storable

data X64Exception = X64NonMaskableInterrupt
                  | X64DeviceNotAvailable
                  | X64DoubleFault
                  | X64LegacyCoprocessorOverrun
                  | X64InvalidTSS
                  | X64SegmentNotPresent
                  | X64StackSegmentFault
                  | X64SecurityException
                    deriving (Show, Read, Eq, Ord, Enum)

data IDTGate = IDTCallGate
             { idtOffset    :: Word64
             , idtSelector  :: Word16
             , idtPrivilege :: X64Privilege }
             | IDTInterruptGate
             { idtOffset    :: Word64
             , idtSelector  :: Word16
             , idtPrivilege :: X64Privilege
             , idtISTEntry  :: Word8 }
             | IDTTrapGate
             { idtOffset    :: Word64
             , idtSelector  :: Word16
             , idtPrivilege :: X64Privilege
             , idtISTEntry  :: Word8 }
             deriving (Show, Eq, Ord)

instance Storable IDTGate where
    sizeOf _ = 16
    alignment _ = 16

    peek a = do ty <- peekElemOff (castPtr a :: Ptr Word8) 5
                istEntry <- peekElemOff (castPtr a :: Ptr Word8) 4
                offsetLo <- peek (castPtr a :: Ptr Word16)
                selector <- peekElemOff (castPtr a :: Ptr Word16) 1
                offsetMid <- peekElemOff (castPtr a :: Ptr Word16) 3
                offsetHi <- peekElemOff (castPtr a :: Ptr Word32) 2
                let offset = (fromIntegral offsetHi `shiftL` 32) .|. (fromIntegral offsetMid `shiftL` 16) .|. (fromIntegral offsetLo)
                    dpl = toEnum (fromIntegral ((ty `shiftR` 5) .&. 0x3))
                    ist = istEntry .&. 0x7
                case ty .&. 0xF of
                  0xC -> return (IDTCallGate offset selector dpl)
                  0xE -> return (IDTInterruptGate offset selector dpl ist)
                  0xF -> return (IDTTrapGate offset selector dpl ist)
                  _   -> fail ("No such IDT gate type: " ++ show (ty .&. 0x7))

    poke a (IDTCallGate offset selector dpl) = pokeIDT a 0xC offset selector dpl 0
    poke a (IDTInterruptGate offset selector dpl ist) = pokeIDT a 0xE offset selector dpl ist
    poke a (IDTTrapGate offset selector dpl ist) = pokeIDT a 0xF offset selector dpl ist

pokeIDT :: Ptr IDTGate -> Word8 -> Word64 -> Word16 -> X64Privilege -> Word8 -> IO ()
pokeIDT p ty offset selector dpl ist =
    do let offsetHi = fromIntegral ((offset `shiftR` 32) .&. 0xFFFFFFFF)
           offsetMid = fromIntegral ((offset `shiftR` 16) .&. 0xFFFF)
           offsetLo = fromIntegral (offset .&. 0xFFFF)
           istAndType = (1 `shiftL` 15) .|. -- Present Bit
                        (fromIntegral (fromEnum dpl) `shiftL` 13) .|.
                        (fromIntegral ty `shiftL` 8) .|.
                        (fromIntegral ist .&. 0x7)
       pokeElemOff (castPtr p :: Ptr Word16) 0 offsetLo
       pokeElemOff (castPtr p :: Ptr Word16) 1 selector
       pokeElemOff (castPtr p :: Ptr Word16) 2 istAndType
       pokeElemOff (castPtr p :: Ptr Word16) 3 offsetMid
       pokeElemOff (castPtr p :: Ptr Word32) 2 offsetHi
       pokeElemOff (castPtr p :: Ptr Word32) 3 0 -- Reserved high word


x64WithNewIDT :: (Ptr IDTGate -> IO b) -> IO (Word64, b)
x64WithNewIDT f = do newIdt <- cPageAlignedAlloc 4096 -- (fromIntegral $ 256 * sizeOf (undefined :: IDTGate))
                     let idtP = wordToPtr newIdt
                     memset idtP 0 4096 --(fromIntegral $ 256 * sizeOf (undefined :: IDTGate)) 0
                     ret <- f idtP
                     return (newIdt, ret)

x64KernelCS :: Word16
x64KernelCS = 0x8

foreign import ccall "arch.h &trap0" trap0 :: Ptr ()
foreign import ccall "arch.h &trap1" trap1 :: Ptr ()
foreign import ccall "arch.h &trap2" trap2 :: Ptr ()
foreign import ccall "arch.h &trap3" trap3 :: Ptr ()
foreign import ccall "arch.h &trap4" trap4 :: Ptr ()
foreign import ccall "arch.h &trap5" trap5 :: Ptr ()
foreign import ccall "arch.h &trap6" trap6 :: Ptr ()
foreign import ccall "arch.h &trap7" trap7 :: Ptr ()
foreign import ccall "arch.h &trap8" trap8 :: Ptr ()
foreign import ccall "arch.h &trap9" trap9 :: Ptr ()
foreign import ccall "arch.h &trap10" trap10 :: Ptr ()
foreign import ccall "arch.h &trap11" trap11 :: Ptr ()
foreign import ccall "arch.h &trap12" trap12 :: Ptr ()
foreign import ccall "arch.h &trap13" trap13 :: Ptr ()
foreign import ccall "arch.h &trap14" trap14 :: Ptr ()
foreign import ccall "arch.h &trap15" trap15 :: Ptr ()
foreign import ccall "arch.h &trap16" trap16 :: Ptr ()
foreign import ccall "arch.h &trap17" trap17 :: Ptr ()
foreign import ccall "arch.h &trap18" trap18 :: Ptr ()
foreign import ccall "arch.h &trap19" trap19 :: Ptr ()
foreign import ccall "arch.h &trap20" trap20 :: Ptr ()
foreign import ccall "arch.h &trap21" trap21 :: Ptr ()
foreign import ccall "arch.h &trap22" trap22 :: Ptr ()
foreign import ccall "arch.h &trap23" trap23 :: Ptr ()
foreign import ccall "arch.h &trap24" trap24 :: Ptr ()
foreign import ccall "arch.h &trap25" trap25 :: Ptr ()
foreign import ccall "arch.h &trap26" trap26 :: Ptr ()
foreign import ccall "arch.h &trap27" trap27 :: Ptr ()
foreign import ccall "arch.h &trap28" trap28 :: Ptr ()
foreign import ccall "arch.h &trap29" trap29 :: Ptr ()
foreign import ccall "arch.h &trap30" trap30 :: Ptr ()
foreign import ccall "arch.h &trap31" trap31 :: Ptr ()

foreign import ccall "arch.h &irq0" irq0 :: Ptr ()
foreign import ccall "arch.h &irq1" irq1 :: Ptr ()
foreign import ccall "arch.h &irq2" irq2 :: Ptr ()
foreign import ccall "arch.h &irq3" irq3 :: Ptr ()
foreign import ccall "arch.h &irq4" irq4 :: Ptr ()
foreign import ccall "arch.h &irq5" irq5 :: Ptr ()
foreign import ccall "arch.h &irq6" irq6 :: Ptr ()
foreign import ccall "arch.h &irq7" irq7 :: Ptr ()
foreign import ccall "arch.h &irq8" irq8 :: Ptr ()
foreign import ccall "arch.h &irq9" irq9 :: Ptr ()
foreign import ccall "arch.h &irq10" irq10 :: Ptr ()
foreign import ccall "arch.h &irq11" irq11 :: Ptr ()
foreign import ccall "arch.h &irq12" irq12 :: Ptr ()
foreign import ccall "arch.h &irq13" irq13 :: Ptr ()
foreign import ccall "arch.h &irq14" irq14 :: Ptr ()
foreign import ccall "arch.h &irq15" irq15 :: Ptr ()
x64NewIDT :: IO (Word64, Word16)
x64NewIDT =  x64WithNewIDT $ \idtP ->
               -- All the IDT entries for the faults use IST1, which we automatically manage
               -- to be the normal temporary stack (when we're in userspace) or the fault
               -- stack (for when we're in kernel-land)
               do pokeElemOff idtP  0 (IDTInterruptGate (ptrToWord trap0 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP  1 (IDTInterruptGate (ptrToWord trap1 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP  2 (IDTInterruptGate (ptrToWord trap2 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP  3 (IDTInterruptGate (ptrToWord trap3 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP  4 (IDTInterruptGate (ptrToWord trap4 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP  5 (IDTInterruptGate (ptrToWord trap5 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP  6 (IDTInterruptGate (ptrToWord trap6 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP  7 (IDTInterruptGate (ptrToWord trap7 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP  8 (IDTInterruptGate (ptrToWord trap8 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP  9 (IDTInterruptGate (ptrToWord trap9 ) x64KernelCS Ring0 1)
                  pokeElemOff idtP 10 (IDTInterruptGate (ptrToWord trap10) x64KernelCS Ring0 1)
                  pokeElemOff idtP 11 (IDTInterruptGate (ptrToWord trap11) x64KernelCS Ring0 1)
                  pokeElemOff idtP 12 (IDTInterruptGate (ptrToWord trap12) x64KernelCS Ring0 1)
                  pokeElemOff idtP 13 (IDTInterruptGate (ptrToWord trap13) x64KernelCS Ring0 1)
                  pokeElemOff idtP 14 (IDTInterruptGate (ptrToWord trap14) x64KernelCS Ring0 1)
                  pokeElemOff idtP 15 (IDTInterruptGate (ptrToWord trap15) x64KernelCS Ring0 1)
                  pokeElemOff idtP 16 (IDTInterruptGate (ptrToWord trap16) x64KernelCS Ring0 1)
                  pokeElemOff idtP 17 (IDTInterruptGate (ptrToWord trap17) x64KernelCS Ring0 1)
                  pokeElemOff idtP 18 (IDTInterruptGate (ptrToWord trap18) x64KernelCS Ring0 1)
                  pokeElemOff idtP 19 (IDTInterruptGate (ptrToWord trap19) x64KernelCS Ring0 1)
                  pokeElemOff idtP 20 (IDTInterruptGate (ptrToWord trap20) x64KernelCS Ring0 1)
                  pokeElemOff idtP 21 (IDTInterruptGate (ptrToWord trap21) x64KernelCS Ring0 1)
                  pokeElemOff idtP 22 (IDTInterruptGate (ptrToWord trap22) x64KernelCS Ring0 1)
                  pokeElemOff idtP 23 (IDTInterruptGate (ptrToWord trap23) x64KernelCS Ring0 1)
                  pokeElemOff idtP 24 (IDTInterruptGate (ptrToWord trap24) x64KernelCS Ring0 1)
                  pokeElemOff idtP 25 (IDTInterruptGate (ptrToWord trap25) x64KernelCS Ring0 1)
                  pokeElemOff idtP 26 (IDTInterruptGate (ptrToWord trap26) x64KernelCS Ring0 1)
                  pokeElemOff idtP 27 (IDTInterruptGate (ptrToWord trap27) x64KernelCS Ring0 1)
                  pokeElemOff idtP 28 (IDTInterruptGate (ptrToWord trap28) x64KernelCS Ring0 1)
                  pokeElemOff idtP 29 (IDTInterruptGate (ptrToWord trap29) x64KernelCS Ring0 1)
                  pokeElemOff idtP 30 (IDTInterruptGate (ptrToWord trap30) x64KernelCS Ring0 1)
                  pokeElemOff idtP 31 (IDTInterruptGate (ptrToWord trap31) x64KernelCS Ring0 1)

                  -- IRQS are remapped to 32+
                  pokeElemOff idtP 32 (IDTInterruptGate (ptrToWord irq0) x64KernelCS Ring0 1)
                  pokeElemOff idtP 33 (IDTInterruptGate (ptrToWord irq1) x64KernelCS Ring0 1)
                  pokeElemOff idtP 34 (IDTInterruptGate (ptrToWord irq2) x64KernelCS Ring0 1)
                  pokeElemOff idtP 35 (IDTInterruptGate (ptrToWord irq3) x64KernelCS Ring0 1)
                  pokeElemOff idtP 36 (IDTInterruptGate (ptrToWord irq4) x64KernelCS Ring0 1)
                  pokeElemOff idtP 37 (IDTInterruptGate (ptrToWord irq5) x64KernelCS Ring0 1)
                  pokeElemOff idtP 38 (IDTInterruptGate (ptrToWord irq6) x64KernelCS Ring0 1)
                  pokeElemOff idtP 39 (IDTInterruptGate (ptrToWord irq7) x64KernelCS Ring0 1)
                  pokeElemOff idtP 40 (IDTInterruptGate (ptrToWord irq8) x64KernelCS Ring0 1)
                  pokeElemOff idtP 41 (IDTInterruptGate (ptrToWord irq9) x64KernelCS Ring0 1)
                  pokeElemOff idtP 42 (IDTInterruptGate (ptrToWord irq10) x64KernelCS Ring0 1)
                  pokeElemOff idtP 43 (IDTInterruptGate (ptrToWord irq11) x64KernelCS Ring0 1)
                  pokeElemOff idtP 44 (IDTInterruptGate (ptrToWord irq12) x64KernelCS Ring0 1)
                  pokeElemOff idtP 45 (IDTInterruptGate (ptrToWord irq13) x64KernelCS Ring0 1)
                  pokeElemOff idtP 46 (IDTInterruptGate (ptrToWord irq14) x64KernelCS Ring0 1)
                  pokeElemOff idtP 47 (IDTInterruptGate (ptrToWord irq15) x64KernelCS Ring0 1)
                  return (fromIntegral (48 * sizeOf (undefined :: IDTGate)))


foreign import "arch.h loadIdt" cLoadIdt :: Ptr () -> IO ()
x64LoadIdt :: (Word64, Word16) -> IO ()
x64LoadIdt (idtBase, idtLength) =
    -- We'll use the memory at x64TempKernelStack as temporary memory
    do poke (castPtr x64TempKernelStack) idtLength
       poke (castPtr (x64TempKernelStack `plusPtr` 2)) idtBase
       cLoadIdt (castPtr x64TempKernelStack)
