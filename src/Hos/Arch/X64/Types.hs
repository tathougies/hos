module Hos.Arch.X64.Types where

import Hos.Types

import Data.Word

import Foreign.Ptr
import Foreign.Storable
import Numeric

data X64Privilege = Ring0 | Ring1 | Ring2 | Ring3 deriving (Show, Read, Eq, Ord, Enum)

data X64Registers = X64Registers
                  { x64GPRegisters   :: X64GPRegisters
                    -- Broken up because JHC doesn't like large structures (?)
                  , x64SSERegisters1 :: X64SSERegisters1
                  , x64SSERegisters2 :: X64SSERegisters2
                  , x64MMXRegisters  :: X64MMXRegisters
                  , x64FPState       :: X64FPState }
                  deriving (Show, Eq, Ord)

data WordSSE = WordSSE
             { sseLo :: !Word64
             , sseHi :: !Word64 }
             deriving (Eq, Ord)

data WordMMX = WordMMX
             { mmxHi :: !Word16
             , mmxLo :: !Word64 }
               deriving (Eq, Ord)

instance Show WordSSE where
    show (WordSSE 0 lo) = showHex lo ""
    show (WordSSE hi lo) = (showHex hi . showHex lo) ""
instance Show WordMMX where
    show (WordMMX 0 lo) = showHex lo ""
    show (WordMMX hi lo) = (showHex hi . showHex lo) ""

-- | General purpose x64 registers
data X64GPRegisters = X64GPRegisters
                    { x64GpRax :: !Word64
                    , x64GpRbx :: !Word64
                    , x64GpRcx :: !Word64
                    , x64GpRdx :: !Word64
                    , x64GpRsi :: !Word64
                    , x64GpRdi :: !Word64
                    , x64GpR8  :: !Word64
                    , x64GpR9  :: !Word64
                    , x64GpR10 :: !Word64
                    , x64GpR11 :: !Word64
                    , x64GpR12 :: !Word64
                    , x64GpR13 :: !Word64
                    , x64GpR14 :: !Word64
                    , x64GpR15 :: !Word64
                    , x64GpRip :: !Word64
                    , x64GpRsp :: !Word64
                    , x64GpRbp :: !Word64
                    , x64GpRflags :: !Word64 }
                    deriving (Show, Eq, Ord)

data X64FPState = X64FPState
                { x64FpRdp       :: !Word64
                , x64FpRip       :: !Word64
                , x64FpMxcsr     :: !Word32
                , x64FpMxcsrMask :: !Word32
                , x64FpFcw       :: !Word16
                , x64FpFsw       :: !Word16
                , x64FpFtw       :: !Word8
                , x64FpFop       :: !Word16 }
                  deriving (Show, Eq, Ord)

data X64MMXRegisters = X64MMXRegisters
                     { x64ST0 :: !WordMMX
                     , x64ST1 :: !WordMMX
                     , x64ST2 :: !WordMMX
                     , x64ST3 :: !WordMMX
                     , x64ST4 :: !WordMMX
                     , x64ST5 :: !WordMMX
                     , x64ST6 :: !WordMMX
                     , x64ST7 :: !WordMMX }
                       deriving (Show, Eq, Ord)

data X64SSERegisters1 = X64SSERegisters1
                      { x64Xmm0 :: !WordSSE
                      , x64Xmm1 :: !WordSSE
                      , x64Xmm2 :: !WordSSE
                      , x64Xmm3 :: !WordSSE
                      , x64Xmm4 :: !WordSSE
                      , x64Xmm5 :: !WordSSE
                      , x64Xmm6 :: !WordSSE
                      , x64Xmm7 :: !WordSSE }
                        deriving (Show, Eq, Ord)
data X64SSERegisters2 = X64SSERegisters2
                      { x64Xmm8 :: !WordSSE
                      , x64Xmm9 :: !WordSSE
                      , x64XmmA :: !WordSSE
                      , x64XmmB :: !WordSSE
                      , x64XmmC :: !WordSSE
                      , x64XmmD :: !WordSSE
                      , x64XmmE :: !WordSSE
                      , x64XmmF :: !WordSSE }
                        deriving (Show, Eq, Ord)

sseZero = WordSSE 0 0
mmxZero = WordMMX 0 0

x64EmptyRegs :: X64Registers
x64EmptyRegs = X64Registers emptyGpRegisters emptySSERegisters1 emptySSERegisters2 emptyMMXRegisters emptyFPState
    where emptyGpRegisters = X64GPRegisters
                           { x64GpRax = 0
                           , x64GpRbx = 0
                           , x64GpRcx = 0
                           , x64GpRdx = 0
                           , x64GpRsi = 0
                           , x64GpRdi = 0
                           , x64GpR8  = 0
                           , x64GpR9  = 0
                           , x64GpR10 = 0
                           , x64GpR11 = 0
                           , x64GpR12 = 0
                           , x64GpR13 = 0
                           , x64GpR14 = 0
                           , x64GpR15 = 0
                           , x64GpRip = 0
                           , x64GpRsp = 0
                           , x64GpRbp = 0
                           , x64GpRflags = 0}

          emptySSERegisters1 = X64SSERegisters1
                             { x64Xmm0 = sseZero
                             , x64Xmm1 = sseZero
                             , x64Xmm2 = sseZero
                             , x64Xmm3 = sseZero
                             , x64Xmm4 = sseZero
                             , x64Xmm5 = sseZero
                             , x64Xmm6 = sseZero
                             , x64Xmm7 = sseZero }
          emptySSERegisters2 = X64SSERegisters2
                             { x64Xmm8 = sseZero
                             , x64Xmm9 = sseZero
                             , x64XmmA = sseZero
                             , x64XmmB = sseZero
                             , x64XmmC = sseZero
                             , x64XmmD = sseZero
                             , x64XmmE = sseZero
                             , x64XmmF = sseZero }

          emptyMMXRegisters = X64MMXRegisters
                            { x64ST0 = mmxZero
                            , x64ST1 = mmxZero
                            , x64ST2 = mmxZero
                            , x64ST3 = mmxZero
                            , x64ST4 = mmxZero
                            , x64ST5 = mmxZero
                            , x64ST6 = mmxZero
                            , x64ST7 = mmxZero }

          emptyFPState = X64FPState
                       { x64FpRdp = 0
                       , x64FpRip = 0
                       , x64FpMxcsr = 0x1f80 -- page 9-3, vol 3a of arch programmers manual, amd
                       , x64FpMxcsrMask = 0
                       , x64FpFcw = 0x037f
                       , x64FpFsw = 0
                       , x64FpFtw = 0xffff
                       , x64FpFop = 0 }

x64PageSize :: Word
x64PageSize = 4096

asmReasonLeft :: TaskReasonLeft -> Word64
asmReasonLeft SysCall = 1
asmReasonLeft Trap = 2
asmReasonLeft IRQ = 3

reasonLeftFromAsm :: Word64 -> TaskReasonLeft
reasonLeftFromAsm 1 = SysCall
reasonLeftFromAsm 2 = Trap
reasonLeftFromAsm 3 = IRQ

-- given a task, and a region of memory, store the task state in that region
-- This region can then be passed on to switchToUserspace in context.S
x64PokeTaskState :: Task X64Registers vMemTbl -> Ptr a -> IO ()
x64PokeTaskState (Task { taskReasonLeft = reasonLeft
                       , taskSavedRegisters = regs }) p =
  do poke (castPtr p) (asmReasonLeft reasonLeft)
     poke (castPtr (p `plusPtr` 8) :: Ptr X64GPRegisters) (x64GPRegisters regs)
     x64PokeSSEState (castPtr p `plusPtr` 0x98) regs

-- TODO Save page tables as well
x64PeekTaskState :: Task X64Registers vMemTbl -> Ptr a -> IO (Task X64Registers vMemTbl)
x64PeekTaskState t p =
    do reasonLeftI <- peek (castPtr p :: Ptr Word64)
       gpRegs <- peek (castPtr (p `plusPtr` 8) :: Ptr X64GPRegisters)
       let regs' = (taskSavedRegisters t) { x64GPRegisters = gpRegs }
       regs'' <- x64PeekSSEState (castPtr p `plusPtr` 0x98) regs'
       return (t { taskReasonLeft = reasonLeftFromAsm reasonLeftI, taskSavedRegisters = regs'' })

instance Storable WordSSE where
    sizeOf _ = 16
    alignment _ = 16

    poke ptr (WordSSE hi lo) = poke (castPtr ptr) lo >> poke (castPtr ptr `plusPtr` 8) hi
    peek ptr = do lo <- peek (castPtr ptr)
                  hi <- peek (castPtr ptr `plusPtr` 8)
                  return (WordSSE hi lo)

instance Storable WordMMX where
    sizeOf _ = 10
    alignment _ = 16

    poke ptr (WordMMX hi lo) = poke (castPtr ptr) lo >> poke (castPtr ptr `plusPtr` 8) hi
    peek ptr = do lo <- peek (castPtr ptr)
                  hi <- peek (castPtr ptr `plusPtr` 8)
                  return (WordMMX hi lo)

instance Storable X64GPRegisters where
    sizeOf _ = 144
    alignment _ = 16

    poke ptr gp =
      do let p = castPtr ptr :: Ptr Word64
         pokeElemOff p 0 (x64GpRax gp)
         pokeElemOff p 1 (x64GpRbx gp)
         pokeElemOff p 2 (x64GpRcx gp)
         pokeElemOff p 3 (x64GpRdx gp)
         pokeElemOff p 4 (x64GpRsi gp)
         pokeElemOff p 5 (x64GpRdi gp)
         pokeElemOff p 6 (x64GpR8 gp)
         pokeElemOff p 7 (x64GpR9 gp)
         pokeElemOff p 8 (x64GpR10 gp)
         pokeElemOff p 9 (x64GpR11 gp)
         pokeElemOff p 10 (x64GpR12 gp)
         pokeElemOff p 11 (x64GpR13 gp)
         pokeElemOff p 12 (x64GpR14 gp)
         pokeElemOff p 13 (x64GpR15 gp)
         pokeElemOff p 14 (x64GpRip gp)
         pokeElemOff p 15 (x64GpRsp gp)
         pokeElemOff p 16 (x64GpRbp gp)
         pokeElemOff p 17 (x64GpRflags gp)

    peek ptr =
      do let p = castPtr ptr :: Ptr Word64
         rax <- peekElemOff p 0
         rbx <- peekElemOff p 1
         rcx <- peekElemOff p 2
         rdx <- peekElemOff p 3
         rsi <- peekElemOff p 4
         rdi <- peekElemOff p 5
         r8  <- peekElemOff p 6
         r9  <- peekElemOff p 7
         r10 <- peekElemOff p 8
         r11 <- peekElemOff p 9
         r12 <- peekElemOff p 10
         r13 <- peekElemOff p 11
         r14 <- peekElemOff p 12
         r15 <- peekElemOff p 13
         rip <- peekElemOff p 14
         rsp <- peekElemOff p 15
         rbp <- peekElemOff p 16
         rflags <- peekElemOff p 17
         return (X64GPRegisters
                { x64GpRax = rax
                , x64GpRbx = rbx
                , x64GpRcx = rcx
                , x64GpRdx = rdx
                , x64GpRsi = rsi
                , x64GpRdi = rdi
                , x64GpR8  = r8
                , x64GpR9  = r9
                , x64GpR10 = r10
                , x64GpR11 = r11
                , x64GpR12 = r12
                , x64GpR13 = r13
                , x64GpR14 = r14
                , x64GpR15 = r15
                , x64GpRip = rip
                , x64GpRsp = rsp
                , x64GpRbp = rbp
                , x64GpRflags = rflags })

miscPage1, miscPage2, miscPage3, miscPage4, miscPage5, miscPage6, miscPage7, miscPage8 :: Word64
miscPage1 = 0xffffff7f7fff8000 -- These are offset from the C ones so that they don't interfere
miscPage2 = miscPage1 - 0x1000
miscPage3 = miscPage2 - 0x1000
miscPage4 = miscPage3 - 0x1000
miscPage5 = miscPage4 - 0x1000
miscPage6 = miscPage5 - 0x1000
miscPage7 = miscPage6 - 0x1000
miscPage8 = miscPage7 - 0x1000

foreign import ccall "arch.h &kernelTmpStack" x64TempKernelStack :: Ptr ()
foreign import ccall "arch.h &kernelTmpStack_top" x64TempKernelStackTop :: Ptr ()

x64_PAGE_PRESENT_BIT :: Int
x64_PAGE_PRESENT_BIT = 0

-- | Pokes the sse/floating point state in the registers into the position pointed to by memory
x64PokeSSEState :: Ptr Word64 -> X64Registers -> IO ()
x64PokeSSEState p (X64Registers { x64FPState = fpState, x64MMXRegisters = mmx, x64SSERegisters1 = sse1, x64SSERegisters2 = sse2 }) =
    do let X64FPState { x64FpRdp = rdp, x64FpRip = rip
                      , x64FpMxcsr = mxcsr, x64FpMxcsrMask = mxcsrMask
                      , x64FpFcw = fcw, x64FpFsw = fsw, x64FpFtw = ftw
                      , x64FpFop = fop } = fpState

           X64MMXRegisters { x64ST0 = st0
                           , x64ST1 = st1
                           , x64ST2 = st2
                           , x64ST3 = st3
                           , x64ST4 = st4
                           , x64ST5 = st5
                           , x64ST6 = st6
                           , x64ST7 = st7 } = mmx

           X64SSERegisters1 { x64Xmm0 = xmm0
                            , x64Xmm1 = xmm1
                            , x64Xmm2 = xmm2
                            , x64Xmm3 = xmm3
                            , x64Xmm4 = xmm4
                            , x64Xmm5 = xmm5
                            , x64Xmm6 = xmm6
                            , x64Xmm7 = xmm7 } = sse1

           X64SSERegisters2 { x64Xmm8 = xmm8
                            , x64Xmm9 = xmm9
                            , x64XmmA = xmmA
                            , x64XmmB = xmmB
                            , x64XmmC = xmmC
                            , x64XmmD = xmmD
                            , x64XmmE = xmmE
                            , x64XmmF = xmmF } = sse2

       poke (castPtr p) fcw
       poke (castPtr p `plusPtr` 0x2) fsw
       poke (castPtr p `plusPtr` 0x4) ftw
       poke (castPtr p `plusPtr` 0x6) fop
       poke (castPtr p `plusPtr` 0x8) rip
       poke (castPtr p `plusPtr` 0x10) rdp
       poke (castPtr p `plusPtr` 0x18) mxcsr
       poke (castPtr p `plusPtr` 0x1C) mxcsrMask
       poke (castPtr p `plusPtr` 0x20) st0
       poke (castPtr p `plusPtr` 0x30) st1
       poke (castPtr p `plusPtr` 0x40) st2
       poke (castPtr p `plusPtr` 0x50) st3
       poke (castPtr p `plusPtr` 0x60) st4
       poke (castPtr p `plusPtr` 0x70) st5
       poke (castPtr p `plusPtr` 0x80) st6
       poke (castPtr p `plusPtr` 0x90) st7
       poke (castPtr p `plusPtr` 0x0A0) xmm0
       poke (castPtr p `plusPtr` 0x0B0) xmm1
       poke (castPtr p `plusPtr` 0x0C0) xmm2
       poke (castPtr p `plusPtr` 0x0D0) xmm3
       poke (castPtr p `plusPtr` 0x0E0) xmm4
       poke (castPtr p `plusPtr` 0x0F0) xmm5
       poke (castPtr p `plusPtr` 0x100) xmm6
       poke (castPtr p `plusPtr` 0x110) xmm7
       poke (castPtr p `plusPtr` 0x120) xmm8
       poke (castPtr p `plusPtr` 0x130) xmm9
       poke (castPtr p `plusPtr` 0x140) xmmA
       poke (castPtr p `plusPtr` 0x150) xmmB
       poke (castPtr p `plusPtr` 0x160) xmmC
       poke (castPtr p `plusPtr` 0x170) xmmD
       poke (castPtr p `plusPtr` 0x180) xmmE
       poke (castPtr p `plusPtr` 0x190) xmmF
       return ()

x64PeekFPState :: Ptr Word64 -> IO X64FPState
x64PeekFPState p =
    do fcw <- peek (castPtr p)
       fsw <- peek (castPtr p `plusPtr` 0x2)
       ftw <- peek (castPtr p `plusPtr` 0x4)
       fop <- peek (castPtr p `plusPtr` 0x6)
       rip <- peek (castPtr p `plusPtr` 0x8)
       rdp <- peek (castPtr p `plusPtr` 0x10)
       mxcsr <- peek (castPtr p `plusPtr` 0x18)
       mxcsrMask <- peek (castPtr p `plusPtr` 0x1C)
       return ( X64FPState
              { x64FpRdp = rdp, x64FpRip = rip
              , x64FpMxcsr = mxcsr, x64FpMxcsrMask = mxcsrMask
              , x64FpFcw = fcw, x64FpFsw = fsw, x64FpFtw = ftw
              , x64FpFop = fop } )

x64PeekMMXRegisters :: Ptr Word64 -> IO X64MMXRegisters
x64PeekMMXRegisters p =
    do st0 <- peek (castPtr p)
       st1 <- peek (castPtr p `plusPtr` 0x10)
       st2 <- peek (castPtr p `plusPtr` 0x20)
       st3 <- peek (castPtr p `plusPtr` 0x30)
       st4 <- peek (castPtr p `plusPtr` 0x40)
       st5 <- peek (castPtr p `plusPtr` 0x50)
       st6 <- peek (castPtr p `plusPtr` 0x60)
       st7 <- peek (castPtr p `plusPtr` 0x70)
       return ( X64MMXRegisters
              { x64ST0 = st0
              , x64ST1 = st1
              , x64ST2 = st2
              , x64ST3 = st3
              , x64ST4 = st4
              , x64ST5 = st5
              , x64ST6 = st6
              , x64ST7 = st7 } )

x64PeekSSERegisters1 :: Ptr Word64 -> IO X64SSERegisters1
x64PeekSSERegisters1 p =
    do xmm0 <- peek (castPtr p)
       xmm1 <- peek (castPtr p `plusPtr` 0x10)
       xmm2 <- peek (castPtr p `plusPtr` 0x20)
       xmm3 <- peek (castPtr p `plusPtr` 0x30)
       xmm4 <- peek (castPtr p `plusPtr` 0x40)
       xmm5 <- peek (castPtr p `plusPtr` 0x50)
       xmm6 <- peek (castPtr p `plusPtr` 0x60)
       xmm7 <- peek (castPtr p `plusPtr` 0x70)
       return  ( X64SSERegisters1
               { x64Xmm0 = xmm0
               , x64Xmm1 = xmm1
               , x64Xmm2 = xmm2
               , x64Xmm3 = xmm3
               , x64Xmm4 = xmm4
               , x64Xmm5 = xmm5
               , x64Xmm6 = xmm6
               , x64Xmm7 = xmm7 } )

x64PeekSSERegisters2 :: Ptr Word64 -> IO X64SSERegisters2
x64PeekSSERegisters2 p =
    do xmm8 <- peek (castPtr p)
       xmm9 <- peek (castPtr p `plusPtr` 0x10)
       xmmA <- peek (castPtr p `plusPtr` 0x20)
       xmmB <- peek (castPtr p `plusPtr` 0x30)
       xmmC <- peek (castPtr p `plusPtr` 0x40)
       xmmD <- peek (castPtr p `plusPtr` 0x50)
       xmmE <- peek (castPtr p `plusPtr` 0x60)
       xmmF <- peek (castPtr p `plusPtr` 0x70)
       return  ( X64SSERegisters2
               { x64Xmm8 = xmm8
               , x64Xmm9 = xmm9
               , x64XmmA = xmmA
               , x64XmmB = xmmB
               , x64XmmC = xmmC
               , x64XmmD = xmmD
               , x64XmmE = xmmE
               , x64XmmF = xmmF } )

x64PeekSSEState :: Ptr Word64 -> X64Registers -> IO X64Registers
x64PeekSSEState p regs =
    do fpState <- x64PeekFPState p
       mmxRegs <- x64PeekMMXRegisters (p `plusPtr` 0x20)
       sseRegs1 <- x64PeekSSERegisters1 (p `plusPtr` 0xA0)
       sseRegs2 <- x64PeekSSERegisters2 (p `plusPtr` 0x120)

       return ( regs
              { x64SSERegisters1 = sseRegs1
              , x64SSERegisters2 = sseRegs2
              , x64MMXRegisters = mmxRegs
              , x64FPState = fpState } )
