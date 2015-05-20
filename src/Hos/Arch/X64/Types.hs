module Hos.Arch.X64.Types where

import Hos.Types

import Data.Word

import Foreign.Ptr
import Foreign.Storable

data X64Privilege = Ring0 | Ring1 | Ring2 | Ring3 deriving (Show, Read, Eq, Ord, Enum)

data X64Registers = X64Registers
                  { x64GPRegisters :: X64GPRegisters
                    -- Broken up because JHC doesn't like large structures (?)
                  , x64SSERegisters1 :: X64SSERegisters1
                  , x64SSERegisters2 :: X64SSERegisters2 }
                  deriving (Show, Eq, Ord)

data WordSSE = WordSSE
             { sseLo :: !Word64
             , sseHi :: !Word64 }
             deriving (Show, Eq, Ord)

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
                    , x64GpRflags :: !Word64}
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

x64EmptyRegs :: X64Registers
x64EmptyRegs = X64Registers emptyGpRegisters emptySSERegisters1 emptySSERegisters2
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

x64PageSize :: Word
x64PageSize = 4096

-- given a task, and a region of memory, store the task state in that region
-- This region can then be passed on to switchToUserspace in context.S
x64PokeTaskState :: Task X64Registers vMemTbl -> Ptr a -> IO ()
x64PokeTaskState (Task { taskReasonLeft = reasonLeft
                       , taskSavedRegisters = regs }) p =
  do poke (castPtr p :: Ptr Word64) (fromIntegral (fromEnum reasonLeft))
     poke (castPtr (p `plusPtr` 8) :: Ptr X64Registers) regs

instance Storable X64Registers where
    sizeOf _ = 208
    alignment _ = 16

    poke ptr (X64Registers { x64GPRegisters = gp }) =
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
         return (x64EmptyRegs
                 { x64GPRegisters =
                   X64GPRegisters
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
                   , x64GpRflags = rflags } } )

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
