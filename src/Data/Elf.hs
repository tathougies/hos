module Data.Elf where

import Data.Word

import Control.Monad

import Foreign.Ptr
import Foreign.Storable

data ElfType = EtNone
             | EtRel
             | EtExec
             | EtDyn
             | EtCore
             | EtUnknown
               deriving Show

data ElfVersion = EvNone
                | EvCurrent
                | EvUnknown
                  deriving Show

data ElfMachine = EmNone
                | EmI386
                | EmX86_64
                | EmSPARC
                | EmPPC
                | EmPPC64
                | EmARM
                | EmAArch64
                | EmUnknown
                  deriving Show

data Elf64Hdr = Elf64Hdr
              { e64Type    :: ElfType
              , e64Machine :: ElfMachine
              , e64Version :: ElfVersion
              , e64Entry   :: Word64
              , e64PhOff   :: Word64
              , e64ShOff   :: Word64
              , e64Flags   :: Word32
              , e64EhSize  :: Word16
              , e64PhEntSize :: Word16
              , e64PhNum   :: Word16
              , e64ShEntSize :: Word16
              , e64ShNum   :: Word16
              , e64ShStrNdx :: Word16 }
                deriving Show

data ElfProgHdrType = PtNull
                    | PtLoad
                    | PtDynamic
                    | PtInterp
                    | PtNote
                    | PtShLib
                    | PtPhdr
                    | PtTls
                    | PtUnknown
                      deriving Show

data Elf64ProgHdr = Elf64ProgHdr
                  { ph64Type :: ElfProgHdrType
                  , ph64Flags :: Word32
                  , ph64Offset :: Word64
                  , ph64VAddr :: Word64
                  , ph64PAddr :: Word64
                  , ph64FileSz :: Word64
                  , ph64MemSz :: Word64
                  , ph64Align :: Word64 }
                    deriving Show

instance Storable Elf64Hdr where
    sizeOf _ = 48
    alignment _ = 16

    poke p _ = error "Unimplemented"
    peek p' = do let p = p' `plusPtr` 16 -- skip the eident
                 ty <- peek (castPtr p)
                 mach <- peek ((castPtr p) `plusPtr` 2)
                 version <- peek ((castPtr p) `plusPtr` 4)
                 entry <- peek ((castPtr p) `plusPtr` 8)
                 phOff <- peek ((castPtr p) `plusPtr` 16)
                 shOff <- peek ((castPtr p) `plusPtr` 24)
                 flags <- peek ((castPtr p) `plusPtr` 32)
                 ehSize <- peek ((castPtr p) `plusPtr` 36)
                 phEntSize <- peek ((castPtr p) `plusPtr` 38)
                 phNum <- peek ((castPtr p) `plusPtr` 40)
                 shEntSize <- peek ((castPtr p) `plusPtr` 42)
                 shNum <- peek ((castPtr p) `plusPtr` 44)
                 shStrNdx <- peek ((castPtr p) `plusPtr` 46)
                 let toElfType 0 = EtNone
                     toElfType 1 = EtRel
                     toElfType 2 = EtExec
                     toElfType 3 = EtDyn
                     toElfType 4 = EtCore
                     toElfType _ = EtUnknown

                     toElfVersion 0 = EvNone
                     toElfVersion 1 = EvCurrent
                     toElfVersion _ = EvUnknown

                     toElfMachine 0 = EmNone
                     toElfMachine 2 = EmSPARC
                     toElfMachine 3 = EmI386
                     toElfMachine 20 = EmPPC
                     toElfMachine 21 = EmPPC64
                     toElfMachine 40 = EmARM
                     toElfMachine 62 = EmX86_64
                     toElfMachine 183 = EmAArch64
                     toElfMachine _ = EmUnknown
                 return (Elf64Hdr (toElfType (ty :: Word16))
                                  (toElfMachine (mach :: Word16))
                                  (toElfVersion (version :: Word32))
                                  entry
                                  phOff
                                  shOff
                                  flags
                                  ehSize
                                  phEntSize
                                  phNum
                                  shEntSize
                                  shNum
                                  shStrNdx)

instance Storable Elf64ProgHdr where
    sizeOf _ = 56
    alignment _ = 16

    poke p _ = error "Unimplemented"
    peek p = do ty <- peek (castPtr p)
                flags <- peek ((castPtr p) `plusPtr` 4)
                offs <- peek ((castPtr p) `plusPtr` 8)
                vAddr <- peek ((castPtr p) `plusPtr` 16)
                pAddr <- peek ((castPtr p) `plusPtr` 24)
                fileSz <- peek ((castPtr p) `plusPtr` 32)
                memSz <- peek ((castPtr p) `plusPtr` 40)
                align <- peek ((castPtr p) `plusPtr` 48)
                let toProgHdrType 0 = PtNull
                    toProgHdrType 1 = PtLoad
                    toProgHdrType 2 = PtDynamic
                    toProgHdrType 3 = PtInterp
                    toProgHdrType 4 = PtNote
                    toProgHdrType 5 = PtShLib
                    toProgHdrType 6 = PtPhdr
                    toProgHdrType 7 = PtTls
                    toProgHdrType _ = PtUnknown
                return (Elf64ProgHdr (toProgHdrType (ty :: Word32)) flags offs vAddr pAddr fileSz memSz align)

elf64ProgHdrs :: Ptr Elf64Hdr -> IO (Elf64Hdr, [Elf64ProgHdr])
elf64ProgHdrs elf64Hdr =
    do hdr <- peek elf64Hdr
       let progHdrP = elf64Hdr `plusPtr` (fromIntegral (e64PhOff hdr))
       progHdrs <- forM [0..(fromIntegral (e64PhNum hdr) - 1)] $ \i -> peek (castPtr progHdrP `plusPtr` (fromIntegral (e64PhEntSize hdr) * i))
       return (hdr, progHdrs)

elfShouldLoad :: ElfProgHdrType -> Bool
elfShouldLoad PtLoad = True
elfShouldLoad _ = False
