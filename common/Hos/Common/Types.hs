module Hos.Common.Types where

import Control.Applicative

import Data.Word
import Data.Bits

import Foreign.Ptr
import Foreign.Storable

newtype TaskId = TaskId Word32
    deriving (Show, Read, Eq, Ord)
newtype TaskPriority = TaskPriority Int
    deriving (Show, Eq, Ord)
newtype AddressSpaceRef = AddressSpaceRef Word32
    deriving (Show, Read, Eq, Ord)

data MemoryPermissions = Privileged ReadWrite
                       | UserSpace ReadWrite
                         deriving (Show, Eq, Ord)

data ReadWrite = ReadOnly | ReadWrite
               deriving (Show, Eq, Ord, Enum)

-- | Some mappings, such as FromPhysical, do not have any sensible forking behavior.
--
--   This data type lets processes specify what should happen to these sorts of mapping
--   when a fork occurs.
data MappingTreatmentOnFork = RetainInParent -- ^ On fork, the parent will keep the region, while the region will be invalid in the child
                            | GiveToChild    -- ^ On fork, the child will be given the region, while the region will be invalid in the parent
                              deriving (Show, Eq, Ord)

data Mapping = AllocateOnDemand MemoryPermissions
             | AllocateImmediately MemoryPermissions (Maybe Word64)
             | FromPhysical MappingTreatmentOnFork MemoryPermissions Word64

             | CopyOnWrite MemoryPermissions Word64

               -- These can only be added or manipulated by the kernel, but can be read from userspace, under certain conditions
             | Mapped MemoryPermissions Word64
               deriving (Show, Eq, Ord)

instance Storable MemoryPermissions where
    sizeOf _ = 1
    alignment _ = 1

    poke p (Privileged ReadWrite) = poke (castPtr p :: Ptr Word8) 0x1
    poke p (Privileged ReadOnly) = poke (castPtr p :: Ptr Word8) 0x0
    poke p (UserSpace ReadWrite) = poke (castPtr p :: Ptr Word8) 0x3
    poke p (UserSpace ReadOnly) = poke (castPtr p :: Ptr Word8) 0x2

    peek p = do tag <- peek (castPtr p :: Ptr Word8)
                return ((if testBit tag 1 then UserSpace else Privileged) (if testBit tag 0 then ReadWrite else ReadOnly))

instance Storable MappingTreatmentOnFork where
    sizeOf _ = 1
    alignment _ = 1

    poke p RetainInParent = poke (castPtr p :: Ptr Word8) 0x0
    poke p GiveToChild = poke (castPtr p :: Ptr Word8) 0x1

    peek p = do tag <- peek (castPtr p :: Ptr Word8)
                case tag of
                  0x0 -> return RetainInParent
                  0x1 -> return GiveToChild

instance Storable Mapping where
    sizeOf _ = 11
    alignment _ = 1

    poke p (AllocateOnDemand perms) =
        poke (castPtr p :: Ptr Word8) 0x1 >>
        poke (castPtr p `plusPtr` 1) perms
    poke p (AllocateImmediately perms Nothing) =
        poke (castPtr p :: Ptr Word8) 0x2 >>
        poke (castPtr p `plusPtr` 1) perms >>
        poke (castPtr p `plusPtr` 2) (0 :: Word64)
    poke p (AllocateImmediately perms (Just alignment)) =
        poke (castPtr p :: Ptr Word8) 0x3 >>
        poke (castPtr p `plusPtr` 1) perms >>
        poke (castPtr p `plusPtr` 2) alignment
    poke p (FromPhysical forkTreatment perms physBase) =
        poke (castPtr p :: Ptr Word8) 0x4 >>
        poke (castPtr p `plusPtr` 1) forkTreatment >>
        poke (castPtr p `plusPtr` 2) perms >>
        poke (castPtr p `plusPtr` 3) physBase

    poke p (Mapped perms pageAddr) =
        poke (castPtr p :: Ptr Word8) 0xFF >>
        poke (castPtr p `plusPtr` 1) perms >>
        poke (castPtr p `plusPtr` 2) pageAddr
    poke p (CopyOnWrite perms pageAddr) =
        poke (castPtr p :: Ptr Word8) 0x7F >>
        poke (castPtr p `plusPtr` 1) perms >>
        poke (castPtr p `plusPtr` 2) pageAddr

    peek p = do tag <- peek (castPtr p :: Ptr Word8)
                case tag of
                  0x1 -> AllocateOnDemand <$> peek (castPtr p `plusPtr` 1)
                  0x2 -> AllocateImmediately <$> peek (castPtr p `plusPtr` 1)
                                             <*> pure Nothing
                  0x3 -> AllocateImmediately <$> peek (castPtr p `plusPtr` 1)
                                             <*> (Just <$> peek (castPtr p `plusPtr` 2))
                  0x4 -> FromPhysical <$> peek (castPtr p `plusPtr` 1)
                                      <*> peek (castPtr p `plusPtr` 2)
                                      <*> peek (castPtr p `plusPtr` 3)

                  0xFF -> Mapped <$> peek (castPtr p `plusPtr` 1)
                                 <*> peek (castPtr p `plusPtr` 2)
                  0x7F -> CopyOnWrite <$> peek (castPtr p `plusPtr` 1)
                                      <*> peek (castPtr p `plusPtr` 2)
