{-# LANGUAGE CPP #-}
module Hos.Common.Types where

import Control.Applicative

import Data.Word
import Data.Monoid
import Data.Bits
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Foreign.Ptr
import Foreign.Storable

#ifndef __GLASGOW_HASKELL__
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
infixr 6 <>
#endif

newtype ChanId = ChanId Word32
    deriving (Show, Read, Eq, Ord)
newtype TaskId = TaskId Word32
    deriving (Show, Read, Eq, Ord)
newtype TaskPriority = TaskPriority Int
    deriving (Show, Eq, Ord)
newtype AddressSpaceRef = AddressSpaceRef Word32
    deriving (Show, Read, Eq, Ord)

curAddressSpaceRef :: AddressSpaceRef
curAddressSpaceRef = AddressSpaceRef maxBound

data MemoryPermissions = Privileged ReadWrite
                       | UserSpace ReadWrite
                         deriving (Show, Eq, Ord)

data WaitOnChannelsFlags = WaitOnChannelsFlags
                         { wocWaitForever :: Bool
                         , wocDontTruncate :: Bool
                         , wocAllChannels :: Bool }
                           deriving Show

data ReadWrite = ReadOnly | ReadWrite
               deriving (Show, Eq, Ord, Enum)

data MessageType = Outgoing MessageOrReply
                 | Incoming MessageOrReply
                   deriving (Show)

data MessageOrReply = MessageFrom ChanId
                    | ReplyTo ChanId
                      deriving (Show)

-- TODO JHC generates a defective Eq instance for types like message type: sum types where both constructors carry the same subtype
instance Eq MessageType where
    Outgoing a == Outgoing b = a == b
    Incoming a == Incoming b = a == b
    _ == _ = False

instance Eq MessageOrReply where
    MessageFrom a == MessageFrom b = a == b
    ReplyTo a == ReplyTo b = a == b
    _ == _ = False

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

             | Message MessageType (Seq (Maybe Word64))

             | CopyOnWrite MemoryPermissions Word64

               -- These can only be added or manipulated by the kernel, but can be read from userspace, under certain conditions
             | Mapped MemoryPermissions Word64
               deriving (Show, Eq)

instance Monoid WaitOnChannelsFlags where
    mempty = WaitOnChannelsFlags False False False
    mappend a b = WaitOnChannelsFlags
                  (wocWaitForever a || wocWaitForever b)
                  (wocDontTruncate a || wocDontTruncate b)
                  (wocAllChannels a || wocAllChannels b)

waitForever, dontTruncate, allChannels :: WaitOnChannelsFlags
waitForever = WaitOnChannelsFlags True False False
dontTruncate = WaitOnChannelsFlags False True False
allChannels = WaitOnChannelsFlags False False True

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

    poke p (Message (Outgoing msgDest) _) =
        poke (castPtr p :: Ptr Word8) 0x5 >>
        poke (castPtr p `plusPtr` 1) msgDest
    poke p (Message (Incoming msgDest) _) =
        poke (castPtr p :: Ptr Word8) 0x6 >>
        poke (castPtr p `plusPtr` 1) msgDest

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

                  0x5 -> Message <$> (Outgoing <$> peek (castPtr p `plusPtr` 1)) <*> pure Seq.empty
                  0x6 -> Message <$> (Incoming <$> peek (castPtr p `plusPtr` 1)) <*> pure Seq.empty

                  0xFF -> Mapped <$> peek (castPtr p `plusPtr` 1)
                                 <*> peek (castPtr p `plusPtr` 2)
                  0x7F -> CopyOnWrite <$> peek (castPtr p `plusPtr` 1)
                                      <*> peek (castPtr p `plusPtr` 2)

instance Storable MessageOrReply where
    sizeOf _ = 4
    alignment _ = 4

    poke p (MessageFrom (ChanId chanId)) = poke (castPtr p) chanId
    poke p (ReplyTo (ChanId chanId)) = poke (castPtr p) (chanId `setBit` 31)
    peek p = do chanId <- peek (castPtr p)
                return (if testBit chanId 31 then ReplyTo (ChanId (chanId `clearBit` 31)) else MessageFrom (ChanId chanId))
