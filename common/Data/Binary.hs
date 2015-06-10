{-# LANGUAGE BangPatterns, CPP #-}
module Data.Binary where

import Hos.Common.Types

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad

import Data.Monoid
import Data.Word
import Data.Char
import Data.Functor

import Debug.Trace

import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr

-- | A builder is a function that takes a pointer and writes a certain number of bytes to that location
data Builder = Builder (Ptr () -> IO ()) !Word64
newtype Get a = Get { doGet :: Ptr () -> Word64 -> IO (Either String (a, Ptr (), Word64)) }

instance Monoid Builder where
    mempty = Builder (const (return ())) 0
    mappend (Builder a aSz) (Builder b bSz) =
        Builder ab (aSz + bSz)
        where ab ptr = do a ptr
                          b (ptr `plusPtr` fromIntegral aSz)

storableB :: Storable a => a -> Builder
storableB w = Builder (\p -> poke (castPtr p) w) (fromIntegral (sizeOf w))

word8B :: Word8 -> Builder
word8B = storableB

word16B :: Word16 -> Builder
word16B = storableB

word32B :: Word32 -> Builder
word32B = storableB

word64B :: Word64 -> Builder
word64B = storableB

asciiStringB :: String -> Builder
asciiStringB s = listB word8B (map (fromIntegral . ord) s)

listB :: (a -> Builder) -> [a] -> Builder
listB itemB items = word64B (fromIntegral (length items)) <> mconcat (map itemB items)

enumB :: (Enum a, Integral b) => (b -> Builder) -> a -> Builder
enumB intB = intB . fromIntegral . fromEnum

class Binary a where
    put :: a -> Builder
    get :: Get a

withSerialized :: Binary a => a -> (Ptr () -> Word64 -> IO b) -> IO b
withSerialized a cont =
    let Builder mkA aSz = put a
    in bracket (mallocBytes (fromIntegral aSz)) free $ \p ->
        do mkA p
           cont p aSz

serializeTo :: Binary a => Ptr () -> Word64 -> a -> IO ()
serializeTo p sz a =
    let Builder putA aSz = put a
    in if aSz > sz then fail "need more space" else putA p

instance (Binary a, Binary b) => Binary (a, b) where
    put (a, b) = put a <> put b
    get = get `gBind` \a ->
          get `gBind` \b ->
          gReturn (a, b)

instance Binary a => Binary [a] where
    put xs = listB put xs
    get = (get :: Get Word64) `gBind` \l ->
          getList l []
          where getList 0 a = gReturn a
                getList n a = get `gBind` \x -> getList (n - 1) (a ++ [x])

instance Binary a => Binary (Maybe a) where
    put Nothing = word8B 0
    put (Just x) = word8B 1 <> put x
    get = (get :: Get Word8) `gBind` \tag ->
          case tag of
            0 -> gReturn Nothing
            1 -> get `gBind` (gReturn . Just)

instance Binary Word8 where
    put = word8B
    get = getStorable
instance Binary Word16 where
    put = word16B
    get = getStorable
instance Binary Word32 where
    put = word32B
    get = getStorable
instance Binary Word64 where
    put = word64B
    get = getStorable
instance Binary Bool where
    put True = word8B 1
    put False = word8B 0
    get = (get :: Get Word8) `gBind`
          (\b ->
           case b of
             0 -> gReturn False
             _ -> gReturn True)
instance Binary Char where
    put = word8B . fromIntegral . ord
    get = (get :: Get Word8) `gBind` (gReturn . chr . fromIntegral)

getStorable :: Storable a => Get a
getStorable = injectA $ \a -> Get (doGet a)
    where doGet a p sz
              | sz >= fromIntegral (sizeOf a) =
                  do a <- peek (castPtr p)
                     return (Right (a, p `plusPtr` sizeOf a, sz - fromIntegral (sizeOf a)))
              | otherwise = return (Left "Not enough space to retrieve storable")

          injectA :: (a -> Get a) -> Get a
          injectA f = f undefined

gReturn :: x -> Get x
gReturn x = Get $ \p sz -> return (Right (x, p, sz))

gBind :: Get a -> (a -> Get b) -> Get b
gBind a b = Get $ \p sz -> doGet a p sz >>= \aRes ->
                           case aRes of
                               Right (a, p', sz') -> doGet (b a) p' sz'
                               Left err -> return (Left err)

gFail :: String -> Get a
gFail x = Get $ \_ _ -> return (Left x)

-- instance Functor Get where
--     fmap f aM = aM >>= return . f

-- instance Applicative Get where
--     pure = return
--     f <*> x = do f' <- f
--                  x' <- x
--                  return (f' x')
getFrom :: Binary a => Ptr () -> Word64 -> IO (Either String a)
getFrom p sz = doGet get p sz >>= \res ->
               case res of
                 Left err -> return (Left err)
                 Right (a, _, _) -> return (Right a)
