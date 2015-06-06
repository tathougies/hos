module Hos.Common.Bundle where

import Control.Applicative

import Data.Word
import Data.Binary
import Data.Monoid

import Numeric

newtype TagName = TagName String
    deriving (Show, Eq, Ord)

data Tag = Tag
         { tTag :: TagName
         , tValue :: TagValue }
         deriving (Show, Eq, Ord)

data TagValue = TextV String
              | IntegerV Integer
              | BooleanV Bool
              | RationalV Double
                deriving (Show, Eq, Ord)

data TagDescriptor = TagDescriptor
                   { tdName :: String
                   , tdDescription :: String
                   , tdDefaultValue :: Maybe TagValue
                   , tdIsUnique :: Bool }
                     deriving (Show, Eq, Ord)

newtype Bundle l = Bundle { bundleContents :: [BundleItem l] }
    deriving (Show, Eq, Ord)
data BundleItem location = BundleItem
                         { biGuid :: GUID
                         , biTags :: [Tag]
                         , biLocation :: location }
                           deriving (Show, Eq, Ord)

data GUID = GUID
          { guid1 :: Word32
          , guid2 :: Word16
          , guid3 :: Word16
          , guid4 :: Word64 }
            deriving (Eq, Ord)

instance Show GUID where
    showsPrec _ (GUID guid1 guid2 guid3 guid4) = showHex guid1 . ('-':) . showHex guid2 . ('-':) . showHex guid3 . ('-':). showHex guid4

instance Binary TagDescriptor where
    put td = mconcat [ put (tdName td)
                     , put (tdDescription td)
                     , put (tdDefaultValue td)
                     , put (tdIsUnique td) ]
    get = get `gBind` \name ->
          get `gBind` \desc ->
          get `gBind` \defVal ->
          get `gBind` \isUnique ->
          gReturn (TagDescriptor name desc defVal isUnique)

instance Binary TagValue where
    put (TextV s) = word8B 1 <> put s
    put (IntegerV s) = word8B 2 <> put (fromIntegral s :: Word64)
    put (BooleanV b) = word8B 3 <> put b
    get = (get :: Get Word8) `gBind` \tag ->
          case tag of
            1 -> get `gBind` (gReturn . TextV)
            2 -> (get :: Get Word64) `gBind` (gReturn . IntegerV . fromIntegral)
            3 -> get `gBind` (gReturn . BooleanV)

instance Binary l => Binary (Bundle l) where
    put (Bundle contents) = put contents
    get = get `gBind` (gReturn . Bundle)

instance Binary l => Binary (BundleItem l) where
    put bi = mconcat [ put (biGuid bi)
                     , put (biTags bi)
                     , put (biLocation bi) ]
    get = get `gBind` \guid ->
          get `gBind` \tags ->
          get `gBind` \loc ->
          gReturn (BundleItem guid tags loc)

instance Binary GUID where
    put (GUID guid1 guid2 guid3 guid4) =
        mconcat [ put guid1
                , put guid2
                , put guid3
                , put guid4 ]
    get = get `gBind` \d1 ->
          get `gBind` \d2 ->
          get `gBind` \d3 ->
          get `gBind` \d4 ->
          gReturn (GUID d1 d2 d3 d4)

instance Binary Tag where
    put t = mconcat [ put (tTag t)
                    , put (tValue t) ]
    get = get `gBind` \name ->
          get `gBind` \val ->
          gReturn (Tag name val)

instance Binary TagName where
    put (TagName s) = put s
    get = get `gBind` (gReturn . TagName)
