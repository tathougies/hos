{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Applicative
import Control.Exception

import Data.Bits
import Data.Word
import Data.Attoparsec.Number(Number(I, D))
import Data.Binary
import Data.Traversable (sequenceA)
import Data.Yaml
import Data.HashMap.Strict (toList)
import qualified Data.Text as T

import Hos.Common.Bundle

import System.Environment
import System.Exit
import System.IO
import System.Posix.Files

pageSize :: Integral a => a
pageSize = 0x1000

alignToPage :: (Num a, Bits a, Integral a) => a -> a
alignToPage x = (x + pageSize - 1) .&. complement (pageSize - 1)

data ValidationResult l = ValidBundle [TagDescriptor] (Bundle l)

newtype TagsAndBundle = TagsAndBundle ([TagDescriptor], Bundle FilePath)

parseBundleSpec :: FilePath -> IO ([TagDescriptor], Bundle FilePath)
parseBundleSpec fp = do res <- decodeFile fp
                        case res of
                          Just (TagsAndBundle tagsAndBundle) -> return tagsAndBundle
                          Nothing -> do putStrLn "Failed to parse"
                                        exitWith (ExitFailure 1)

validateBundle :: [TagDescriptor] -> Bundle l -> ValidationResult l
validateBundle tagDescs items = ValidBundle tagDescs items

embedFile :: Handle -> FilePath -> Word64 -> IO ()
embedFile fh fileToEmbed fileSz =
    do embedH <- openBinaryFile fileToEmbed ReadMode
       let copyBytes = do isEof <- hIsEOF embedH
                          if isEof then return () else hGetChar embedH >>= hPutChar fh >> copyBytes
       copyBytes
       hClose embedH
       replicateM_ (fromIntegral (alignToPage fileSz - fileSz)) (hPutChar fh '\0')

getFileSize :: FilePath -> IO Word64
getFileSize fp = bracket (openBinaryFile fp ReadMode) hClose $ \h -> fromIntegral <$> hFileSize h

main :: IO ()
main = getArgs >>= \args ->
       case args of
         [fileName, outputFileName] ->
             do (tags, fileBundle) <- parseBundleSpec fileName
                fileSizes <- mapM (\bundleItem -> alignToPage <$> getFileSize (biLocation bundleItem)) (bundleContents fileBundle)
                let bundle = Bundle $
                             zipWith (\offset fileBundle -> fileBundle { biLocation = offset })
                                         (zip (scanl (+) (0 :: Word64) fileSizes) fileSizes)
                                         (bundleContents fileBundle)
                case validateBundle tags (bundle :: Bundle (Word64, Word64)) of
                  ValidBundle tags bundle ->
                      do fh <- openBinaryFile outputFileName WriteMode
                         headerLen <- withSerialized (tags, bundle) $ \ptr length ->
                                      do hPutBuf fh ptr length
                                         return length
                         replicateM_ (alignToPage headerLen - headerLen) (hPutChar fh '\0')
                         -- Now we're aligned to a page boundary, so now just copy each file in
                         forM_ (zip (bundleContents fileBundle) fileSizes) $ \(BundleItem { biLocation = fileName }, fileSz) ->
                             embedFile fh fileName fileSz
                         hClose fh
         _ -> do procName <- getProgName
                 putStrLn (procName ++ ": usage")
                 putStrLn ("  " ++ procName ++ " <bundle-spec.yaml> <output.bundle>")
                 exitWith (ExitFailure 1)

newtype TagDescriptors = TagDescriptors { unTagDescriptors :: [TagDescriptor] }

instance FromJSON TagsAndBundle where
    parseJSON (Object v) = TagsAndBundle <$> ((,) <$> (unTagDescriptors <$> v .: "tags") <*> v .: "content")
    parseJSON _ = mzero

instance FromJSON TagDescriptors where
    parseJSON (Object v) = TagDescriptors <$> sequenceA (map parseTagDescriptor (toList v))
        where parseTagDescriptor (name, Object v) =
                  TagDescriptor (T.unpack name)
                  <$> (v .:? "description" .!= "")
                  <*> (v .:? "default")
                  <*> (v .:? "unique" .!= False)
              parseTagDescriptor _ = mzero
    parseJSON _ = mzero

instance FromJSON a => FromJSON (Bundle a) where
    parseJSON v = Bundle <$> parseJSON v

instance FromJSON a => FromJSON (BundleItem a) where
    parseJSON (Object v) = BundleItem (GUID 0 0 0 0) <$> v .: "tags" <*> v .: "location"
    parseJSON _ = mzero

instance FromJSON Tag where
    parseJSON (Object v) = case toList v of
                             [(key, value)] -> Tag (TagName (T.unpack key)) <$> parseJSON value
                             _ -> mzero
    parseJSON (String t) = pure (Tag (TagName (T.unpack t)) (BooleanV True))
    parseJSON _ = mzero

instance FromJSON TagValue where
    parseJSON (String t) = pure (TextV (T.unpack t))
    parseJSON (Bool b) = pure (BooleanV b)
    parseJSON (Number (I i)) = pure (IntegerV i)
    parseJSON (Number (D d)) = pure (RationalV d)
