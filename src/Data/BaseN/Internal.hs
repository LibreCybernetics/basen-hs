{-# LANGUAGE FlexibleInstances #-}

module Data.BaseN.Internal where

import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List       as L
import qualified Data.Text       as T

--
-- Type Classes
--

class ByteStringLike b where
  null   :: b -> Bool
  uncons :: b -> Maybe (Word8, b)
  (!!)   :: Integral i => b -> i -> Maybe Word8

instance ByteStringLike [Word8] where
  null   = L.null
  uncons = L.uncons
  l !! i | i `boundedBy` L.length l = Just (l L.!! fromIntegral i)
         | otherwise = Nothing

instance ByteStringLike BS.ByteString where
  null   = BS.null
  uncons = BS.uncons
  bs !! i | i `boundedBy` BS.length bs = Just (bs `BS.index` fromIntegral i)
          | otherwise = Nothing

instance ByteStringLike LBS.ByteString where
  null   = LBS.null
  uncons = LBS.uncons
  bs !! i | i `boundedBy` LBS.length bs = Just(bs `LBS.index` fromIntegral i)
          | otherwise = Nothing

class Concat s where
  concat :: [s] -> s

instance Concat String where
  concat = L.concat

instance Concat BS.ByteString where
  concat = BS.concat

instance Concat LBS.ByteString where
  concat = LBS.concat

instance Concat T.Text where
  concat = T.concat

--
-- Helper Functions
--

-- | Bounded between 0 and a given i. [0, i)
boundedBy :: (Integral idx, Integral len) => idx -> len -> Bool
idx `boundedBy` len = 0 <= idx' && idx' < len
  where idx' = fromIntegral idx
