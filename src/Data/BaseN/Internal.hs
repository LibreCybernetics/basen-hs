{-# LANGUAGE FlexibleInstances #-}

module Data.BaseN.Internal where

import Data.Word

import Prelude hiding (drop, length, take)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List   as L
import qualified Data.String as S
import qualified Data.Text   as T

--
-- Type Classes
--

-- ByteStringLike

class ByteStringLike b where
  cons   :: Word8 -> b -> b
  empty  :: b
  null   :: b -> Bool
  uncons :: b -> Maybe (Word8, b)
  (!!)   :: Integral i => b -> i -> Maybe Word8

instance ByteStringLike [Word8] where
  cons   = (:)
  empty  = []
  null   = L.null
  uncons = L.uncons
  l !! i | i `boundedBy` L.length l = Just (l L.!! fromIntegral i)
         | otherwise = Nothing

instance ByteStringLike BS.ByteString where
  cons   = BS.cons
  empty  = BS.empty
  null   = BS.null
  uncons = BS.uncons
  bs !! i | i `boundedBy` BS.length bs = Just (bs `BS.index` fromIntegral i)
          | otherwise = Nothing

instance ByteStringLike LBS.ByteString where
  cons   = LBS.cons
  empty  = LBS.empty
  null   = LBS.null
  uncons = LBS.uncons
  bs !! i | i `boundedBy` LBS.length bs = Just(bs `LBS.index` fromIntegral i)
          | otherwise = Nothing

--  StringLike

class S.IsString s => StringLike s where
  concat   :: [s] -> s
  drop     :: Int -> s -> s
  length   :: s -> Int
  take     :: Int -> s -> s
  toString :: s -> String

instance StringLike String where
  concat   = L.concat
  drop     = L.drop
  length   = L.length
  take     = L.take
  toString = id

instance StringLike T.Text where
  concat   = T.concat
  drop     = T.drop
  length   = T.length
  take     = T.take
  toString = T.unpack

--
-- Helper Functions
--

-- | Bounded between 0 and a given i. [0, i)
boundedBy :: (Integral idx, Integral len) => idx -> len -> Bool
idx `boundedBy` len = 0 <= idx' && idx' < len
  where idx' = fromIntegral idx

inChunksOf :: StringLike s => s -> Int -> [s]
s `inChunksOf` n | length s > n = take n s : (drop n s `inChunksOf` n)
                 | length s > 0 = [s]
                 | otherwise    = []


-- Taken from: https://github.com/pmlodawski/error-util
-- License: MIT
infixl 4 <?>
(<?>) :: Maybe b -> a -> Either a b
val <?> m = maybe (Left m) Right val
