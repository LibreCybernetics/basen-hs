{-# LANGUAGE FlexibleInstances #-}

module Data.BaseN.Internal where

import Data.Word
import Data.Maybe

import Prelude hiding (drop, length, null, take)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as L
import qualified Data.String          as S
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT

--
-- Type Classes
--

-- SeqLike

class SeqLike s where
  empty  :: s
  null   :: s -> Bool
  length :: s -> Int
  drop   :: Int -> s -> s
  take   :: Int -> s -> s
  concat :: [s] -> s

instance SeqLike [e] where
  empty  = []
  null   = L.null
  length = L.length
  drop   = L.drop
  take   = L.take
  concat = L.concat

instance SeqLike BS.ByteString where
  empty  = BS.empty
  null   = BS.null
  length = BS.length
  drop   = BS.drop
  take   = BS.take
  concat = BS.concat

instance SeqLike LBS.ByteString where
  empty  = LBS.empty
  null   = LBS.null
  length = fromIntegral . LBS.length
  drop   = LBS.drop . fromIntegral
  take   = LBS.take . fromIntegral
  concat = LBS.concat

instance SeqLike T.Text where
  empty  = T.empty
  null   = T.null
  length = T.length
  drop   = T.drop
  take   = T.take
  concat = T.concat

instance SeqLike LT.Text where
  empty  = LT.empty
  null   = LT.null
  length = fromIntegral . LT.length
  drop   = LT.drop . fromIntegral
  take   = LT.take . fromIntegral
  concat = LT.concat

-- ByteStringLike

class SeqLike b => ByteStringLike b where
  cons   :: Word8 -> b -> b
  uncons :: b -> Maybe (Word8, b)
  (!!)   :: b -> Int -> Maybe Word8

instance ByteStringLike [Word8] where
  cons   = (:)
  uncons = L.uncons
  l !! i | i `boundedBy` L.length l = Just (l L.!! fromIntegral i)
         | otherwise = Nothing

instance ByteStringLike BS.ByteString where
  cons   = BS.cons
  uncons = BS.uncons
  bs !! i | i `boundedBy` BS.length bs = Just (bs `BS.index` fromIntegral i)
          | otherwise = Nothing

instance ByteStringLike LBS.ByteString where
  cons   = LBS.cons
  uncons = LBS.uncons
  bs !! i | i `boundedBy` LBS.length bs = Just(bs `LBS.index` fromIntegral i)
          | otherwise = Nothing

--  StringLike

class (SeqLike s, S.IsString s) => StringLike s where
  takeWhile :: (Char -> Bool) -> s -> s
  toString  :: s -> String

instance StringLike String where
  takeWhile = L.takeWhile
  toString  = id

instance StringLike T.Text where
  takeWhile = T.takeWhile
  toString  = T.unpack

instance StringLike LT.Text where
  takeWhile = LT.takeWhile
  toString  = LT.unpack

--
-- Helper Functions (Not exported as part of the package)
--

attemptSum :: [Maybe Int] -> Maybe Word16
attemptSum l = fromIntegral . sum <$> sequence l

-- | Bounded between 0 and a given i. [0, i)
boundedBy :: (Integral idx, Integral len) => idx -> len -> Bool
idx `boundedBy` len = 0 <= idx' && idx' < len
  where idx' = fromIntegral idx

inChunksOf :: StringLike s => s -> Int -> [s]
s `inChunksOf` n | length s > n = take n s : (drop n s `inChunksOf` n)
                 | not (null s) = [s]
                 | otherwise    = []


positionValue :: Int -> [Char] -> (Char, Int) -> Maybe Int
positionValue base baseAlphabet (v, e) = (* base^e) <$> L.elemIndex v baseAlphabet

-- Taken from: https://github.com/pmlodawski/error-util
-- License: MIT
infixl 4 <?>
(<?>) :: Maybe b -> a -> Either a b
val <?> m = maybe (Left m) Right val

contains :: (Eq a) => Maybe a -> a -> Bool
contains m v = case m of
  Just v' -> v' == v
  Nothing -> False
