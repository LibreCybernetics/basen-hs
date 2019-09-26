{-# LANGUAGE FlexibleInstances #-}

module Data.BaseN (Base(..), baseNEncode) where

import Prelude hiding (concat)

import Data.Bits
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List       as L
import qualified Data.String     as S
import qualified Data.Text       as T

--
-- Type Classes / Data Types
--

class ByteStringLike b where
  uncons :: b -> Maybe (Word8, b)
  (!!)   :: Integral i => b -> i -> Maybe Word8

instance ByteStringLike [Word8] where
  uncons = L.uncons
  l !! i | i `boundedBy` L.length l = Just (l L.!! fromIntegral i)
         | otherwise = Nothing

instance ByteStringLike BS.ByteString where
  uncons = BS.uncons
  bs !! i | i `boundedBy` BS.length bs = Just (bs `BS.index` fromIntegral i)
          | otherwise = Nothing

instance ByteStringLike LBS.ByteString where
  uncons = LBS.uncons
  bs !! i | i `boundedBy` LBS.length bs = Just(bs `LBS.index` fromIntegral i)
          | otherwise = Nothing

class S.IsString s => IsStringConcat s where
  concat :: [s] -> s

instance IsStringConcat String where
  concat = L.concat

instance IsStringConcat BS.ByteString where
  concat = BS.concat

instance IsStringConcat LBS.ByteString where
  concat = LBS.concat

instance IsStringConcat T.Text where
  concat = T.concat

data Base = Base2

--
-- Constants
--

base2Alphabet = ['0', '1']
base8Alphabet = ['0'..'7']
base16Alphabet = ['0'..'9'] <> ['a'..'f']

--
-- Functions
--

baseNEncode :: (ByteStringLike b, IsStringConcat s) => b -> Base -> s
baseNEncode seq base = case base of
  Base2 -> seq `base2NEncode` 2

--
-- Helper function
--

base2NEncode :: (ByteStringLike b, IsStringConcat s) => b -> Int -> s
base2NEncode seq base = concat
  [S.fromString [base2Alphabet L.!! val | val <- chunk] | chunk <- base2NEncode' seq base]

base2NEncode' :: (ByteStringLike b) => b -> Int -> [[Int]]
base2NEncode' seq base = case uncons seq of
  Just (h, seq') -> case base of
    2 -> [if h `testBit` i then 1 else 0 | i <- [7,6..0] :: [Int]] : base2NEncode' seq' base
    _ -> undefined
  Nothing        -> []

-- | Bounded between 0 and a given i. [0, i)
boundedBy :: (Integral idx, Integral len) => idx -> len -> Bool
idx `boundedBy` len = 0 <= idx' && idx' < len
  where idx' = fromIntegral idx
