{-# LANGUAGE FlexibleInstances #-}

module Data.BaseN (Base(..), encodeBaseN) where

import Data.BaseN.Internal

import Prelude hiding (concat, seq)

import Data.Bits

import qualified Data.List   as L
import qualified Data.String as S

--
-- Type Classes / Data Types
--

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

encodeBase2 :: (ByteStringLike b, S.IsString s, Concat s) => b -> s
encodeBase2 = (`encodeBaseN` Base2)

encodeBaseN :: (ByteStringLike b, S.IsString s, Concat s) => b -> Base -> s
encodeBaseN seq base = case base of
  Base2 -> seq `encodeBase2N` 2

--
-- Helper function
--

encodeBase2N :: (ByteStringLike b, S.IsString s, Concat s) => b -> Int -> s
encodeBase2N seq base = concat
  [S.fromString [base2Alphabet L.!! val | val <- chunk] | chunk <- encodeBase2N' seq base]

encodeBase2N' :: (ByteStringLike b) => b -> Int -> [[Int]]
encodeBase2N' seq base = case uncons seq of
  Just (h, seq') -> case base of
    2 -> [if h `testBit` i then 1 else 0 | i <- [7,6..0] :: [Int]] : encodeBase2N' seq' base
    _ -> undefined
  Nothing        -> []

decodeBase2N :: (S.IsString s, ByteStringLike b) => s -> Int -> b
decodeBase2N = undefined
