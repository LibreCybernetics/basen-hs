{-# LANGUAGE FlexibleInstances #-}

module Data.BaseN (Base(..), baseNEncode) where

import Data.BaseN.Internal

import Prelude hiding (concat)

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

