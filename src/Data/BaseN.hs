{-# LANGUAGE FlexibleInstances #-}

module Data.BaseN (
  Base(..), Casing(..), DecodeError(..),
  encodeBaseN,  decodeBaseN,
  encodeBase2,  decodeBase2,
  encodeBase8,  decodeBase8,
  encodeBase10, decodeBase10,
  encodeBase16, decodeBase16
) where

import Data.BaseN.Internal

import Prelude hiding (concat, length, seq, tail, take)

import Data.Bits

import qualified Data.List   as L
import qualified Data.String as S

--
-- Type Classes / Data Types
--

data Base = Base2 | Base8 | Base10 | Base16 Casing deriving (Eq, Show)
data Casing = LowerCase | UpperCase deriving (Eq, Show)
data DecodeError = UnknownAlphabet | WrongLength Int deriving (Eq, Show)

--
-- Constants
--

base2Alphabet  :: [Char]
base2Alphabet  = ['0', '1']
base8Alphabet  :: [Char]
base8Alphabet  = ['0'..'7']
base10Alphabet :: [Char]
base10Alphabet = ['0'..'9']
base16Alphabet :: [Char]
base16Alphabet = ['0'..'9'] <> ['a'..'f']

--
-- Alias Functions
--

encodeBase2 :: (ByteStringLike b, StringLike s) => b -> s
encodeBase2 = (`encodeBaseN` Base2)

decodeBase2 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase2 = (`decodeBaseN` Base2)

encodeBase8 :: (ByteStringLike b, StringLike s) => b -> s
encodeBase8 = (`encodeBaseN` Base8)

decodeBase8 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase8 = (`decodeBaseN` Base2)

encodeBase10 :: (ByteStringLike b, StringLike s) => b -> s
encodeBase10 = (`encodeBaseN` Base10)

decodeBase10 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase10 = (`decodeBaseN` Base10)

encodeBase16 :: (ByteStringLike b, StringLike s) => b -> s
encodeBase16 = (`encodeBaseN` Base8)

decodeBase16 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase16 = (`decodeBaseN` Base2)

--
-- Main Functions
--

encodeBaseN :: (ByteStringLike b, StringLike s) => b -> Base -> s
encodeBaseN seq base = case base of
  Base2  -> seq `encodeBase2N` 2
  Base8  -> seq `encodeBase2N` 8
  Base10 -> undefined
  Base16 c -> seq `encodeBase2N` 16

decodeBaseN :: (StringLike s, ByteStringLike b) => s -> Base -> Either DecodeError b
decodeBaseN seq base = case base of
  Base2  -> seq `decodeBase2N` 2
  Base8  -> seq `decodeBase2N` 8
  Base10 -> undefined
  Base16 c -> seq `decodeBase2N` 16

--
-- Helper function
--

encodeBase2N :: (ByteStringLike b, StringLike s) => b -> Int -> s
encodeBase2N seq base = concat
  [S.fromString [base2Alphabet L.!! val | val <- chunk] | chunk <- encodeBase2N' seq base]

encodeBase2N' :: (ByteStringLike b) => b -> Int -> [[Int]]
encodeBase2N' seq base = case uncons seq of
  Just (h, seq') -> case base of
    2 -> [if h `testBit` i then 1 else 0 | i <- [7,6..0] :: [Int]] : encodeBase2N' seq' base
    _ -> undefined
  Nothing        -> []

decodeBase2N :: (StringLike s, ByteStringLike b) => s -> Int -> Either DecodeError b
decodeBase2N seq base = case base of
  2 | lenDivBy 8 -> decodeBase2N' (seq `inChunksOf` 8) 2
    | otherwise  -> Left . WrongLength $ 8 - (length seq `mod` 8)
  _ -> undefined
  where lenDivBy d = length seq `rem` d == 0

decodeBase2N' :: (StringLike s, ByteStringLike b) => [s] -> Int -> Either DecodeError b
decodeBase2N' []      _    = Right empty
decodeBase2N' (cnk:t) base = case base of
  2 -> do
    val  <- attemptSum [positionValue base base2Alphabet vals | vals <- zip (toString cnk) [7,6..0]] <?> UnknownAlphabet
    tail <- decodeBase2N' t base
    Right $ cons val tail
  _ -> undefined
