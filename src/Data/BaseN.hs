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

import Prelude hiding (concat, drop, length, seq, tail, take, (!!))

import Data.Bits
import Data.Maybe

import qualified Data.List   as L
import qualified Data.String as S

--
-- Type Classes / Data Types
--

data Base = Base2 | Base8  Padding | Base10 | Base16 Casing Padding deriving (Eq, Show)
data Casing  = LowerCase | UpperCase deriving (Eq, Show)
data Padding = NoPadding | WithPadding deriving (Eq, Show)
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
encodeBase8 = (`encodeBaseN` Base8 WithPadding)

decodeBase8 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase8 = (`decodeBaseN` Base8 WithPadding)

encodeBase10 :: (ByteStringLike b, StringLike s) => b -> s
encodeBase10 = (`encodeBaseN` Base10)

decodeBase10 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase10 = (`decodeBaseN` Base10)

encodeBase16 :: (ByteStringLike b, StringLike s) => b -> s
encodeBase16 = (`encodeBaseN` Base16 LowerCase WithPadding)

decodeBase16 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase16 = (`decodeBaseN` Base16 LowerCase WithPadding)

--
-- Main Functions
--

encodeBaseN :: (ByteStringLike b, StringLike s) => b -> Base -> s
encodeBaseN seq base = case base of
  Base2      -> seq `encodeBase2N` 2
  Base8 p    -> seq `encodeBase2N` 8
  Base10     -> undefined
  Base16 c p -> seq `encodeBase2N` 16

decodeBaseN :: (StringLike s, ByteStringLike b) => s -> Base -> Either DecodeError b
decodeBaseN seq base = case base of
  Base2      -> seq `decodeBase2N` 2
  Base8 p    -> seq `decodeBase2N` 8
  Base10     -> undefined
  Base16 c p -> seq `decodeBase2N` 16

--
-- Helper function
--

encodeBase2N :: (ByteStringLike b, StringLike s) => b -> Int -> s
encodeBase2N seq base = case base of
  2  -> result base2Alphabet
  8  -> result base8Alphabet
  16 -> result base16Alphabet
  _  -> undefined
  where
    result alphabet = concat [S.fromString [alphabet L.!! val | val <- chunk] | chunk <- encodeBase2N' seq base]

encodeBase2N' :: (ByteStringLike b) => b -> Int -> [[Int]]
encodeBase2N' seq base = case uncons seq of
  Just (h, seq') -> case base of
    2 -> [if h `testBit` i then 1 else 0 | i <- [7,6..0] :: [Int]] : encodeBase2N' seq' base
    8 -> (fromIntegral <$> catMaybes [slice 0 <$> (seq !! 0),
                                      slice 3 <$> (seq !! 0),
                                      if ((`testBit` 0) <$> (seq !! 1)) `contains` True
                                      then (1 +) . (2 *) . slice 6 <$> (seq !! 0)
                                      else         (2 *) . slice 6 <$> (seq !! 0),
                                      slice 1 <$> (seq !! 1),
                                      slice 4 <$> (seq !! 1),
                                      if ((`testBit` 7) <$> (seq !! 1)) `contains` True
                                      then (4 +) . (`shiftR` 6) <$> (seq !! 2)
                                      else         (`shiftR` 6) <$> (seq !! 2),
                                      slice 2 <$> (seq !! 2),
                                      slice 5 <$> (seq !! 2)
                                     ]) : encodeBase2N' (drop 3 seq) base
                         where slice i = (`shiftR` max 5 i) . (`shiftL` i)
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
