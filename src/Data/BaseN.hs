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

import Control.Applicative hiding (empty)
import Data.Bits
import Data.Maybe
import Data.Word

import qualified Data.List   as L
import qualified Data.String as S

--
-- Type Classes / Data Types
--

data Base = Base2 | Base8  Padding | Base10 | Base16 Casing Padding deriving (Eq, Show)
data Casing  = LowerCase | UpperCase deriving (Eq, Show)
data Padding = NoPadding | WithPadding Char deriving (Eq, Show)
data DecodeError = UnknownAlphabet | WrongLength Int deriving (Eq, Show)

--
-- Constants
--

defaultPadding :: Padding
defaultPadding = WithPadding '='

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
encodeBase8 = (`encodeBaseN` Base8 defaultPadding)

decodeBase8 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase8 = (`decodeBaseN` Base8 defaultPadding)

encodeBase10 :: (ByteStringLike b, StringLike s) => b -> s
encodeBase10 = (`encodeBaseN` Base10)

decodeBase10 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase10 = (`decodeBaseN` Base10)

encodeBase16 :: (ByteStringLike b, StringLike s) => b -> s
encodeBase16 = (`encodeBaseN` Base16 LowerCase defaultPadding)

decodeBase16 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase16 = (`decodeBaseN` Base16 LowerCase defaultPadding)

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
    result alphabet = concat [S.fromString [alphabet L.!! min val 7 | val <- chunk] | chunk <- encodeBase2N' seq base]

encodeBase2N' :: (ByteStringLike b) => b -> Int -> [[Int]]
encodeBase2N' seq base = case uncons seq of
  Just (h, seq') -> case base of
    2 -> [if h `testBit` i then 1 else 0 | i <- [7,6..0] :: [Int]] : encodeBase2N' seq' base
    -- | Base8 Encoding; This code is hacky and should be refactored.
    8 -> (fromIntegral <$> catMaybes [slice 5 8 <$> (seq !! 0),
                                      slice 2 5 <$> (seq !! 0),
                                      if ((`testBit` 7) <$> (seq !! 1)) `contains` True
                                      then (1 +) . (2 *) . slice 0 2 <$> (seq !! 0)
                                      else         (2 *) . slice 0 2 <$> (seq !! 0),
                                      slice 4 7 <$> (seq !! 1),
                                      slice 1 4 <$> (seq !! 1),
                                      if ((`testBit` 0) <$> (seq !! 1)) `contains` True
                                      then (4 +) . slice 6 8 <$> ((seq !! 2) <|> Just 0)
                                      else if isJust $ seq !! 1
                                      then slice 6 8 <$> ((seq !! 2) <|> Just 0)
                                      else Nothing,
                                      slice 3 6 <$> (seq !! 2),
                                      slice 0 3 <$> (seq !! 2)
                                     ]) : encodeBase2N' (drop 3 seq) base
                         where
                           slice :: Word8 -> Word8 -> Word8 -> Word8
                           slice a b i = (`shiftR` fromIntegral a) $ ((2^b) - (2^a) :: Word8) .&. i
    _ -> undefined
  Nothing        -> []

decodeBase2N :: (StringLike s, ByteStringLike b) => s -> Int -> Either DecodeError b
decodeBase2N seq base = case base of
  2 | lenCongBy 8 [0] -> decodeBase2N' (seq `inChunksOf` 8) 2
    | otherwise -> Left . WrongLength $ 8 - (length seq `mod` 8)
  8 | lenCongBy 8 [0, 3, 6] -> decodeBase2N' (seq `inChunksOf` 8) 8
    -- | More hacky code that needs refactoring
    | otherwise -> Left . WrongLength . foldr min 3 . filter (>0) $ ((\x -> x - (length seq `mod` 8)) <$> [3, 6, 8])
  _ -> undefined
  where lenCongBy d cong = (length seq `mod` d) `L.elem` cong

decodeBase2N' :: (StringLike s, ByteStringLike b) => [s] -> Int -> Either DecodeError b
decodeBase2N' []      _    = Right empty
decodeBase2N' (cnk:t) base = case base of
  2 -> do
    val  <- attemptSum [positionValue base base2Alphabet vals | vals <- zip (toString cnk) [7,6..0]] <?> UnknownAlphabet
    tail <- decodeBase2N' t base
    Right $ cons val tail
  _ -> undefined
