{-# LANGUAGE FlexibleInstances #-}

module Data.BaseN (
  Base(..), DecodeError(..),
  encodeBaseN, decodeBaseN,
  encodeBase2, decodeBase2
) where

import Data.BaseN.Internal

import Prelude hiding (concat, length, seq, tail, take)

import Data.Bits
import Data.Word

import qualified Data.List   as L
import qualified Data.String as S

--
-- Type Classes / Data Types
--

data Base = Base2 deriving (Eq, Show)
data DecodeError = UnkownAlphabet | WrongLength Int deriving (Eq, Show)

--
-- Constants
--

base2Alphabet = ['0', '1']
base8Alphabet = ['0'..'7']
base16Alphabet = ['0'..'9'] <> ['a'..'f']

--
-- Functions
--

encodeBase2 :: (ByteStringLike b, StringLike s) => b -> s
encodeBase2 = (`encodeBaseN` Base2)

decodeBase2 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase2 = (`decodeBaseN` Base2)

encodeBaseN :: (ByteStringLike b, StringLike s) => b -> Base -> s
encodeBaseN seq base = case base of
  Base2 -> seq `encodeBase2N` 2

decodeBaseN :: (StringLike s, ByteStringLike b) => s -> Base -> Either DecodeError b
decodeBaseN seq base = case base of
  Base2 -> seq `decodeBase2N` 2

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
    val  <- attemptSum [positionValue base base2Alphabet vals | vals <- zip (toString cnk) [7,6..0]] <?> UnkownAlphabet
    tail <- decodeBase2N' t base
    Right $ cons val tail
  _ -> undefined

attemptSum :: [Maybe Int] -> Maybe Word8
attemptSum l = fromIntegral . sum <$> sequence l
positionValue base baseAlphabet (v, e) = (* base^e) <$> L.elemIndex v baseAlphabet
