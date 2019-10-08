{-# LANGUAGE FlexibleInstances #-}

module Data.BaseN (
  Base(..), Casing(..), DecodeError(..), Padding(..), -- Useful Data Types
  encodeBaseN,  decodeBaseN, -- Generic Encode/Decode
  encodeBase2,  decodeBase2, -- Aliases
  encodeBase8,  decodeBase8,
  encodeBase10, decodeBase10,
  encodeBase16, decodeBase16
) where

import Data.BaseN.Internal

import Prelude hiding (concat, drop, length, null, seq, tail, take, takeWhile, (!!))

import Control.Applicative hiding (empty)
import Data.Bits
import Data.Maybe
import Data.Word

import qualified Data.List   as L
import qualified Data.String as S

--
-- Type Classes / Data Types
--

-- | Available Base Codecs
data Base        = Base2 | Base8  Padding | Base10 | Base16 Casing | Base32 Casing Padding
                 | Base58 deriving (Eq, Show)
-- | Casing Option if applicable
data Casing      = LowerCase | UpperCase        deriving (Eq, Show)
-- | Padding Option if applicable
data Padding     = NoPadding | WithPadding Char deriving (Eq, Show)

-- | Decode Error (Unknown Char in encoded text or ambiguous length)
data DecodeError = UnknownAlphabet | WrongLength Int deriving (Eq, Show)

--
-- Constants
--

-- | If not provided, use `=` as the default padding option
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
base16AlphabetUpper :: [Char]
base16AlphabetUpper = ['0'..'9'] <> ['A'..'F']
-- RFC 4648 Base32 alphabet
base32Alphabet :: [Char]
base32Alphabet = ['A'..'Z'] <> ['2'..'9']
base32AlphabetLower :: [Char]
base32AlphabetLower = ['a'..'z'] <> ['2'..'9']
-- Bitcoin Base58
base58Alphabet :: [Char]
base58Alphabet = undefined

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
encodeBase16 = (`encodeBaseN` Base16 LowerCase)

-- This will first attempt to decode a lowercase string and fallback to an uppercase string
decodeBase16 :: (StringLike s, ByteStringLike b) => s -> Either DecodeError b
decodeBase16 = (`decodeBaseN` Base16 LowerCase) <> (`decodeBaseN` Base16 UpperCase)

--
-- Main Functions
--

encodeBaseN :: (ByteStringLike b, StringLike s) => b -> Base -> s
encodeBaseN seq base = case base of
  Base2   -> seq `encodeBase2N` Base2
  Base8 p -> seq `encodeBase2N` Base8 p
  Base10 -> encodeOtherBase seq 10 base10Alphabet
  Base16 c   -> seq `encodeBase2N` Base16 c
  Base32 c p -> seq `encodeBase2N` Base32 c p
  Base58 -> encodeOtherBase seq 58 base58Alphabet

decodeBaseN :: (StringLike s, ByteStringLike b) => s -> Base -> Either DecodeError b
decodeBaseN seq base = case base of
  Base2   -> seq `decodeBase2N` Base2
  Base8 p -> seq `decodeBase2N` Base8 p
  Base10 -> decodeOtherBase seq 10 base10Alphabet
  Base16 c   -> seq `decodeBase2N` Base16 c
  Base32 c p -> seq `decodeBase2N` Base32 c p
  Base58 -> decodeOtherBase seq 58 base58Alphabet

--
-- Helper functions
--

-- Encoding/Decoding 2^n bases is done is a `map` fashion since it is natural to the representation of data

encodeBase2N :: (ByteStringLike b, StringLike s) => b -> Base -> s
encodeBase2N seq base = case base of
  Base2   -> result 2 base2Alphabet
  Base8 p -> result 8 base8Alphabet
  Base16 LowerCase -> result 16 base16Alphabet
  Base16 UpperCase -> result 16 base16AlphabetUpper
  _  -> undefined
  where
    result baseValue alphabet = concat [S.fromString [alphabet L.!! min val 7 | val <- chunk] | chunk <- encodeBase2N' seq baseValue]

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
                           slice a b i = (`shiftR` fromIntegral a) $ ((2^b) - (2^a)) .&. i
    _ -> undefined
  Nothing        -> []

decodeBase2N :: (StringLike s, ByteStringLike b) => s -> Base -> Either DecodeError b
decodeBase2N seq base = case base of
  Base2   | lenCongBy 8 [0] -> decodeBase2N' (seq `inChunksOf` 8) 2
          | otherwise -> Left . WrongLength $ 8 - (length seq `mod` 8)
  Base8 p | lenCongBy 8 [0, 3, 6] -> decodeBase2N' (seq `inChunksOf` 8) 8
          | otherwise -> Left . WrongLength . foldr min 3 . filter (>0) $ ((\x -> x - (length seq `mod` 8)) <$> [3, 6, 8])
  _ -> undefined
  where lenCongBy d cong = (length seq `mod` d) `L.elem` cong

decodeBase2N' :: (StringLike s, ByteStringLike b) => [s] -> Int -> Either DecodeError b
decodeBase2N' []      _    = Right empty
decodeBase2N' (cnk:t) base = case base of
  2 -> do
    val  <- attemptSum [positionValue 2 base2Alphabet vals | vals <- zip (toString cnk) [7,6..0]] <?> UnknownAlphabet
    tail <- decodeBase2N' t base
    Right $ cons (fromIntegral val) tail
  8 -> case t of
    _ | length cnk' == 8 -> do
           byte1' <- fromIntegral <$> byte1 <?> UnknownAlphabet
           byte2' <- fromIntegral <$> byte2 <?> UnknownAlphabet
           byte3' <- fromIntegral <$> byte3 <?> UnknownAlphabet
           t' <- decodeBase2N' t 8
           Right . (byte1' `cons`) . (byte2' `cons`) . (byte3' `cons`) $ t'
    [] | length cnk' == 6 -> do
           byte1' <- fromIntegral <$> byte1 <?> UnknownAlphabet
           byte2' <- fromIntegral <$> byte2 <?> UnknownAlphabet
           Right . (byte1' `cons`) . (byte2' `cons`) $ empty
    [] | length cnk' == 3 -> do
           byte1' <- fromIntegral <$> byte1 <?> UnknownAlphabet
           Right . (byte1' `cons`) $ empty
    _ -> Left UnknownAlphabet
    where
      cnk'  = takeWhileC (/= '=') cnk
      byte1 = (`shiftR` 1) <$> attemptSum [positionValue 8 base8Alphabet vals | vals <- zip (toString . take 3          $ cnk') [2,1,0]]
      byte2 = (`shiftR` 2) <$> attemptSum [positionValue 8 base8Alphabet vals | vals <- zip (toString . take 4 . drop 2 $ cnk') [3,2..0]]
      byte3 =                  attemptSum [positionValue 8 base8Alphabet vals | vals <- zip (toString . take 3 . drop 5 $ cnk') [2,1,0]]
  _ -> undefined

-- Other encodings (those not of a 2^n base) shouldn't be used for data since they are _very_ inefficient.

encodeOtherBase :: (ByteStringLike b, StringLike s) => b -> Int -> [Char] -> s
encodeOtherBase seq base alphabet = case uncons seq of
  Nothing -> empty
  Just _  -> S.fromString $ padding <> [alphabet L.!! fromIntegral ((totalValue seq' `div` base'^i) `rem` base') | i <- range]
  where
    base' :: Integer
    base' = fromIntegral base
    seq' = dropWhileW (== 0) seq
    totalValue :: (ByteStringLike b) => b -> Integer
    totalValue bs = case uncons bs of
      Nothing     -> 0
      Just (h, t) -> (fromIntegral h * (256 ^ length t)) + totalValue t
    largestPower :: Int
    largestPower = case totalValue seq' of
      0 -> -1 -- Magic number, signals value o since log(0) is undefined
      _ -> floor $ logBase (fromIntegral base) (fromIntegral $ totalValue seq')
    range :: [Int]
    range = case largestPower of
      -1 -> []
      0  -> [0]
      i | i > 0 -> [largestPower,(largestPower-1)..0]
    padding :: [Char]
    padding = replicate (length . takeWhileW (== 0) $ seq) '0'

decodeOtherBase :: (StringLike s, ByteStringLike b) => s -> Int -> [Char] -> Either DecodeError b
decodeOtherBase seq base alphabet | null seq  = Right empty
                                  | otherwise = Right unpadding
  where
    unpadding = pack $ replicate (length . takeWhileC (=='0') $ seq) 0
