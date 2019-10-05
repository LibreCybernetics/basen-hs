module Data.BaseNTest where

import Test.Tasty.Hspec
import Test.QuickCheck.Instances.ByteString ()

import Data.BaseN

import Data.ByteString
import Data.Word
import Data.Text

--
-- Specs
--

spec_examples :: Spec
spec_examples = do
  describe "Edge Cases" $ do
    it "Empty" $ do
      goodEnc encodeBase2  [] ""
      goodEnc encodeBase8  [] ""
      goodEnc encodeBase10 [] ""
      goodDec decodeBase2  "" []
      goodDec decodeBase8  "" []
      goodDec decodeBase10 "" []
    it "Malformed Base2" $ do
      badDec decodeBase2 "0" $ WrongLength 7
      badDec decodeBase2 "000011110000" $ WrongLength 4
      badDec decodeBase2 "01234567" UnknownAlphabet
    it "Malformed Base8" $ do
      badDec decodeBase8 "0" $ WrongLength 2
      badDec decodeBase8 "0123456701234" $ WrongLength 1
      badDec decodeBase8 "01234567890" UnknownAlphabet
  describe "Base2 Spec" $ do
    testEnc2 [0] "00000000"
    testEnc2 [1] "00000001"
    testEnc2 [10] "00001010"
    testEnc2 [123] "01111011"
    testEnc2 [0,0] "0000000000000000"
    testEnc2 [213,231] "1101010111100111"
  describe "Base8 Spec" $ do
    testEnc8 [0] "000"
    testEnc8 [1] "002"
    testEnc8 [10] "024"
    testEnc8 [123] "366"
    testEnc8 [0,0] "000000"
    testEnc8 [213, 231] "653634"
    testEnc8 [0, 10, 20, 30, 40, 50] "0000502407424062"
  describe "Base10 Spec" $ do
    testEnc10 [0] "0"
    testEnc10 [1] "1"
    testEnc10 [10] "10"
    testEnc10 [123] "123"
    testEnc10 [0,0] "00"
    testEnc10 [213,231] "54759"
  where
    goodEnc c b t   = c (b :: [Word8]) `shouldBe` t
    goodDec c t b   = c t `shouldBe` (Right b :: Result)
    badDec  c t err = c t `shouldBe` (Left err :: Result)
    testEnc2  = testEncGen encodeBase2
    testEnc8  = testEncGen encodeBase8
    testEnc10 = testEncGen encodeBase10
    testEncGen enc i s = it (show i) $ enc (i :: [Word8]) `shouldBe` s

--
-- Properties
--

prop_decodeBase2IsLeftInverseOfencode :: ByteString -> Bool
prop_decodeBase2IsLeftInverseOfencode input =
  decodeBase2 encBase2 == Right input &&
  decodeBase8 encBase8 == Right input
  where
    encBase2 :: Text
    encBase2 = encodeBase2 input
    encBase8 :: Text
    encBase8 = encodeBase8 input

--
-- Helpers
--

type Result = Either DecodeError [Word8]
