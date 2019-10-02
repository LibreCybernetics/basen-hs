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
      encodeBase2 ([] :: [Word8]) `shouldBe` ""
      encodeBase8 ([] :: [Word8]) `shouldBe` ""
      decodeBase2 "" `shouldBe` Right ([] :: [Word8])
      decodeBase8 "" `shouldBe` Right ([] :: [Word8])
    it "Malformed Base2" $ do
      decodeBase2 "0" `shouldBe` (Left (WrongLength 7) :: Result)
      decodeBase2 "000011110000" `shouldBe`(Left (WrongLength 4) :: Result)
      decodeBase2 "01234567" `shouldBe` (Left UnknownAlphabet :: Result)
    it "Malformed Base8" $ do
      decodeBase8 "0" `shouldBe` (Left (WrongLength 2) :: Result)
      decodeBase8 "0123456701234" `shouldBe` (Left (WrongLength 1) :: Result)
      decodeBase8 "01234567890" `shouldBe` (Left UnknownAlphabet :: Result)
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
  where
    testEnc2 = testEncGen encodeBase2
    testEnc8 = testEncGen encodeBase8
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
