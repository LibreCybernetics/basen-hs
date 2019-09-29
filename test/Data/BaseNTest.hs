module Data.BaseNTest where

import Test.Tasty.Hspec
import Test.QuickCheck.Instances.ByteString ()

import Data.BaseN

import Data.ByteString
import Data.Word

--
-- Specs
--

spec_examples :: Spec
spec_examples = do
  describe "Edge Cases" $ do
    it "Empty Base2" $ do
      encodeBase2 ([] :: [Word8]) `shouldBe` ""
      decodeBase2 "" `shouldBe` Right ([] :: [Word8])
    it "Malformed Base2" $ do
      decodeBase2 "0" `shouldBe` (Left (WrongLength 7) :: Either DecodeError [Word8])
      decodeBase2 "000011110000" `shouldBe`(Left (WrongLength 4) :: Either DecodeError [Word8])
      decodeBase2 "01234567" `shouldBe` (Left UnknownAlphabet :: Either DecodeError [Word8])
  describe "Base2 Spec" $ do
    testEnc2 [0] "00000000"
    testEnc2 [1] "00000001"
    testEnc2 [10] "00001010"
    testEnc2 [123] "01111011"
    testEnc2 [0,0] "0000000000000000"
    testEnc2 [213,231] "1101010111100111"
  where
    testEnc2 i s = it (show i) $ encodeBase2 (i :: [Word8]) `shouldBe` s

--
-- Properties
--

prop_decodeBase2IsLeftInverseOfencodeBase2 :: ByteString -> Bool
prop_decodeBase2IsLeftInverseOfencodeBase2 input =
  decodeBase2 encoded == Right input
  where
    encoded :: String
    encoded = encodeBase2 input
