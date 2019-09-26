module Data.BaseNTest where

import Test.Tasty.Hspec

import Data.BaseN

import Data.Word

spec_examples :: Spec
spec_examples = do
  describe "Edge Cases" $ do
    it "Empty Base2" $
      baseNEncode ([] :: [Word8]) Base2 `shouldBe` ""
  describe "Base2 Spec" $ do
    testEnc [0] Base2 "00000000"
    testEnc [1] Base2 "00000001"
    testEnc [10] Base2 "00001010"
    testEnc [123] Base2 "01111011"
    testEnc [0,0] Base2 "0000000000000000"
    testEnc [213,231] Base2 "1101010111100111"
  where
    testEnc i b s = it (show i) $ baseNEncode (i :: [Word8]) b `shouldBe`s
