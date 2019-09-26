module Data.BaseNTest where

import Test.Tasty.Hspec

import Data.BaseN

import Data.Word

spec_examples :: Spec
spec_examples = do
  describe "Edge Cases" $ do
    it "Empty Base2" $
      encodeBaseN ([] :: [Word8]) Base2 `shouldBe` ""
  describe "Base2 Spec" $ do
    testEnc2 [0] "00000000"
    testEnc2 [1] "00000001"
    testEnc2 [10] "00001010"
    testEnc2 [123] "01111011"
    testEnc2 [0,0] "0000000000000000"
    testEnc2 [213,231] "1101010111100111"
  where
    testEnc2 i s = it (show i) $ encodeBaseN (i :: [Word8]) Base2 `shouldBe`s
