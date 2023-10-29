module SampleSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

spec_sample :: Spec
spec_sample = do
  describe "test-test" $ do
    it "test01" $ 0 `shouldBe` 0
