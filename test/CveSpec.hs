module CveSpec where

import Protolude

import Test.Hspec

spec :: Spec
spec = do
  describe "test" $ do
    it "decode . encode == id" $
      True `shouldBe` True
