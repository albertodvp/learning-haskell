-- |
module ParseIPSpec where

import           Test.Tasty
import           Test.Tasty.Hspec

spec_buildIP :: Spec
spec_buildIP =
  describe "buildIP" $ do
    it "builds from zero" $
      buildIP [0,0,0,0] `shouldBe` (IP 0)
