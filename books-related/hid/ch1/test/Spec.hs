import Test.Hspec

import Stems

main :: IO ()
main = hspec $ do
  describe "comp" $ do
    it "proper comp" $ do
      comp (Stem [1,2])  (Stem [1, 2, 3]) `shouldBe` (Just $ Stem [3])
    it "improper comp" $ do
      comp (Stem [1,2])  (Stem [1, 2]) `shouldBe` (Just $ Stem [])
    it "fail comp" $ do
      comp (Stem [1,2])  (Stem [1]) `shouldBe` Nothing
    it "proper comp bad order" $ do
      comp (Stem [2,1])  (Stem [1, 2, 3]) `shouldBe` Nothing
      
  describe "superStem" $ do
    it "returns `True` if the second param is super stem of the first" $ do
      superStem (Stem [1]) (Stem [1,2]) `shouldBe` True
    it "returns `True` if the first and the second params are 'equal'" $ do
      superStem (Stem [1]) (Stem [1]) `shouldBe` True
    it "returns `False` if the first param is (proper) super stem of the first" $ do
      superStem (Stem [1,2]) (Stem [1]) `shouldBe` False
    it "returns `False` if the second param is super stem of a prumtation of the first" $ do
      superStem (Stem [2,1]) (Stem [1,2,3]) `shouldBe` False


  

