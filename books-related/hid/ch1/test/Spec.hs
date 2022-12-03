import Test.Hspec

import Stems
import Control.Applicative (Alternative(empty))
import Stems (value)

main :: IO ()
main = hspec $ do
  describe "comp" $ do
    it "proper comp" $ do
      comp (Stem "ab")  (Stem "abc") `shouldBe` Just (Stem "c")
    it "improper comp" $ do
      comp (Stem "ab")  (Stem "ab") `shouldBe` Just (Stem [])
    it "fail comp" $ do
      comp (Stem "ab")  (Stem "a") `shouldBe` Nothing
    it "proper comp bad order" $ do
      comp (Stem "ba")  (Stem "abc") `shouldBe` Nothing
      
  describe "superStem" $ do
    it "returns `True` if the second param is super stem of the first" $ do
      superStem (Stem "a") (Stem "ab") `shouldBe` True
    it "returns `True` if the first and the second params are 'equal'" $ do
      superStem (Stem "a") (Stem "a") `shouldBe` True
    it "returns `False` if the first param is (proper) super stem of the first" $ do
      superStem (Stem "ab") (Stem "a") `shouldBe` False
    it "returns `False` if the second param is super stem of a prumtation of the first" $ do
      superStem (Stem "ba") (Stem "abc") `shouldBe` False

  describe "dist" $ do
    it "computes the dist properly" $ do
      dist (Stem "ab") (Stem "abc") `shouldBe` Just 1
    it "does not compute the dist if the second param is not a super stem" $ do      
      dist (Stem "ba") (Stem "abc") `shouldBe` Nothing
    it "computes the empty dist properly" $ do
      dist (Stem "abc") (Stem "abc") `shouldBe` Just 0


  describe "subs" $ do
    it "finds subs (improper)" $ do
      subs (Stem "st") (Population [Stem "st", Stem "star", Stem "start"]) `shouldBe` Population [Stem "st", Stem "star", Stem "start"]
    it "finds subs (proper)" $ do
      subs (Stem "st") (Population [Stem "other", Stem "st", Stem "star", Stem "start"]) `shouldBe` Population [Stem "st", Stem "star", Stem "start"]
  
    it "does not find subs" $ do
      subs (Stem "st") (Population [Stem "other"]) `shouldBe` Population []
    it "does not find subs (empty)" $ do
      subs (Stem "st") (Population []) `shouldBe` Population []
  
  describe "value" $ do
    it "does not compute value" $ do            
      value (Population [Stem "star", Stem "stop", Stem "street"]) (Stem "a") (Stem "st") `shouldBe` Nothing
    it "computes value" $ do      
      value (Population [Stem "star", Stem "stop", Stem "street"]) (Stem "s") (Stem "st") `shouldBe` Just 1
    it "computes value#2" $ do      
      value (Population [Stem "not match", Stem "star", Stem "stop", Stem "street"]) (Stem "s") (Stem "st") `shouldBe` Just 1
  
    it "computes value with empty start" $ do
      value (Population [Stem "star", Stem "stop", Stem "street"]) (Stem "") (Stem "st") `shouldBe` Just 2
  
  
