import Test.Hspec

import Stems
import Control.Applicative (Alternative(empty))
import Data.Text (pack)
import Stems

main :: IO ()
main = hspec $ do
  describe "cleanLine" $ do
    it "cleans line substituting special characters and lower case" $ do
      cleanLine (pack "Exeunt. Marching: after the which, a Peale of Ordenance are shot") `shouldBe` "exeunt  marching  after the which  a peale of ordenance are shot"
  
  describe "getStems" $ do
    it "creates stems cleaning text" $ do
      getStems (pack "Exeunt. Marching: after the which, a Peale of Ordenance are shot\r\noff.\r\n\r\n\r\nFINIS. The tragedie of HAMLET, Prince of Denmarke.\r\n") `shouldBe` [Stem "exeunt", Stem "marching", Stem "after", Stem "the", Stem "which", Stem "a", Stem "peale", Stem "of", Stem "ordenance", Stem "are", Stem "shot", Stem "off", Stem "finis", Stem "the", Stem"tragedie", Stem "of", Stem "hamlet", Stem "prince", Stem "of", Stem "denmarke"]
    
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
      value (Population [Stem "star", Stem "stop", Stem "street"]) (Stem "s") (Stem "st") `shouldBe` Just 14
    it "computes value#2" $ do      
      value (Population [Stem "not match", Stem "star", Stem "stop", Stem "street"]) (Stem "s") (Stem "st") `shouldBe` Just 14

    it "computes value#3" $ do      
      value (Population [Stem "star"]) (Stem "s") (Stem "st") `shouldBe` Just 4
  
  
    it "computes value with empty start" $ do
      value (Population [Stem "star", Stem "stop", Stem "street"]) (Stem "") (Stem "st") `shouldBe` Just 28
  
  
  describe "query" $ do
    it "computes the 'n' most valuable super stems" $ do
      query 2 (Population [Stem "start", Stem "sum", Stem "stop", Stem "other"]) (Stem "s") `shouldBe` [Stem "sum", Stem "stop"]

    it "computes the at-most 'n' most valuable super stems" $ do
      query 10 (Population [Stem "start", Stem "sum", Stem "stop", Stem "other"]) (Stem "s") `shouldBe` [Stem "sum", Stem "stop", Stem "start"]


    it "computes the at-most 'n' most valuable super stems (empty)" $ do
      query 10 (Population [Stem "start", Stem "sum", Stem "stop", Stem "other"]) (Stem "") `shouldBe` [Stem "sum", Stem "stop", Stem "start", Stem "other"]
