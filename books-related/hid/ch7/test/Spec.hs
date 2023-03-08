{-# LANGUAGE OverloadedStrings #-}
import           Lib
import           Test.Hspec

import           Control.Monad.Except
import           Control.Monad.State
import           Lib                  (EvalError (ExtraElements, NotEnoughElements))
main :: IO ()
main = hspec $ do
  describe "Test RPN" $ do
    it "push empty" $ do
      execEvalM [] [] (push 42) `shouldBe` [42]
    it "push" $ do
      execEvalM [] [41] (push 42) `shouldBe` [42, 41]
    it "pop empty" $ do
      evalEvalM [] [] pop `shouldBe` Left NotEnoughElements
    it "pop one from two" $ do
      evalEvalM [] [42, 41] pop `shouldBe` Right 42
    it "check one last element" $ do
      evalEvalM [] [42] oneElementOnStack `shouldBe` Right ()
    it "check one last element, > 1" $ do
      evalEvalM [] [42, 41] oneElementOnStack `shouldBe` Left ExtraElements
    it "check one last element, < 1" $ do
      evalEvalM [] [] oneElementOnStack `shouldBe` Left NotEnoughElements
    it "parseNumber 42" $ do
      evalEvalM [] [] (parseNumber "42") `shouldBe` Right 42
    it "parseNumber abc" $ do
      evalEvalM [] [] (parseNumber "abc") `shouldBe` Left CannotParseNumber
    it "getEnvValue ok" $ do
      evalEvalM [("x", 42)] [] (getEnvValue "x") `shouldBe` Right 42
    it "getEnvValue ko" $ do
      evalEvalM [] [] (getEnvValue "x") `shouldBe` Left (SymbolNotPresentOnEnv "x")
    it "parse ok num" $ do
      evalEvalM [] [] (parse "42") `shouldBe` Right 42
    it "parse ok var" $ do
      evalEvalM [("x", 42)] [] (parse "x") `shouldBe` Right 42
    it "parse ok var" $ do
      evalEvalM [("x", 42)] [] (parse "y") `shouldBe` Left (SymbolNotPresentOnEnv "y")
    it "eval ok 1" $ do
      evalEvalM [("x", 40)] [] (evalPRN "x 2 +") `shouldBe` Right 42
    it "eval ok 2" $ do
      evalEvalM [("x", 40)] [] (evalPRN "x 2 + 2 *") `shouldBe` Right 84
    it "eval ko 1" $ do
      evalEvalM [("x", 40)] [] (evalPRN "x 2") `shouldBe` Left ExtraElements
    it "eval ko 2" $ do
      evalEvalM [("x", 40)] [] (evalPRN "+") `shouldBe` Left NotEnoughElements
