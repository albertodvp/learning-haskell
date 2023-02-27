import           Lib
import           Test.Hspec

import           Control.Monad.Except
import           Control.Monad.State
main :: IO ()
main = hspec $ do
  describe "Test RPN" $ do
    it "push empty" $ do
      execState (push 42) [] `shouldBe` [42]
    it "push" $ do
      execState (push 42) [41] `shouldBe` [42, 41]
    it "pop empty" $ do
      evalState (runExceptT pop) [] `shouldBe` (Left NotEnoughElements)
