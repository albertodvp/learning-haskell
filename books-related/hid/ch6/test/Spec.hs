import           Test.Hspec

import           Expr

main :: IO ()
main = hspec $ do
  describe "Test Expr" $ do
    it "a literal should be parsed" $ do
      parse "42" `shouldBe` Lit 42

    it "a literal with spaces should be parsed" $ do
      parse "   42 " `shouldBe` Lit 42

    it "a literal with parethesis should be parsed" $ do
      parse "(42)" `shouldBe` Lit 42

    it "a literal with parethesis and space should be parsed" $ do
      parse " ( 42 ) " `shouldBe` Lit 42

    it "an addition should be parsed" $ do
      parse "21+21" `shouldBe` Add (Lit 21) (Lit 21)

    it "an multiplication should be parsed" $ do
      parse "2*21" `shouldBe` Mult (Lit 2) (Lit 21)

    it "an \"complex\" equation should be parsed" $ do
      parse "(1 + 1) * 21" `shouldBe` Add (Add (Lit 1) (Lit 1)) (Lit 21)

    it "an \"\"very\" complex\" equation should be parsed" $ do
      parse "1 * (2 + 0) * (1 + 20)" `shouldBe` Mult (Mult (Lit 1) (Add (Lit 2) (Lit 0))) (Add (Lit 1) (Lit 20))
