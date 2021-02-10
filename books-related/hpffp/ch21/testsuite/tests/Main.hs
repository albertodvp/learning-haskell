module Main where
import           Lib
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [pure Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq


main :: IO ()
main = do
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int])))


