module Main where
import           Lib
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [pure Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))


