import Lib

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary (Cmp f g a) where
  arbitrary = undefined

instance EqProp (Cmp f g a) where
  (=-=) (Cmp fga) (Cmp fgb) = undefined


main :: IO()
main = do
  -- quickBatch $ functor (undefined :: Cmp Maybe (Either String) (Int, String, Char) )
  -- quickBatch $ applicative (undefined :: Cmp Maybe (Either String) (Int, String, Char) )
