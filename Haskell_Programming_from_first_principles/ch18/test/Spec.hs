import EitherMonad
import BadMonad
import FinalExs

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ fmap return [First a, Second b]

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary =
    CountMe <$> arbitrary <*> arbitrary

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = oneof $ [arbitrary >>= return . PLeft, arbitrary >>= return . PRight]

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

instance EqProp (Nope a) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
      n :: Nope (Int, String, Int)
      n = undefined
      be :: BahEither String (Int, String, Int)
      be = undefined
  quickBatch $ monad (undefined :: Sum String (String, Int, Char))
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  quickBatch $ monad (undefined :: Sum String (String, Int, Char))
  quickBatch $ functor n
  quickBatch $ applicative n
  quickBatch $ monad n
  quickBatch $ functor be
  quickBatch $ applicative n

