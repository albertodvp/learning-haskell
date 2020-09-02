import Control.Applicative
import Test.QuickCheck
import Data.Monoid
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import ZipList
import List
import qualified Validation as V
import FinalExs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [ (5, liftA2 Cons arbitrary arbitrary),
                          (1, return Nil)
                        ]

instance (Arbitrary e, Arbitrary a) => Arbitrary (V.Validation e a) where
  arbitrary = oneof [ V.Success <$> arbitrary,
                      V.Failure <$> arbitrary]

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a'<- arbitrary
    return $ Pair a a'

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = take' 3000 xs
      ys' = take' 3000 ys

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
      where
        xs' = take' 3000 $ getList xs
        ys' = take' 3000 $ getList ys

instance (Eq e, Eq a) => EqProp (V.Validation e a) where
  (=-=) = eq

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


main :: IO ()
main = do
  quickBatch $ monoid (Nil :: List (Sum Int))
  quickBatch $ monoid (ZipList' Nil :: ZipList' (List (Sum Int)))
  quickBatch $ functor (undefined :: List (String, Char, Integer))
  quickBatch $ applicative (undefined :: List (String, Char, Integer))
  quickBatch $ functor (undefined :: ZipList' (String, Char, Integer))
  quickBatch $ applicative (undefined :: ZipList' (String, Char, Integer))
  quickBatch $ functor (undefined :: V.Validation String ((String, Char, Integer)))
  quickBatch $ applicative (undefined :: V.Validation String ((String, Char, Integer)))
  quickBatch $ functor (undefined :: Pair (String, Char, Integer))
  quickBatch $ applicative (undefined :: Pair (String, Char, Integer))
  quickBatch $ functor (undefined :: Two (Sum Int) (String, Char, Integer))
  quickBatch $ applicative (undefined :: Two (Sum Int) (String, Char, Integer))
  quickBatch $ functor (undefined :: Three (Sum Int) String (String, Char, Integer))
  quickBatch $ applicative (undefined :: Three (Sum Int) String (String, Char, Integer))
