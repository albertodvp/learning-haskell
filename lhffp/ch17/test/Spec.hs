import Control.Applicative
import Test.QuickCheck
import Data.Monoid
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import ZipList
import List


instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

--instance Arbitrary a => Arbitrary (Sum a) where
--  arbitrary = Sum <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [ (5, liftA2 Cons arbitrary arbitrary),
                          (1, return Nil)
                        ]

              
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


main :: IO ()
main = do
  quickBatch $ monoid (Nil :: List (Sum Int))
  quickBatch $ monoid (ZipList' Nil :: ZipList' (List (Sum Int)))
  quickBatch $ functor (undefined :: List (String, Char, Integer))
  quickBatch $ applicative (undefined :: List (String, Char, Integer))
  quickBatch $ functor (undefined :: ZipList' (String, Char, Integer))
  quickBatch $ applicative (undefined :: ZipList' (String, Char, Integer))

 
