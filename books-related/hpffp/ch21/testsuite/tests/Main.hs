module Main where
import           Control.Applicative      (liftA2, liftA3)
import           Data.Monoid
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

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =  frequency [
    (5, pure Nil),
    (1, liftA2 Cons arbitrary arbitrary)
    ]

take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' 0  _          = Nil
take' i (Cons x xs) = Cons x (take' (i-1) xs)


instance Eq a => EqProp (List a) where
  x =-= y = x' `eq` y'
    where
      x' = take' 3000 x
      y' = take' 3000 y

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary


instance (Eq a, Eq b) => EqProp (Pair a b ) where
  (=-=) = eq


instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary


instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance ( Functor n, Arbitrary (n a), Arbitrary a ) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency [
    (4, pure Empty),
    (4, Leaf <$> arbitrary),
    (1, liftA3 Node arbitrary arbitrary arbitrary)
    ]


instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  putStr "Identity"
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
  putStr "Optional"
  quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))
  putStr "Constant"
  quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int])))
  putStr "List"
  quickBatch (traversable (undefined :: List (Int, Int, [Int])))
  putStr "Three"
  quickBatch (traversable (undefined :: Three Int Int (Int, Int, [Int])))
  putStr "Pair"
  quickBatch (traversable (undefined :: Pair Int (Int, Int, [Int])))
  putStr "Big"
  quickBatch (traversable (undefined :: Big Int (Int, Int, [Int])))
  putStr "Bigger"
  quickBatch (traversable (undefined :: Bigger Int (Int, Int, [Int])))
  putStr "S"
  quickBatch (traversable (undefined :: S [] (Int, Int, [Int])))
  putStr "Tree"
  quickBatch (traversable (undefined :: Tree (Int, Int, [Int])))
