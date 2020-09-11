import EitherMonad
import BadMonad
import FinalExs

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative (liftA2)

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

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency xs
    where
      xs = [
        (5, liftA2 Cons arbitrary arbitrary),
        (1, return Nil)
        ]

-- EqProp


instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

instance EqProp (Nope a) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

take' :: Int -> List a -> List a
take' 0 xs = Nil
take' _ Nil = Nil
take' i (Cons a xs) = Cons a $ take' (i-1) xs

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = take' 3000 xs
      ys' = take' 3000 ys

main :: IO ()
main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
      n :: Nope (Int, String, Int)
      n = undefined
      be :: BahEither String (Int, String, Int)
      be = undefined
      i :: Identity (Int, String, Int)
      i = undefined
      l :: List (Int, String, Int)
      l = undefined
        
  quickBatch $ monad (undefined :: Sum String (String, Int, Char))
  putStrLn ""
  putStrLn "CountMe"
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  putStrLn ""
  putStrLn "Sum"
  quickBatch $ monad (undefined :: Sum String (String, Int, Char))
  putStrLn ""
  putStrLn "Nope"
  quickBatch $ functor n
  quickBatch $ applicative n
  quickBatch $ monad n
  putStrLn ""
  putStrLn "BahEither"
  quickBatch $ functor be
  quickBatch $ applicative be
  quickBatch $ monad be
  putStrLn ""
  putStrLn "Identity"
  quickBatch $ functor i
  quickBatch $ applicative i
  quickBatch $ monad i
  putStrLn ""
  putStrLn "List"
  quickBatch $ functor l


