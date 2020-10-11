import Lib

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA, liftA2, liftA3)
-- Cmp

instance Arbitrary (Cmp f g a) where
  arbitrary = undefined

instance EqProp (Cmp f g a) where
  (=-=) (Cmp fga) (Cmp fgb) = undefined

-- Bin
instance Arbitrary a => Arbitrary (Bin a) where
  arbitrary = frequency [
    (4, liftA Leaf arbitrary),
    (1, liftA2 Bin arbitrary arbitrary)
    ]

instance Eq a => EqProp (Bin a) where
  b1 =-= b2 = b1 `eq` b2

-- Bin L
instance Arbitrary a => Arbitrary (BinL a) where
  arbitrary = frequency [
    (4, return LeafL),
    (1, liftA3 BinL arbitrary arbitrary arbitrary)
    ]
   
instance Eq a => EqProp (BinL a) where
  b1 =-= b2 = b1 `eq` b2

-- Rose
instance Arbitrary a => Arbitrary (Rose a) where
  arbitrary = frequency [
    (4, liftA LeafRose arbitrary),
    (1, liftA Rose arbitrary)
    ]
    
instance Eq a => EqProp (Rose a) where
  r1 =-= r2 = x `eq` y
    where
      takeRose (Rose rs) = Rose $ take 3000 rs
      takeRose lr = lr
      x = takeRose r1
      y = takeRose r2

        
main :: IO()
main = do
  putStr "Compare"
  quickBatch $ functor (undefined :: Cmp Maybe (Either String) (Int, String, Char) )
  quickBatch $ applicative (undefined :: Cmp Maybe (Either String) (Int, String, Char) )
  putStr "Bin"
  quickBatch $ functor (undefined :: Bin (Int, String, Char) )
  quickBatch $ applicative (undefined :: Bin (Int, String, Char) )
  putStr "BinL"
  quickBatch $ functor (undefined :: BinL (Int, String, Char) )
  quickBatch $ applicative (undefined :: BinL (Int, String, Char) )
  putStr "Rose"
--  quickBatch $ functor (undefined :: Rose (Int, String, Char) )
