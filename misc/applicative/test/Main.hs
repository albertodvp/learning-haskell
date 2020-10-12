import Lib

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA, liftA2, liftA3)

-- Cmp
-- instance Arbitrary (Cmp f g a) where
--   arbitrary = pure <$> arbitrary

-- instance EqProp (Cmp f g a) where
--   a =-= b = a `eq` b


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
    (10, liftA LeafRose arbitrary),
    (1, liftA Rose $ frequency [(1, arbitrary), (10, return [])])
    ]
    
instance Eq a => EqProp (Rose a) where
  r1 =-= r2 = x `eq` y
    where
      takeRose (Rose rs) = Rose $ take 3000 rs
      takeRose lr = lr
      x = takeRose r1
      y = takeRose r2

-- RoseL
instance Arbitrary a => Arbitrary (RoseL a) where
  arbitrary = frequency [
      (100, flip RoseL [] <$> arbitrary),
      (1, liftA2 RoseL arbitrary arbitrary)
      ]

instance Eq a => EqProp (RoseL a) where
  r1 =-= r2 = x `eq` y
    where
      takeRose (RoseL a rs) = RoseL a $ take 3000 rs
      x = takeRose r1
      y = takeRose r2


              
main :: IO()
main = do
  putStr "\n\n-- Compare (TODO) --\n"
  -- quickBatch $ functor (undefined :: Cmp Maybe (Either String) (Int, String, Char) )
  -- quickBatch $ applicative (undefined :: Cmp Maybe (Either String) (Int, String, Char) )
  putStr "\n\n-- Bin --\n"
  quickBatch $ functor (undefined :: Bin (Int, String, Char) )
  quickBatch $ applicative (undefined :: Bin (Int, String, Char) )
  putStr "\n\n-- BinL --\n"
  quickBatch $ functor (undefined :: BinL (Int, String, Char) )
  quickBatch $ applicative (undefined :: BinL (Int, String, Char) )
  putStr "\n\n-- Rose --\n"
  quickBatch $ functor (undefined :: Rose (Int, String, Char) )
  quickBatch $ applicative (undefined :: Rose (Int, String, Char) )
  putStr "\n\n-- RoseL --\n"
--  quickBatch $ functor (undefined :: RoseL (Int, String, Char) )
--  quickBatch $ applicative (undefined :: RoseL (Int, String, Char) )
