module P1 where

import           Data.Monoid
type Predicates = Int -> Bool
mkFilter :: [Predicates] -> Int -> Bool
mkFilter p = getAny . foldMap (Any .) p

p1f :: Int -> Bool
p1f = mkFilter [m 3, m 5]
  where
    m x = (==) 0 . flip rem x

p1 :: Int
p1 = sum $ filter p1f [1..999]
