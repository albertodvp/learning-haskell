module ReaderPractice where

import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
x = [1,2,3]
y = [4,5,6]
z = [7,8,9]


lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' x [] = Nothing
lookup' x ((a,b):xs)
  | x == a = Just b
  | otherwise = lookup' x xs


xs :: Maybe Integer
xs = lookup' 3 $ zip x y


ys :: Maybe Integer
ys = lookup' 6 $ zip y z

zs :: Maybe Integer
zs = lookup' 4 $ zip x y


z' :: Integer -> Maybe Integer
z' = flip lookup' $ zip x z


x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys


x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
--x3 x = (z' x, z' x)
x3 = liftA2 (,) z' z'


uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a,b) =  f a b

uncurry'' :: (a -> b -> c) -> (a, b) -> c
uncurry'' f = liftA2 f fst snd


summed :: Num c => (c,c) -> c
summed = uncurry'' (+)

summed' :: Num c => (c,c) -> c
summed' = liftA2 (+) fst snd

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' _ (Just x) = x
fromMaybe' x Nothing  = x

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> liftA2 (,) xs ys
  print $ summed <$> liftA2 (,) xs zs
  print $ bolt 7
  print $ bolt <$> z
  print $ sequenceA [(>3), (<8), even] 7
-- 1. Fold the Boolean conjunction operator over the list of results of sequA (applied to some value).
  print $  and $ sequA 42
-- 2. Apply sequA to s'—you’ll need fromMaybe.
  print $ (sequA <$> fromMaybe 0) s'
-- 3. Apply bolt to ys—you’ll need fromMaybe.
  print $ (bolt <$> fromMaybe 0) s'

