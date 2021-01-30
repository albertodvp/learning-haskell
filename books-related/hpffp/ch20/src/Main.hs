module Main where

import           Data.Monoid

main :: IO ()
main = do
  putStrLn "hello world"


sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

product'' :: (Foldable t, Num a) => t a  -> a
product'' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (x ==))

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x = foldr ((||) . (x ==)) False

-- TODO
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' fs
  | null fs   = Nothing
  | otherwise = Just $ foldr1 min fs

-- TODO
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' fs
  | null fs = Nothing
  | otherwise = Just $  foldr1 max fs

null' :: Foldable t => t a -> Bool
null' = foldr ((&&) . const False) True

null'' :: Foldable t => t a -> Bool
null'' = getAll . foldMap (const (All False))

length' :: Foldable t => t a -> Int
length' = getSum . foldMap (const (Sum 1))

length'' :: Foldable t => t a -> Int
length'' = foldr (const (+1)) 0

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

fold'' :: (Foldable t, Monoid m) => t m -> m
fold'' = foldr (<>) mempty

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

data Constant a b = Constant b

instance Foldable (Constant a) where
  -- foldr :: (b -> c -> c) -> c -> Constant a b -> c
  foldr fbc c (Constant b) = fbc b c
  -- foldMap :: (Monoid c) => (b -> c) -> Constant a b -> c
  foldMap fbc (Constant b) = fbc b

data Two a b = Two a b

instance Foldable (Two a) where
  foldr fbc c (Two _ b) = fbc b c
  foldMap fbm (Two _ b) = fbm b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap fcm (Three _ _ c) = fcm c
  foldr fcd d (Three _ _ c) = fcd c d

data Three' a b = Three' a b b

-- more alternatives here
instance Foldable (Three' a) where
  foldr fbc c (Three' _ b _) = fbc b c

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr fbc c (Four' _ _ b _) = fbc b c


filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

