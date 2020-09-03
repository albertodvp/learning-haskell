module FinalExs where


import Control.Applicative (liftA3)

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair  where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)
  
--

data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b


instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a f) (Two a' x) = Two (a <> a') (f x)

--

data Three a b c = Three a b c deriving (Eq, Show)


instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' x) =
    Three (a<>a') (b<>b') (f x)
  
--

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')


instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f g) (Three' a' x y) = Three' (a<>a') (f x) (g y) 
  
stops :: String
stops = "pdtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
