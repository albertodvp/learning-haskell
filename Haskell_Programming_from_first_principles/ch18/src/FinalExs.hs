module FinalExs where

import Control.Monad (join)


data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg


instance Applicative Nope where
  pure = const NopeDotJpg
  _ <*> _ = NopeDotJpg


instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg
  


--

data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a)  = PLeft $ f a


instance Applicative (BahEither b) where
  pure = PLeft
  PRight f <*> _ = PRight f
  PLeft f <*> be = fmap f be

instance Monad (BahEither b) where
  return = pure
  (PLeft a) >>= fbe = fbe a
  (PRight b) >>= _  = PRight b

--

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> ida = fmap f ida

instance Monad Identity where
  return = pure
  Identity a >>= fid = fid a


--

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil             = Nil
  fmap f (Cons a xs)     = Cons (f a) (fmap f xs)

append :: List a -> List a -> List a
append Nil         ys   = ys
append (Cons x xs) ys   = Cons x $ xs `append` ys

-- concat' :: List (List a) -> List a
-- concat' Nil             = Nil
-- concat' (Cons x xs)     = append x (concat' xs)

fold :: (b -> a -> b) -> b -> List a -> b
fold _ b Nil            = b
fold f b (Cons x xs)    = fold f (f b x) xs

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Applicative List where
  pure = flip Cons Nil
  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil       <*> _   = Nil
  _         <*> Nil = Nil
  --  Cons f fs <*> xs  = (f <$> xs) `append` (fs <*> xs)
  fs <*> xs = flatMap (<$> xs) fs

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  -- (>>=) :: List a -> (a -> List b) -> List b
  xs >>= f = concat' $ f <$> xs
  
--

-- 1
j :: Monad m => m (m a) -> m a
-- j = join
j = (>>= id)

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 = liftA2
-- l2 f xs ys = f <$> xs <*> ys
-- without apply
l2 f xs ys  = f <$> xs >>= (<$> ys)

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
-- a = flip (<*>)
-- a xs = (<*> xs)
a xs fs = fs >>= (<$> xs)

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs fmy = foldr (l2 (:) . fmy) (return []) xs

-- 6
flipType :: Monad m => [m a] -> m [a]
flipType lmx = meh lmx id
