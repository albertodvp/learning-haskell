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
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

concat' :: List (List a) -> List a
concat' = undefined

instance Applicative List where
  pure = flip Cons Nil
  Nil       <*> _   = Nil
  _         <*> Nil = Nil
  Cons f fs <*> xs  = concat' $ Cons (fmap f xs) (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = concat' $ Cons (f x) (fmap f xs)
  
