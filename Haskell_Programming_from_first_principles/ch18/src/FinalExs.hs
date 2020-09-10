module FinalExs where

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
