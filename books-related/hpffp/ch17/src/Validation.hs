module Validation where

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)


instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a



instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Failure e) (Failure e') = Failure $ e <> e'
  (<*>) (Failure e) _            = Failure e
  (<*>) _           (Failure e)  = Failure e
  (<*>) (Success f) (Success a)  = Success $ f a
