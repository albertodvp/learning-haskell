module ZipList where

import Control.Applicative

import List

newtype ZipList' a = ZipList' {getList :: List a} deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

  
instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  (<*>) (ZipList' fs) (ZipList' as) = ZipList' $ zipWith' ($) fs as

instance Semigroup a => Semigroup (ZipList' a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend

