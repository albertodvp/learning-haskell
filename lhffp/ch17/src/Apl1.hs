module Apl1 where

import Control.Applicative


newtype ZL a = ZL {getList :: [a]} deriving (Eq, Show)

instance Functor ZL where
  --  fmap :: (a -> b) -> ZL a -> ZL b
  fmap f (ZL xs) = ZL $ fmap f xs


instance Applicative ZL where
  --  pure :: a -> F a
  pure x = ZL $ repeat x
  (<*>) (ZL fs) (ZL as) = ZL $ fs <*> as

instance Semigroup a => Semigroup (ZL a) where
  --  (<>) :: a -> a -> a
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZL a) where
  mempty = pure mempty
  mappend = liftA2 mappend

