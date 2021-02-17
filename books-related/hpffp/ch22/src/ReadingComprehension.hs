{-# LANGUAGE InstanceSigs #-}

module ReadingComprehension where

newtype Reader r a =
  Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

ask :: (r -> a) -> Reader r a
ask = Reader

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r
