{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module MonadReader() where

import           Prelude hiding (Semigroup)
-- class Monad m => MonadReader r m | m -> r where
--   ask :: m r

class Monad m => MonadReader r m where
  ask :: m r
  reader :: (r -> a) -> m a

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure = Reader . const
  Reader rfa <*> Reader ra = Reader $ \r -> rfa r (ra r)

instance Monad (Reader r) where
  return = pure
  Reader ra >>= faRb = Reader $ \r -> let a = ra r
                                      in runReader (faRb a) r
instance MonadReader r (Reader r) where
  ask = Reader id
  reader ra = Reader ra
