{-# LANGUAGE InstanceSigs #-}

module MyMaybeT where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor f => Functor (MaybeT f) where
  fmap :: (a -> b) -> MaybeT f a -> MaybeT f b
  fmap f (MaybeT mMa) = MaybeT $ fmap f <$> mMa


instance Applicative f => Applicative (MaybeT f) where
  pure :: a -> MaybeT f a
  pure = MaybeT . pure . Just

  (<*>) :: MaybeT f (a -> b) -> MaybeT f a -> MaybeT f b
  MaybeT mMfab <*> MaybeT mMa = MaybeT $ fmap (<*>) mMfab <*> mMa


instance Monad m => Monad (MaybeT m)  where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT mMa >>= faMmMb = undefined


