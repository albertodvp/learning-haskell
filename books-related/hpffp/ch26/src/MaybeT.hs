
{-# LANGUAGE InstanceSigs #-}
module MaybeT where
import           Control.Applicative
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }


instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT mMa) = MaybeT $ (fmap . fmap) f mMa


instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . pure
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  MaybeT mMfab <*> MaybeT mMa = MaybeT $ fmap (<*>) mMfab <*> mMa


instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b ) -> MaybeT m b
  MaybeT mMa >>= faMTmMb = MaybeT $ mMa >>= f
    where
--      f :: Maybe a -> m (Maybe b) TODO this looks like the real sign but does not compile if uncommented
      f (Just x) = runMaybeT (faMTmMb x)
      f Nothing  = return Nothing
---

newtype MaybeT' m a = MaybeT' { runMaybeT' :: m (Maybe a) }

instance Functor m => Functor (MaybeT' m) where
  fmap f (MaybeT' mMa) = MaybeT' $ (fmap .  fmap) f mMa



instance Applicative m => Applicative (MaybeT' m) where
  pure :: a -> MaybeT' m a
  pure = MaybeT' . pure . pure
  (<*>) :: MaybeT' m (a -> b) -> MaybeT' m a -> MaybeT' m b
  -- MaybeT' mfab <*> MaybeT' ma = MaybeT' $ fmap (<*>) mfab <*> ma
  MaybeT' mfab <*> MaybeT' ma = MaybeT' $ liftA2 (<*>) mfab ma


instance Monad m => Monad (MaybeT' m) where
  return = pure
  (>>=) :: MaybeT' m a -> (a -> MaybeT' m b) -> MaybeT' m b
  MaybeT' ma >>= faMb = MaybeT' $ ma >>= f
    where
      f Nothing  = return Nothing
      f (Just x) = runMaybeT' $ faMb x





