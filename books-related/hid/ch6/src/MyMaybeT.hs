{-# LANGUAGE InstanceSigs #-}


module MyMaybeT where
import           Control.Monad.RWS   (MonadState, state)
import           Control.Monad.Trans


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
  MaybeT mMa >>= faMTmb = MaybeT $ mMa >>= g
    where
      g (Just a) = runMaybeT $ faMTmb a
      g Nothing  = pure Nothing

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = MaybeT . fmap Just
