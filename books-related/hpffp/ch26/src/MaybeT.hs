{-# LANGUAGE InstanceSigs #-}
module MaybeT where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT mMa) = MaybeT $ (fmap . fmap) f mMa

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . pure

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  MaybeT mMfab <*> MaybeT mMa = MaybeT $ (fmap (<*>) mMfab) <*> mMa


instance Monad m => Modan (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  >>=

--    (<*>) (fmap (<*>) (fmap <*> f)) x
