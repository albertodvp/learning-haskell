-- |
{-# LANGUAGE InstanceSigs #-}
module LiftIO where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           MaybeT
import           ReaderT
import           StateT

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = MaybeT . fmap Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  --liftIO x = MaybeT $ Just <$> liftIO x
  --liftIO = MaybeT . fmap Just . liftIO
  liftIO = lift . liftIO

--
instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  --lift ma = ReaderT $ \r -> ma
  -- lift ma = ReaderT $ const ma
  lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  --liftIO ioa  = ReaderT $ \r -> liftIO ioa
  -- liftIO = ReaderT .  const <$> liftIO
  liftIO = lift . liftIO


--
instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ \s -> ma >>= \a -> return (a,s)


instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
