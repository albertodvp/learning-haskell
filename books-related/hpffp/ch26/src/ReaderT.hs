{-# LANGUAGE InstanceSigs #-}
module ReaderT where
import           Control.Applicative
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  --fmap fab (ReaderT rma) = ReaderT $ fmap fab . rma
  fmap fab (ReaderT rma) = ReaderT $ (fmap . fmap) fab rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . const . pure
  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  --ReaderT rmfab <*> ReaderT rma = ReaderT $ \r -> rmfab r <*> rma r
  ReaderT rmfab <*> ReaderT rma = ReaderT $ liftA2 (<*>) rmfab rma


instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  -- ReaderT rma >>= aRTrmb = ReaderT $ \r -> rma r >>= (\a -> runReaderT (aRTrmb a) r)
  ReaderT rma >>= aRTrmb = ReaderT $ \r -> do
    a <- rma r
    runReaderT (aRTrmb a) r

--

newtype ReaderT' r m a = ReaderT' { runReaderT' :: r -> m a }

instance Functor m => Functor (ReaderT' r m) where
  fmap f (ReaderT' rma) = ReaderT' $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT' r m) where
  pure = ReaderT' . const . pure
  ReaderT' rmfab <*> ReaderT' rma = ReaderT' $ liftA2 (<*>) rmfab rma


instance Monad m => Monad (ReaderT' r m) where
  return = pure
  -- ReaderT' rma >>= faRmb = ReaderT' $ rma >>= \ma r-> ma >>= \a -> runReaderT' (faRmb a) r
  -- ReaderT' rma >>= faRmb = ReaderT' $ \r -> rma r >>= \a -> runReaderT' (faRmb a) r
  ReaderT' rma >>= faRmb = ReaderT' $ \r -> do
    a <- rma r
    runReaderT' (faRmb a) r




