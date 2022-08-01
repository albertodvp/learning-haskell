-- |
{-# LANGUAGE InstanceSigs #-}

module StateT where
import           Control.Applicative
import           Control.Monad
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap fab (StateT smas) = StateT $ \s -> fmap f (smas s)
    where
      f (a,s) = (fab a, s)

-- instance Applicative m => Applicative (StateT s m) where
--   pure a = StateT $ \s -> pure (a, s)
--   (StateT smfabs) <*> (StateT smas) = StateT $ \s -> let mfabs = smfabs s
--                                                          mas = smas s
--                                                          g (fab, s) (a, s') = (fab a, s)
--                                                      in liftA2 g mfabs mas

-- you can’t express the order-
-- dependent computation you’d expect the StateT Applicative to
-- have without having a Monad for m. In essence, the issue is that
-- without Monad, you’re feeding the initial state to each computa-
-- tion in StateT rather than threading it through as you go. This is
-- a general pattern contrasting Applicative and Monad and is worth
-- contemplating
instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT smfabs) <*> (StateT smas) = StateT $ \s -> let g fab' (a, s'') = (fab' a, s'')
                                                     in smfabs s >>= \(fab, s') -> g fab <$> smas s'


instance Monad m => Monad (StateT s m) where
  return = pure
  -- 1)
  -- StateT smas >>= aSTsmbs = StateT $ \s -> do
  --   (a, s') <- smas s
  --   (runStateT $ aSTsmbs a) s'
  -- 2)
  -- StateT smas >>= aSTsmbs = StateT $ \s -> smas s >>= \(a, s') -> (runStateT $ aSTsmbs a) s'
  -- 3)
  StateT smas >>= aSTsmbs = StateT $ smas >=> \(a,s') -> (runStateT $ aSTsmbs a) s'



--


newtype StateT' s m a = StateT' { runStateT' :: s -> m (a, s) }

instance Functor m => Functor (StateT' s m) where
  fmap :: (a -> b) -> StateT' s m a -> StateT' s m b
  fmap f (StateT' smas) = StateT' $ (fmap . fmap) g smas
    where
      g (a, s') = (f a, s')


instance Monad m => Applicative (StateT' s m) where
  pure x = StateT' $ \s -> pure (x ,s)
  -- \s -> smf s >>= \(fab, s') -> (\(a, s'') -> (fab a, s''))<$> smas s'
  StateT' smf <*> StateT' smas = StateT' $ smf >=> \(fab, s') -> (\(a, s'') -> (fab a, s''))<$> smas s'

instance Monad m => Monad (StateT' s m) where
  return = pure
  -- StateT' smas >>= fSsmbs = StateT' $ \s -> do
  --   (a, s') <- smas s
  --   runStateT' (fSsmbs a) s'
  StateT' smas >>= fSsmbs = StateT' $ smas >=> \(a, s') -> runStateT' (fSsmbs a) s'



