{-# LANGUAGE InstanceSigs #-}
module EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap fab (EitherT mEea) = EitherT $ (fmap . fmap) fab mEea

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . pure

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  EitherT mEfab <*> EitherT mEa = EitherT $ fmap (<*>) mEfab <*> mEa


instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT mEea >>= faETmEea = EitherT $ mEea >>= f
    where
      f (Right a) = runEitherT $ faETmEea a
      f (Left e)  = return $ Left e
  -- EitherT mEea >>= faETmEea = EitherT $ do
  --   eea <- mEea
  --   case eea of
  --     Right a -> runEitherT $ faETmEea a
  --     Left e  -> return $ Left e

swapEither :: Either e a -> Either a e
swapEither (Right a) = Left a
swapEither (Left e)  = Right e

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
--swapEitherT (EitherT mEea) = EitherT $ swapEither <$> mEea
swapEitherT  = EitherT . fmap swapEither . runEitherT
