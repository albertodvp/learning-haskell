{-# LANGUAGE InstanceSigs #-}
module EitherT where
import           Control.Applicative
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
swapEitherT  = EitherT . fmap swapEither . runEitherT

either' :: (Monad m) => (a -> m c) -> (b -> m c) -> Either a b -> m c
either' famc fbmc eab = case eab of
  Left a  -> famc a
  Right b -> fbmc b

eitherT :: (Monad m) => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g x = runEitherT x >>= either' f g

--

newtype EitherT' e m a = EitherT' {runEitherT' :: m (Either e a)}

instance Functor m => Functor (EitherT' e m) where
  fmap f (EitherT' mEea) = EitherT' $ (fmap . fmap) f mEea


instance Applicative m => Applicative (EitherT' e m) where
  pure = EitherT' . pure . pure
  (<*>) :: EitherT' e m (a -> b) -> EitherT' e m a -> EitherT' e m b
  EitherT' mfab <*> EitherT' mfa = EitherT' $ liftA2 (<*>) mfab mfa



instance Monad m => Monad (EitherT' e m) where
  return = pure
  EitherT' ma >>= faEmb = EitherT' $ ma >>= f
    where
      f (Left e)  = return $ Left e
      f (Right a) = runEitherT' $ faEmb a

swapEitherT' :: (Functor m) => EitherT' e m a -> EitherT' a m e
swapEitherT' = EitherT' . fmap swapEither . runEitherT'

either'' :: (a -> c) -> (b -> c) -> Either a b -> c
either'' f _ (Left a)  = f a
either'' _ g (Right b) = g b

eitherT' :: Monad m => (a -> m c) -> (b -> m c) -> EitherT' a m b -> m c
eitherT' f g = (=<<) (either'' f g) . runEitherT'

