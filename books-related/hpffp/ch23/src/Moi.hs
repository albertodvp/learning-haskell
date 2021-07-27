{-# LANGUAGE InstanceSigs #-}

module Moi where

newtype Moi s a = Moi {runMoi :: s -> (a, s)}


instance Functor (Moi s) where
  fmap f (Moi sas) = Moi $ \s -> let (a, s') = sas s
                                 in (f a, s')

-- TODO check with tests if I cannot propagate the state
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure = undefined
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi sab <*> Moi sa = Moi $ \s -> let (fab, s') = sab s
                                       (a, s'') = sa s'
                                    in (fab a, s'')

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi sa >>= aMb = Moi $ \s -> let (a, s') = sa s
                                   (b, s'') = runMoi (aMb a) s'
                               in (b, s'')
