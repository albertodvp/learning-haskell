{-# LANGUAGE InstanceSigs #-}
module ReaderMonad where

import           Data.Char


newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure = Reader . const
  Reader rab <*> Reader ra = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r



strIsEnougthToLower :: Reader [Char] Bool
strIsEnougthToLower = Reader $ \s -> length s > 5

strLenFilterLower :: Bool -> Reader [Char] Int
strLenFilterLower x = Reader $  length . filter (if x then isLower else isUpper)


comp = do
  isEnougth <- strIsEnougthToLower
  strLenFilterLower isEnougth

comp' = strIsEnougthToLower >>= strLenFilterLower
