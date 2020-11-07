{-# LANGUAGE DeriveFunctor #-}
module Lib where

-- Reader

newtype Reader r a  = Reader {runReader :: r -> a} deriving (Functor)

instance Applicative (Reader r) where
  pure = Reader . const
  (Reader rf) <*> (Reader ra) = Reader  (\r -> rf r (ra r))

instance Monad (Reader r) where
  Reader ra >>= fr = undefined

    

-- State

newtype State s a = State {runState :: s -> (a, s)}  deriving (Functor)

-- Writer

newtype Writer w a = Writer {runWriter :: (a, w)} deriving (Functor, Show)

-- Cont
newtype Cont r a = Cont {runCont :: (a -> r) -> r}

