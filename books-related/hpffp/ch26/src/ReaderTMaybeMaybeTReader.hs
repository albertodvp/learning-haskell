-- |

module ReaderTMaybeMaybeTReader where

import           MaybeT
import           ReaderT

newtype Reader r a = Reader { runReader :: r -> a }

type First r = ReaderT r Maybe
type Second r = MaybeT (Reader r)


ex :: First r a -> r -> Maybe a
ex = runReaderT

ex' :: Second r a -> r -> Maybe a
ex' = runReader . runMaybeT

one :: a -> First r a
one x = ReaderT $ const (Just x)

one' :: a -> Second r a
one' x= MaybeT $ Reader $ const (Just x)

