{-# LANGUAGE NoImplicitPrelude #-}

module Data.Deque (Deque, empty, isEmpty, front, back, push_back, push_front, pop_back, pop_front) where

import           Data.Bool     (Bool)
import           Data.Maybe    (Maybe (..))
import           Data.Sequence hiding (empty)
import qualified Data.Sequence as Seq

newtype Deque a = Deque (Seq a)

empty :: Deque a
empty = Deque Seq.empty

isEmpty :: Deque a -> Bool
isEmpty (Deque d) = Seq.null d

front :: Deque a -> Maybe a
front (Deque Empty)     = Nothing
front (Deque (x :<| _)) = Just x

back :: Deque a -> Maybe a
back (Deque Empty)     = Nothing
back (Deque (_ :|> x)) = Just x

push_front :: a -> Deque a -> Deque a
push_front x (Deque d) = Deque (x <| d)

push_back :: a -> Deque a -> Deque a
push_back x (Deque d) = Deque (d |> x)

pop_front :: Deque a -> Deque a
pop_front (Deque Empty)      = empty
pop_front (Deque (_ :<| xs)) = Deque xs

pop_back :: Deque a -> Deque a
pop_back (Deque Empty)      = empty
pop_back (Deque (xs :|> _)) = Deque xs
