 -- |
module ChapterExercises where

import           Control.Applicative (liftA2)



newtype State s a = State { runState :: s -> (a,s)}

instance Functor (State s) where
  fmap f (State sas) = State $ \s -> let (a, s') = sas s
                                     in (f a, s')

instance Applicative (State s) where
  pure a = State $ (,) a
  State sfa <*> State sa = State $ \s -> let (fa, s') = sfa s
                                             (a, s'') = sa s'
                                         in (fa a, s'')

instance Monad (State s) where
  State  sa >>= faSb = State $ \s -> let (a, s') = sa s
                                     in runState (faSb a) s'


-- 1. Construct a State where the state is also the value you return:
get :: State s s
get = State $ \s -> (s, s)

get' :: State s s
get' = State $ liftA2 (,) id id

-- 2. Construct a State where the resulting state is the argument
--  provided, and the value defaults to unit
put :: s -> State s ()
put s = State $ \s' -> ((), s)

put' :: s -> State s ()
put' s = State $ const ((),s)

-- 3. Run the State with s and get the state that results:
exec :: State s a -> s -> s
exec (State sa) = snd . sa

-- 4. Run the State with s and get the value that results:
eval :: State s a -> s -> a
eval (State sa) = fst . sa


-- 5. Write a function that applies a function to create a new State:
modify :: (s -> s) -> State s ()
modify ss = State $ \s -> ((), ss s)
