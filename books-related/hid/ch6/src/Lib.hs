module Lib where

import           Control.Monad.State (State, StateT, evalState, get, guard,
                                      lift, modify, put, state, when)
import           Data.Foldable       (traverse_)


type Stack = [Integer]
type EvalM = State Stack


push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  xs <- get
  put $ tail xs
  pure $ head xs

pop' :: EvalM Integer
pop' = state $ \(x:xs) -> (x,xs)


evalRPN :: String -> Integer
evalRPN s = evalState evalRPN' []
  where
    evalRPN' :: EvalM Integer
    evalRPN' = traverse_ step (words s) >> pop
    step :: String -> EvalM ()
    step "+" = execOp (+)
    step "*" = execOp (+)
    step "-" = execOp (+)
    step n   = push $ read n
    execOp op = flip op <$> pop <*> pop >>= push


cases =
  [ "2 3 +"
  , "2 k"
  , "2 +"
  , "2 6"
  ]

boom :: [Integer]
boom = map evalRPN cases


type EvalM' = StateT Stack Maybe
push' :: Integer -> EvalM' ()
push' x = modify (x:)

-- Solution1: using `lift`
pop'' :: EvalM' Integer
pop'' = do
  xs <- get
  when (null xs) $ lift Nothing
  put $ tail xs
  pure $ head xs


-- Solution2: using `guard` from `Alternative`
pop''' :: EvalM' Integer
pop''' = do
  xs <- get
  guard (not $ null xs)
  put $ tail xs
  pure $ head xs

-- TODO:
-- with guard is that?
-- https://hackage.haskell.org/package/monadplus-1.4.3/docs/Control-Monad-Plus.html

-- Solution3: using MonadFail instance
pop'''' :: EvalM' Integer
pop'''' = do
  (x:xs) <- get
  put xs
  pure x
