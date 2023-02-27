module Lib where
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Monad.State
import           Data.Text

type Stack = [Integer]

type EnvVars = [(Text, Integer)]

data EvalError = NotEnoughElements | ExtraElement deriving (Show, Eq)

type EvalM = ReaderT EnvVars (ExceptT EvalError (State Stack))

evalEvalM :: EnvVars -> EvalM Integer -> Either EvalError Integer
evalEvalM envVars e = evalState (runExceptT (runReaderT e envVars)) []

push :: Integer -> State Stack ()
push x = modify (x:)

pop :: ExceptT EvalError (State Stack) Integer
pop = get >>= pop'
  where
    pop' []     = throwError NotEnoughElements
    pop' (x:xs) = put xs >> pure x

