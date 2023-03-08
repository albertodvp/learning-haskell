{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Applicative  (liftA2, (<|>))
import           Control.Monad.State
import           Data.Foldable        (traverse_)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Read       (decimal)
type Stack = [Integer]

type EnvVars = [(Text, Integer)]

data EvalError = NotEnoughElements
               | ExtraElements
               | CannotParseNumber
               | SymbolNotPresentOnEnv Text
               deriving (Show, Eq)

type EvalM = ReaderT EnvVars (ExceptT EvalError (State Stack))

evalEvalM :: EnvVars -> Stack -> EvalM a -> Either EvalError a
evalEvalM envVars state e = evalState (runExceptT (runReaderT e envVars)) state

execEvalM :: EnvVars -> Stack -> EvalM a -> Stack
execEvalM envVars state e = execState (runExceptT (runReaderT e envVars)) state

parseNumber  :: Text -> EvalM Integer
parseNumber t = case decimal t of
  Right (n, "") -> return n
  _             -> throwError CannotParseNumber

getEnvValue :: Text -> EvalM Integer
getEnvValue t = ask >>= getValue . lookup t
  where
    getValue (Just n) = return n
    getValue Nothing  = throwError $ SymbolNotPresentOnEnv t


parse :: Text -> EvalM Integer
parse x = catchError (parseNumber x) (const $ getEnvValue x)

evalPRN :: Text -> EvalM Integer
evalPRN str = do
  traverse_ step (T.words str) >> oneElementOnStack >> gets head
  where
    step :: Text -> EvalM ()
    step "+" = applyOp (+)
    step "*" = applyOp (*)
    step n   = parse n >>= push
    applyOp op = liftA2 op pop pop >>= push

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = get >>= pop'
  where
    pop' []     = throwError NotEnoughElements
    pop' (x:xs) = put xs >> pure x

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  xs <- get
  case xs of
    []  -> throwError NotEnoughElements
    [_] -> pure ()
    _   -> throwError ExtraElements

