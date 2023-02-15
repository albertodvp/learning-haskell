{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor      (first, second)
import           Data.Char           (isDigit, isSpace)
import           Data.Foldable       (traverse_)
import           Data.List           (groupBy)
import           TextShow
someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Expr a = Lit a
            | Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)


evalExpr :: Num a => Expr a -> a
evalExpr (Lit a)      = a
evalExpr (Add e1 e2)  = evalExpr e1 + evalExpr e2
evalExpr (Mult e1 e2) = evalExpr e1 * evalExpr e2


instance TextShow a => TextShow (Expr a) where
  showbPrec p e =
    case e of
      Lit a      -> showb a
      Add e1 e2  -> showbHelper p 5 "+" e1 e2
      Mult e1 e2 -> showbHelper p 6 "*" e1 e2
    where
      showbHelper outerPrec thisPrec op e1 e2 =
        showbParen (outerPrec > thisPrec)
        $ showbPrec thisPrec e1 <> op <> showbPrec thisPrec e2



type Token = String
type Stack = [Token]
type Output = [Expr Integer]
type SYState = (Stack, Output)

isEmpty :: State SYState Bool
isEmpty = gets (null . fst)
notEmpty :: State SYState Bool
notEmpty = not <$> isEmpty

top :: State SYState Token
top = gets (head . fst)

pop :: State SYState Token
pop = do
  (stack, output) <- get
  put (tail stack, output)
  return $ head stack
  --(h:t, output) <- get TODO why this does not work?
  -- put (t, output)
  -- return h

pop_ :: State SYState ()
pop_ = modify (first tail)

push :: Token -> State SYState ()
push t = modify (first (t:))


whileNotEmptyAnd :: (Token -> Bool) -> State SYState () -> State SYState ()
whileNotEmptyAnd predicate m = go
  where
    go = do
      b1 <- notEmpty
      when b1 $ do
        b2 <- predicate <$> top
        when b2 (m >> go)


output :: Token -> State SYState ()
output = modify . second . builder
  where
    builder "+" (e1:e2:es) = Add e1 e2 : es
    builder "*" (e1:e2:es) = Mult e1 e2 : es
    builder n es           = Lit (read n) : es

isOp "+" = True
isOp "*" = True
isOp _   = False

precedence "+" = 1
precedence "*" = 2
precedence _   = 0

t1 `precGTE` t2 = precedence t1 >= precedence t2

convertToExpr :: String -> Expr Integer
convertToExpr str = head $ snd $ execState shuntingYard ([],[])

  where
    tokens = reverse $ tokenize str

    shuntingYard = traverse_ transferToken tokens >> transferRest

    transferToken ")" = push ")"
    transferToken "(" = transferWhile (/= ")") >> pop_
    transferToken t
      | isOp t = transferWhile (`precGTE` t) >> push t
      | otherwise = output t

    transfer = pop >>= output
    transferWhile predicate = whileNotEmptyAnd predicate transfer
    transferRest = transferWhile (const True)

    tokenize = groupBy (\a b -> isDigit a && isDigit b) . filter (not . isSpace)
