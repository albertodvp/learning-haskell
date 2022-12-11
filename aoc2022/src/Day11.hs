{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day11 (module Day11) where

import Protolude
import qualified Prelude

import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepBy1, oneOf)
import Text.Megaparsec.Char (string, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
type Id = Int
type WorryLevel = Int
type TestTrow = WorryLevel -> Bool
type Op = WorryLevel -> WorryLevel 

data Monkey = Monkey Id [WorryLevel] Op TestTrow Id Id

instance Prelude.Show Monkey where
  show (Monkey id wl op tt tid fid) = "Monkey: " ++ show id ++ ", worry levels: " ++ show wl ++ "(trueId, falseId): " ++ show (tid, fid)
  
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space1

mkOp :: Char -> WorryLevel -> Op
mkOp '+' = (+)
mkOp '*' = (*)
mkOpFromOld :: Char -> Op
mkOpFromOld '+' = (*2)
mkOpFromOld '*' = (^2)

opP :: Parser Op
opP = do
  opC <- oneOf ['+', '*'] <* space1
  mkOp opC <$> L.decimal <|> mkOpFromOld opC <$ string "old"

ttP :: Parser TestTrow
ttP = do
  y <- L.decimal
  return $ \x -> mod x y == 0

monkeyP :: Parser Monkey
monkeyP = do
  id <- lexeme $ string "Monkey " *> L.decimal <* string ":"
  wl <- lexeme $ string "Starting items: " *> sepBy1 L.decimal (string ", ")
  op <- lexeme $ string "Operation: new = old " *> opP
  tt <- lexeme $ string "Test: divisible by " *> ttP
  trueCaseId <- lexeme $ string "If true: throw to monkey " *> L.decimal
  falseCaseId <- lexeme $ string "If false: throw to monkey " *> L.decimal
  pure $ Monkey id wl op tt trueCaseId falseCaseId

monkeysP :: Parser [Monkey]
monkeysP = some monkeyP


day11 :: IO ()
day11 = do
    t <- readFile "inputs/day11.txt"
    case parse monkeysP "" t of
      Left err -> putStrLn $ errorBundlePretty err
      Right ms -> print ms
