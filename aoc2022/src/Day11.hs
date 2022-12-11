{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day11 (module Day11) where

import Protolude
import qualified Prelude

import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepBy1, oneOf)
import Text.Megaparsec.Char (string, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as M
type Parser = Parsec Void Text
type Id = Int
type WorryLevel = Int
type TestTrow = WorryLevel -> Bool
type Op = WorryLevel -> WorryLevel

data Monkey = Monkey {
  id::Id,
  worryLevels::[WorryLevel],
  inspectionOperation::Op,
  throwPredicate::TestTrow,
  idIfTrueThrow :: Id,
  idIfFalseThrow :: Id
  }
data Game = Game {monkeysMap :: M.Map Id Monkey, inspectedItems :: M.Map Id Int} deriving Show

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

initialState :: [Monkey] -> Game
initialState ms = let monkeys = M.fromList $ zip [0..] ms
                      dim = M.size monkeys
                      activities = M.fromList $ zip [0..dim-1] (repeat 0)
                  in Game monkeys activities

receive :: WorryLevel -> Monkey -> Monkey
receive wl m@(Monkey id wls op tt tid fid) = Monkey id (wls ++ [wl]) op tt tid fid

playThrow :: TestTrow -> Id -> Id -> Game -> WorryLevel -> Game
playThrow tt tid fid (Game mm as) wl = Game (M.insert targetId receiver mm) as
  where
    targetId = if tt wl then tid else fid
    receiver = receive wl $ (M.!) mm targetId

playMonkey :: Game -> Int -> Game
playMonkey (Game mm as) monkeyId = foldl' (playThrow tt tid fid) updatedGame (updatedWorryLevel)
  where
    (Monkey mId wls op tt tid fid) = (M.!) mm monkeyId
    updatedMM = M.insert mId (Monkey mId [] op tt tid fid) mm
    updatedAS = M.insertWith (+) mId (length wls) as
    updatedGame = Game updatedMM updatedAS
    updatedWorryLevel = map ((`div` 3) . op) wls

playRound :: Game -> Int -> Game
playRound g@(Game mm _) _ = foldl' playMonkey g [0..M.size mm-1]

play :: [Monkey] -> Int
play ms = let (Game _ as) = foldl' playRound (initialState ms) [0..19]
          in product $ take 2 $ sortOn Down (snd <$> M.toList as)

play' :: [Monkey] -> Game
play' ms = foldl' playRound (initialState ms) [0..19]

day11 :: IO ()
day11 = do
    t <- readFile "inputs/day11.txt"
    case parse monkeysP "" t of
      Left err -> putStrLn $ errorBundlePretty err
      Right ms -> print $ play ms
--      Right ms -> print $ play' ms
