{-# LANGUAGE OverloadedStrings #-}

module Day02(day02) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Choice = Rock | Paper | Scissors deriving Eq
data Result = Lose | Draw | Win

type Round = (Choice, Choice)
type Game = [Round]

-- Parsing
parseResult :: Char -> Result
parseResult 'X' = Lose
parseResult 'Y' = Draw
parseResult 'Z' = Win
parseResult _ = error "error"

parseChoice :: Char -> Choice
parseChoice c
  | elem c ("AX" :: String) = Rock
  | elem c ("BY" :: String) = Paper
  | elem c ("CZ" :: String) = Scissors
  | otherwise = error "error"


parseRoundP1 :: Text -> Round
parseRoundP1 t = (parseChoice c1, parseChoice c2)
  where
    [c1, ' ', c2] = T.unpack t

parseRoundP2 :: Text -> Round
parseRoundP2 t = (oChoice, choice)
  where
    [c1, ' ', c2] = T.unpack t
    oChoice = parseChoice c1
    result= parseResult c2
    choice = choiceFromResult result oChoice

mkGame :: Text -> Game
mkGame = map parseRoundP2 . T.lines

-- Logic
choiceFromResult :: Result -> Choice -> Choice
choiceFromResult Draw = id
choiceFromResult Lose = choiceToWin . choiceToWin
choiceFromResult Win = choiceToWin 

choiceToWin :: Choice -> Choice
choiceToWin Scissors = Rock
choiceToWin Paper = Scissors
choiceToWin Rock = Paper

choiceValue :: Choice -> Int
choiceValue Rock = 1
choiceValue Paper = 2
choiceValue Scissors = 3

roundScore :: Choice -> Choice -> Int
roundScore Rock Scissors = 6
roundScore Paper Rock = 6
roundScore Scissors Paper = 6
roundScore x y
  | x == y = 3
  | otherwise = 0


playRound :: Round -> Int
playRound (opponent, toPlay) = choiceValue toPlay + roundScore toPlay opponent

playGame :: Game -> Int
playGame = sum . map playRound

play :: Text -> Int
play = playGame . mkGame

-- day02
fileName :: [Char]
fileName = "inputs/day02.txt"

day02 :: IO ()
day02 = TIO.readFile fileName >>= print . play
