{-# LANGUAGE OverloadedStrings #-}

module Day02(day02) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative (liftA2)

data Choice = Rock | Paper | Scissors deriving Eq
data Result = Lose | Draw | Win
type Round = (Choice, Choice)

parseResult :: Char -> Result
parseResult 'X' = Lose
parseResult 'Y' = Draw
parseResult 'Z' = Win
parseResult _ = error "error"

parseChoice :: Char -> Choice
parseChoice c
  | c `elem` ("AX" :: String) = Rock
  | c `elem` ("BY" :: String) = Paper
  | c `elem` ("CZ" :: String) = Scissors
  | otherwise = error "error"

parseRoundP1 :: Text -> Round
parseRoundP1 t = case T.unpack t of
                   [c1, ' ', c2] -> (parseChoice c1, parseChoice c2)
                   _ -> error "Wrong input"

parseRoundP2 :: Text -> Round
parseRoundP2 t = case T.unpack t of
                   [c1, ' ', c2] -> let cc1 = parseChoice c1
                                    in (cc1, choiceFromResult (parseResult c2) cc1)
                   _ -> error "Wrong input"

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

play :: (Text -> Round) -> Text -> Int
play roundParser = (sum . map playRound) . map roundParser . T.lines

day02 :: IO ()
day02 = TIO.readFile "inputs/day02.txt" >>= print . liftA2 (,) (play parseRoundP1) (play parseRoundP2)
