{-# LANGUAGE OverloadedStrings #-}
module Day01 (day01) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortBy)
fileName :: [Char]
fileName = "inputs/day01.txt"

parseNumber :: T.Text -> Int
parseNumber = read . T.unpack

computeSum :: [T.Text] -> Int
computeSum = sum . fmap parseNumber

day01Base :: ([Int] -> Int) -> T.Text -> Int
day01Base selector = selector . fmap computeSum . fmap T.lines . T.splitOn "\n\n"

day01p1 :: IO ()
day01p1 = TIO.readFile fileName >>= print . day01Base maximum

day01p2 :: IO ()
day01p2 = TIO.readFile fileName >>= print . day01Base (sum . take 3 . sortBy (flip compare))

day01 :: IO ()
day01 = day01p1 >> day01p2
