{-# LANGUAGE OverloadedStrings #-}
module Day01 (day01) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortBy)
import Control.Applicative (liftA2)

parseNumber :: T.Text -> Int
parseNumber = read . T.unpack

computeSum :: [T.Text] -> Int
computeSum = sum . fmap parseNumber

day01Base :: ([Int] -> Int) -> T.Text -> Int
day01Base selector = selector . fmap (computeSum . T.lines) . T.splitOn "\n\n"

day01 :: IO ()
day01 = TIO.readFile "inputs/day01.txt" >>= print . liftA2 (,)
   (day01Base maximum) (day01Base (sum . take 3 . sortBy (flip compare)))
