{-# LANGUAGE NoImplicitPrelude #-}

module Day08 (module Day08) where

import qualified Data.Matrix as M
import Protolude
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Char (eol, numberChar)

type Parser = Parsec Void Text
type Trees = M.Matrix Char
type Index = (Int, Int)
type Solver = Trees -> [Index] -> Int

parseGame :: Parser Trees
parseGame = M.fromLists <$> many (many numberChar <* eol)

getTargetIndexes :: Index -> M.Matrix a -> [[Index]]
getTargetIndexes (ri, ci) trees = [up, right, down, left]
  where
    up = [(i, ci) | i <- [ri - 1, ri - 2 .. 1]]
    down = [(i, ci) | i <- [ri + 1 .. M.nrows trees]]
    left = [(ri, i) | i <- [ci - 1, ci - 2 .. 1]]
    right = [(ri, i) | i <- [ci + 1 .. M.ncols trees]]

compareVisible :: Ord a => M.Matrix a -> Index -> Index -> Bool
compareVisible trees base candidate = let getValue = (M.!) trees in getValue candidate < getValue base

isVisible :: Ord a => M.Matrix a -> Index -> Bool
isVisible trees i = or $ all (compareVisible trees i) <$> getTargetIndexes i trees

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 f (x : xs)
    | not (f x) = [x]
    | otherwise = x : takeWhile1 f xs

scenicScore :: Ord a => M.Matrix a -> Index -> Int
scenicScore trees i = product $ length . takeWhile1 (compareVisible trees i) <$> getTargetIndexes i trees

p1 :: Solver
p1 trees = length . filter (isVisible trees)
p2 :: Solver
p2 trees = maximum . map (scenicScore trees)

play :: Solver -> Trees -> Int
play solver trees = solver trees [(i, j) | i <- [1 .. M.nrows trees], j <- [1 .. M.ncols trees]]

day08 :: IO ()
day08 =
    readFile "inputs/day08.txt" >>= \t -> case parse parseGame "" t of
        Left err -> print $ errorBundlePretty err -- NOTE: assume the output is syntatically correct
        Right trees -> print (play p1 trees, play p2 trees)
