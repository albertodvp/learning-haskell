{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day04(day04) where

import Protolude
import Data.Text.Read (decimal)
import Data.Text (splitOn)
import qualified Data.Set as S

type SetFilter = (S.Set Int -> S.Set Int -> Bool) 
-- NOTE: temporary solution before usage of parsers
parseInt :: Text -> Int
parseInt = either undefined fst . decimal

mkRange :: Text -> S.Set Int
mkRange t = S.fromList [parseInt start..parseInt end]
  where
    [start, end] = splitOn "-" t

play :: SetFilter -> [Text] -> Int
play setFilter= length . filter f . map (sortOn S.size . map mkRange . splitOn ",")
  where
    f [small, big] = setFilter small big

fileName :: [Char]
fileName = "inputs/ch04.txt"

filterP1 :: SetFilter
filterP1 = S.isSubsetOf

filterP2 :: SetFilter
filterP2 s1 s2 = not $ S.disjoint s1 s2

day04 :: IO ()
--day04 = readFile fileName >>= print . play filterP1 . lines
day04 = readFile fileName >>= print . play filterP2 . lines

