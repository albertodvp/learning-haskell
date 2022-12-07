{-# LANGUAGE NoImplicitPrelude #-}

module Day03 (day03) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (unpack)
import Protolude
import qualified Prelude

type Rucksack = (S.Set Char, S.Set Char)

priorities :: M.Map Char Int
priorities = M.fromList $ zip chars nums
  where
    nums = [1 .. 26] ++ [27 .. 52]
    chars = ['a' .. 'z'] ++ ['A' .. 'Z']

mkRucksack :: Text -> Rucksack
mkRucksack t = (S.fromList d1, S.fromList d2)
  where
    s = unpack t
    l = length s
    (d1, d2) = splitAt (div l 2) s

getErrorItem :: Rucksack -> Char
getErrorItem = S.elemAt 0 . uncurry S.intersection
getGroupBadgeItem :: [Rucksack] -> Char
getGroupBadgeItem = S.elemAt 0 . Prelude.foldl1 S.intersection . map (uncurry S.union)

getPriority :: Char -> Int
getPriority = (M.!) priorities
getErrorPriority :: Rucksack -> Int
getErrorPriority = getPriority . getErrorItem
getGroupPriority :: [Rucksack] -> Int
getGroupPriority = getPriority . getGroupBadgeItem

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = f : groupsOf n s
  where
    (f, s) = splitAt n xs

playP1 :: [Text] -> Int
playP1 = sum . map (getErrorPriority . mkRucksack)
playP2 :: [Text] -> Int
playP2 = sum . map getGroupPriority . groupsOf 3 . map mkRucksack

day03 :: IO ()
day03 = readFile "inputs/day03.txt" >>= print . liftA2 (,) playP1 playP2 . lines
