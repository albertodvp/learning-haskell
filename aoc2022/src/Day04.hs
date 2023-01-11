{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day04 (day04) where

import           Protolude

import           Text.Megaparsec            (Parsec, runParser)
import           Text.Megaparsec.Char       (char)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Set                   as S

type SetFilter = (S.Set Int -> S.Set Int -> Bool)
type Parser = Parsec Void Text

setParser :: Parser (S.Set Int)
setParser = f <$> L.decimal <*> char '-' <*> L.decimal
  where
    f start _ end = S.fromList [start .. end]
lineParser :: Parser (S.Set Int, S.Set Int)
lineParser = f <$> setParser <*> char ',' <*> setParser
  where
    f s1 _ s2 = if S.size s1 <= S.size s2 then (s1, s2) else (s2, s1)

play :: SetFilter -> [Text] -> Int
play setFilter = length . filter (uncurry setFilter) . rights . map (runParser lineParser "")

filterP1 :: SetFilter
filterP1 = S.isSubsetOf
filterP2 :: SetFilter
filterP2 s1 s2 = not $ S.disjoint s1 s2

day04 :: IO ()
day04 = readFile "inputs/day04.txt" >>= print . liftA2 (,) (play filterP1) (play filterP2) . lines
