{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day05(day05) where

import Protolude

import Text.Megaparsec
import Text.Megaparsec.Char (char, space)
import Data.Maybe (catMaybes)
import qualified Text.Megaparsec.Char.Lexer as L
-----------
-- CrateStack --
-----------
type Crate = Char

newtype Stack a = Stack [a]

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack $ x:xs

-------------
-- Parsing --
-------------
type Parser = Parsec Void Text
type CrateLine = [Maybe Crate]

parseCrate :: Parser (Maybe Crate)
parseCrate = Just <$> between (char '[') (char ']') L.charLiteral <|> const Nothing <$> (space >> space >> space)

parseCrateLine :: Parser CrateLine
parseCrateLine = m
  
  

mkCrateStacks :: [CrateLine] -> [Stack Crate]
mkCrateStacks = map (Stack . reverse . catMaybes) . transpose

-----------
-- Maing --
-----------

fileName :: [Char]
fileName = "inputs/ch05.txt"
day05 :: IO ()
day05 = readFile fileName >>= print . lines

