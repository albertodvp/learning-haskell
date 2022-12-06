{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day05(day05) where

import Protolude hiding (some, many, head)
import Prelude (error)
import Data.List ((!!))
import Text.Megaparsec
import Text.Megaparsec.Char (char, string, eol)

import qualified Text.Megaparsec.Char.Lexer as L

-- NOTE: 'undefined' on code represents the assumptions on the input data (always legic and clean),
-- these assumptions could have been encoded in more precise types and/or functions
-- but I preferred to show these explicitly. It's likely I'll change the approach
-- tomorrow ¯\_(ツ)_/¯

type Crate = Char

newtype Stack a = Stack [a] deriving (Show)

data Move = Move {mFrom :: Int, mTo :: Int} deriving (Eq, Show)

type Parser = Parsec Void Text
type CrateLine = [Maybe Crate]

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack $ x:xs

head :: Stack a -> Maybe a
head s = fst <$> pop s


replaceAt :: Int -> a -> [a] -> [a]
replaceAt n y xs = take n xs ++ [y] ++ drop (n+1) xs


ss :: Parser Char
ss = char ' '

parseCrate :: Parser (Maybe Crate)
parseCrate = Just <$> between (char '[') (char ']') L.charLiteral <|> const Nothing <$> (ss >> ss >> ss)

parseCrateLine :: Parser CrateLine
parseCrateLine = sepBy parseCrate $ char ' '

parseMoves :: Parser [Move]
parseMoves = do
  _ <- string "move "
  times <- L.decimal
  from <-  between (string " from ") (string " to ") L.decimal
  to <- L.decimal
  return $ replicate times $ Move (from - 1) (to-1)

mkCrateStacks :: [CrateLine] -> [Stack Crate]
mkCrateStacks = map (Stack . reverse . catMaybes) . transpose

inputParser :: Parser ([Stack Crate], [Move])
inputParser = do
  sc <- some (parseCrateLine <* eol)
  moves <- many parseMoves
  return (mkCrateStacks sc, (concat moves))

-----------
-- Logic --
-----------

 -- NOTE: here I assume there are a only legit moves
move :: Move -> [Stack Crate] -> [Stack Crate]
move  move stacks = let fromIndex = mFrom move
                        toIndex = mTo move
                        (x, newFromStack) = fromMaybe (error "empty stack (should be illegal)") $ pop $ stacks !! fromIndex
                        newToStack = push x $ stacks !! toIndex
                    in replaceAt fromIndex newFromStack (replaceAt toIndex newToStack stacks)
                       

moves :: [Stack Crate] -> [Move] -> [Stack Crate]
moves stacks = foldr move stacks


play :: Text -> [Crate]
play t= fromRight (error "parsing error (should be impossible)") $ runParser (catMaybes . map head . uncurry moves <$> inputParser) "" t

-----------
-- Main --
-----------

fileName :: [Char]
fileName = "inputs/day05.txt"
day05 :: IO ()
-- day05 = readFile fileName >>= print . play
day05 = readFile fileName >>= parseTest inputParser
