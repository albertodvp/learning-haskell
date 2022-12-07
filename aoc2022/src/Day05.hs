{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day05 (day05) where

import Protolude hiding (some, many, head)
import Prelude (id)
import Data.List ((!!))
import Text.Megaparsec
import Text.Megaparsec.Char (char, string, eol)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (pack)
type Crate = Char

newtype Stack a = Stack [a] deriving (Show)
data Move = Move {mFrom :: Int, mTo :: Int, mTimes :: Int} deriving (Eq, Show)
type Parser = Parsec Void Text
type CrateLine = [Maybe Crate]

pop :: Int -> Stack a -> (Stack a, Stack a)
pop n (Stack xs) = (Stack $ take n xs, Stack $ drop n xs)
push :: Stack a -> Stack a -> Stack a
push (Stack xs) (Stack ys) = Stack $ xs ++ ys
head :: Stack a -> Either Text a
head (Stack []) = Left "Empty stack"
head (Stack (x:_)) = Right x
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n y xs = take n xs ++ [y] ++ drop (n+1) xs
mkCrateStacks :: [CrateLine] -> [Stack Crate]
mkCrateStacks = map (Stack . catMaybes) . transpose

move :: [Stack Crate] -> Move  -> [Stack Crate]
move ss m = replaceAt fromIndex newFromStack (replaceAt toIndex newToStack ss)
  where
    fromIndex = mFrom m
    toIndex = mTo m
    times = mTimes m
    (popedStack, newFromStack) = pop times $ ss !! fromIndex
    newToStack = push popedStack $ ss !! toIndex

moves  :: [Stack Crate] -> [Move] -> [Crate]
moves ss = rights . map head . foldl move ss

parseCrate :: Parser (Maybe Crate)
parseCrate = Just <$> between (char '[') (char ']') L.charLiteral <|> Nothing <$ string "   "
parseCrateLine :: Parser CrateLine
parseCrateLine = sepBy parseCrate $ char ' '

parseMoveLineP1 :: Parser [Move]
parseMoveLineP1 = do
  _ <- string "move "
  times <- L.decimal
  from1Based <-  between (string " from ") (string " to ") L.decimal
  to1Based <- L.decimal
  return $ replicate times $ Move (from1Based - 1) (to1Based - 1) 1
parseMoveLinesP1 :: Parser [Move]
parseMoveLinesP1 = concat <$> some (parseMoveLineP1 <* eol)
parseMoveLineP2 :: Parser Move
parseMoveLineP2 = do
  _ <- string "move "
  times <- L.decimal
  from1Based <-  between (string " from ") (string " to ") L.decimal
  to1Based <- L.decimal
  return $ Move (from1Based - 1) (to1Based - 1) times
parseMoveLinesP2 :: Parser [Move]
parseMoveLinesP2 = some (parseMoveLineP2 <* eol)

parseGame :: Parser [Move] -> Parser ([Stack Crate], [Move])
parseGame parserMoves = do
  sc <- some (parseCrateLine <* eol)
  _ <- takeWhileP Nothing (/= '\n') >> eol >> eol
  ms <- parserMoves
  return (mkCrateStacks sc, ms)

finalParser :: Parser [Move] -> Parser [Crate]
finalParser parserMoves = uncurry moves <$> parseGame parserMoves

play  :: Parser [Move] -> Text -> Text
play parserMoves = pack . either errorBundlePretty id . parse (finalParser parserMoves) ""

day05 :: IO ()
day05 = readFile "inputs/day05.txt" >>= print . liftA2 (,) (play parseMoveLinesP1) (play parseMoveLinesP2)
