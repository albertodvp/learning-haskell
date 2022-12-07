{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Protolude hiding (some, many, head)
import Prelude (id)
import Data.List ((!!))
import Text.Megaparsec
import Text.Megaparsec.Char (char, string, eol)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (pack)
type Crate = Char

newtype Stack a = Stack [a] deriving (Show)

data Move = Move {mFrom :: Int, mTo :: Int} deriving (Eq, Show)
type Parser = Parsec Void Text
type CrateLine = [Maybe Crate]

pop :: Stack a -> Either Text (a, Stack a)
pop (Stack []) = Left "Invalid move, empty stack"
pop (Stack (x:xs)) = Right (x, Stack xs)
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack $ x:xs
head :: Stack a -> Either Text a
head s = fst <$> pop s
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n y xs = take n xs ++ [y] ++ drop (n+1) xs
mkCrateStacks :: [CrateLine] -> [Stack Crate]
mkCrateStacks = map (Stack . reverse . catMaybes) . transpose
mkMoves :: Int -> Int -> Int -> [Move]
mkMoves times from1Based to1Based = replicate times $ Move (from1Based - 1) (to1Based - 1)

parseCrate :: Parser (Maybe Crate)
parseCrate = Just <$> between (char '[') (char ']') L.charLiteral <|> Nothing <$ string "   "
parseCrateLine :: Parser CrateLine
parseCrateLine = sepBy parseCrate $ char ' '

parseMoves :: Parser [Move]
parseMoves = mkMoves <$> (string "move " >> L.decimal) <*> between (string " from ") (string " to ") L.decimal <*> L.decimal

parseGame :: Parser ([Stack Crate], [Move])
parseGame = do
  sc <- some (parseCrateLine <* eol)
  _ <- takeWhileP Nothing (/= '\n') >> eol >> eol
  moves <- some (parseMoves  <* eol)
  return (mkCrateStacks sc, (concat moves))

move :: Move -> [Stack Crate] -> Either Text [Stack Crate]
move move stacks = do
  let fromIndex = mFrom move
  let toIndex = mTo move
  (x, newFromStack) <- pop $ stacks !! fromIndex
  let newToStack = push x $ stacks !! toIndex
  return $ replaceAt fromIndex newFromStack (replaceAt toIndex newToStack stacks)

-- TODO: is there a better way to write this?
moves :: [Stack Crate] -> [Move] -> Either Text [Stack Crate]
moves stacks [] = Right stacks
moves stacks (m:ms) = do
  newStacks <- move m stacks
  moves newStacks ms
  
play :: Text -> Text
play t = either (show . bundleErrors) f (runParser parseGame "" t)
  where
    f (stacks, ms) = either id pack $ do
      newStacks <- moves stacks ms
      return $ rights $ map head newStacks
      
fileName :: [Char]
fileName = "inputs/day05.txt"
day05 :: IO ()
day05 = readFile fileName >>= print . play
