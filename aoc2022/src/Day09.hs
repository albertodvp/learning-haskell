module Day09 (module Day09) where

import Data.Bifunctor (first, second)
import Data.Foldable (foldl')
import Data.List (scanl')
import qualified Data.Set as S

data Command = U | R | D | L
type Index = (Int, Int)
data GameState = GameState {_head :: Index, _tail :: [Index], tailVisited :: S.Set Index} deriving (Show)

parseCommand :: Char -> Command
parseCommand c = case c of
    'U' -> U
    'R' -> R
    'D' -> D
    'L' -> L
    _ -> error "invalid input"

parseCommands :: String -> [Command]
parseCommands (c : ' ' : n) = parseCommand <$> replicate (read n) c

nextHeadMove :: Command -> Index -> Index
nextHeadMove c = case c of
    U -> second succ
    D -> second pred
    R -> first succ
    L -> first pred

moveTail :: Index -> Index -> Index
moveTail (headX, headY) tail@(tailX, tailY)
    | dx == 2 && dy == 2 = ((headX + tailX) `div` 2, (headY + tailY) `div` 2)
    | dx == 2 = ((headX + tailX) `div` 2, headY)
    | dy == 2 = (headX, (headY + tailY) `div` 2)
    | otherwise = tail
  where
    dx = abs $ headX - tailX
    dy = abs $ headY - tailY

moveTail' :: Index -> Index -> Index
moveTail' a b = b

move :: GameState -> Command -> GameState
move (GameState _ [] _) _ = error "invalid game"
move (GameState h t vis) c =
    let newHead = nextHeadMove c h
        newTail = tail $ scanl' moveTail newHead t
        newVis = S.insert (last newTail) vis
     in GameState newHead newTail newVis
mkInitGame :: Int -> GameState
mkInitGame ropeSize = GameState (0, 0) (replicate (ropeSize - 1) (0, 0)) $ S.singleton (0, 0)

play :: Int -> String -> Int
play ropeSize = S.size . tailVisited . foldl' move (mkInitGame ropeSize) . concatMap parseCommands . lines

day09 :: IO ()
day09 = readFile "inputs/day09.txt" >>= \t -> print (play 2 t, play 10 t)
