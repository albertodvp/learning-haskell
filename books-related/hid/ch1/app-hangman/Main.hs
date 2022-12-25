-- | 

module Main(main) where

import Data.List (elemIndices)
import Control.Monad (when)
import System.Exit (exitSuccess)
data Game = Game { toGuess :: String, guessed :: String} deriving Show

replaceAt :: a -> Int -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt y 0 (_:xs) = y:xs
replaceAt y n (x:xs) = x:replaceAt y (n-1) xs

initGame :: String -> Game
initGame toGuess = let l = length toGuess
                   in Game toGuess (take l $ repeat '_')

playRound :: Char -> Game -> Game
playRound c (Game toGuess guessed) = let is = elemIndices c toGuess
                                         newGuessed = foldr (replaceAt c) guessed is
                                     in Game toGuess newGuessed

isEnded :: Game -> Bool
isEnded (Game toGuess guessed) = toGuess == guessed

playRoundIO :: Game -> IO (Game)
playRoundIO g = do
  putStrLn "Current game: "
  print g
  putStrLn "Try your guess: "
  (c:_) <- getLine
  return $ playRound c g
  
playIO :: Game -> IO ()
playIO g = do
  when (isEnded g) (putStrLn "Correct." >> exitSuccess)
  newG <- playRoundIO g
  playIO newG

main :: IO ()
main = do
  putStrLn "Pick a word: "
  w <- getLine
  playIO $ initGame w

