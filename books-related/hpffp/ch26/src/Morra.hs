-- |

module Morra where

import           Control.Monad
import           Control.Monad.Trans.State
import           System.Random
-- The state being accumulated is the score of the player and the computer AI oponent. User StateT and IO
-- Player is odds, computer is evens
-- On exit, report the scores for the player and the computer

-- 1) Make the computer choose its play randomly
-- 2) Add human vs hyman mode to the game with interstitial screens between input prompts (so the players can change out of the hot seat without seeing the other player's answer
-- 3) Improve the computer AI slightly by making it remember 3-grams of the player's behavior, adjusting its answer insterd of deciding randomly when the player's behavior mathces a known behavior
-- Steps

gen :: StdGen
gen = mkStdGen 42

data Score = Score {
  playerPoints :: Int,
  otherPoints  :: Int,
  randGen      :: StdGen
} deriving Show

data Mode = Ai | Opp deriving Show
type Game = StateT Score IO ()
type PointsToWin = Int


parsePointsToWin :: IO PointsToWin
parsePointsToWin = do
  putStrLn "How many points to win?"
  read <$> getLine

parseMode :: IO Mode
parseMode =  putStrLn "Which mode are you playing in [Ai, Opp]?" >> getLine >>= supp
  where
    supp "Ai"  = return Ai
    supp "Opp" = return Opp
    supp _     = error "Unsupported mode"

playerWins :: Int -> Int -> Bool
playerWins x y = odd $ x + y

updateScore :: Score -> Int -> Int -> StdGen -> Score
updateScore (Score p o _) x y newGen  = if playerWins x y then pwScore else owScore
  where
    pwScore = Score (p+1) o newGen
    owScore = Score p (o+1) newGen

isGameFinished :: PointsToWin -> Score -> Bool
isGameFinished pts (Score p o _) = p >= pts || o >= pts

playerMove :: IO Int
playerMove = do
  putStrLn "What's your move"
  move <- read <$> getLine
  when (move < 1 || move > 5) (error "Invalid input")
  return move

playGameAiRound :: PointsToWin -> Game
playGameAiRound pts = StateT $ \s -> do
  pm <- playerMove
  let (om, newGen) = uniformR (1,5) (randGen s)
  putStrLn $ "DEBUG: Player: " ++ show pm ++ ", other: " ++ show om
  let newScore = updateScore s pm om newGen
  putStrLn $ "DEBUG: Score: " ++ show newScore
  return ((), newScore)

playGameAi :: PointsToWin -> IO Score
playGameAi pts = go $ pure (Score 0 0 gen)
  where
    go :: IO Score -> IO Score
    go iscore = do
      score <- iscore
      if isGameFinished pts score
        then iscore
        else go $ execStateT (playGameAiRound pts) score

-- type Game = StateT Score IO ()
-- StateT Score IO ()
-- IO ()
playGame :: Mode -> PointsToWin -> IO Score
playGame Ai pts  = playGameAi pts

playGame Opp pts = undefined

main :: IO ()
main = do
  putStrLn "Welcome in!"
  mode <- parseMode
  pts <- parsePointsToWin
  (Score pp op _) <- playGame mode pts
  putStrLn $ "Score: player - " ++ show pp ++ " other - " ++ show op
  putStrLn "Game ends!"
