-- |

module RandomExample where


import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random


data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 intger " ++ show x



rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1,6) s
      (d2, s2) = randomR (1,6) s1
      (d3, s3) = randomR (1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDieThreeTimes' :: Int -> (Die, Die, Die)
rollDieThreeTimes' x = do
  let s = mkStdGen x
      (d1, s1) = randomR (1,6) s
      (d2, s2) = randomR (1,6) s1
      (d3, s3) = randomR (1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)


rollDieThreeTimes'' :: StdGen -> (Die, Die, Die)
rollDieThreeTimes'' g = do
  let (d1, s1) = randomR (1,6) g
      (d2, s2) = randomR (1,6) s1
      (d3, s3) = randomR (1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)


-- With StateT

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)


rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))


rollDieThreeTimes''' :: State StdGen (Die, Die, Die)
rollDieThreeTimes''' = liftA3 (,,) rollDie rollDie rollDie

-- NOTE THIS DOES NOT WORK
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


-- Keep on rolling

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
         let (die, nextGen) = randomR (1,6) gen
         in go (sum + die) (count + 1) nextGen

rs = rollsToGetTwenty .  mkStdGen

