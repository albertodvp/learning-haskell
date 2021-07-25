-- |

module RollYourOwn where



import           RandomExample (Die, intToDie)
import           System.Random
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count g
      | sum > n   = count
      | otherwise =
        let (die, nextG) = randomR (1,6) g
        in go (sum + die) (count + 1) nextG



rs :: Functor f => Int -> f Int -> f Int
rs n mx = rollsToGetN n . mkStdGen <$> mx


rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 0 []
  where
    go :: Int -> Int -> [Int] -> StdGen -> (Int, [Die])
    go sum count results g
      | sum > n = (count, intToDie <$> results)
      | otherwise =
        let (dieN, newG) = randomR (1,6) g
        in go (sum + dieN) (count + 1) (results ++ [dieN]) newG


-- TODO
rollsCountLogged' :: Int -> StdGen -> (Int, [Die])
rollsCountLogged' = undefined
