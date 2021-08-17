-- |

module FizzBuzz where

import           Control.Monad
import           Control.Monad.Trans.State



fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

main :: IO()
main = mapM_ (putStrLn . fizzBuzz) [1..100]

-- TODO write with traverse
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []


addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)


main' :: IO()
main' = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
