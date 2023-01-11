module P2 where

fib1 :: [Int]
fib1 = 0 : 1 : zipWith (+) fib1 (tail fib1)

fib2 :: [Int]
fib2 = scanl (+) 0 (1:fib2)

p2 :: Int
p2 = sum $ w f
  where
    w = takeWhile (< 4000000)
    f = filter even fib1

