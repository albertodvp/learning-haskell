module P3 where

import Data.List (find)

isPrime :: Integer -> Bool
isPrime x = not $ any ((==0) . rem x) [2..x-1]

divsSupp :: Integer -> Integer -> [Integer]
divsSupp _   1 = []
divsSupp d n = case rem n d of
  0 -> d : (divsSupp d (n `quot` d))
  _ -> divsSupp (d+1) n


divs :: Integer -> [Integer]
divs n= 1:(divsSupp 2 n)

p3 :: Integer
p3 = head $ reverse $ divs 600851475143
