module P3 where

import Data.List (find)

divBy :: Integer -> Integer -> Bool
divBy x = (==0) . (rem x)

factors :: Integer -> [Integer]
factors x = filter (divBy x) [1..x]

isPrime :: Integer -> Bool
isPrime x = not $ any (divBy x) [2..x-1]


primeFactors :: Integer -> [Integer]
primeFactors = filter isPrime . factors

p3 :: Integer
p3 = head $ reverse $ primeFactors 600851475143
