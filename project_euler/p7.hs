module P7 where

isPrime :: Integer -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime x = all ((/= 0) . rem x) [2..x-1]

primes :: [Integer]
primes = filter isPrime [1..]

p7 :: Integer
p7 = head $ drop 10000 primes
