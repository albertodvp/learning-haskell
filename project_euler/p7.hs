module P7 where

divPrimeBy :: Integer -> Integer -> Bool
divPrimeBy x by = by == x || x `rem` by /= 0

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = all (divPrimeBy x) (takeWhile (<=x `quot` 2)  primes)

primes :: [Integer]
primes = filter isPrime [2..]

primes' :: [Integer]
primes' = f [2..]
  where
    f (p:xs) = p : f (filter (\x -> mod x p /= 0) xs)


p7 :: Integer
p7 = primes !! 10000

