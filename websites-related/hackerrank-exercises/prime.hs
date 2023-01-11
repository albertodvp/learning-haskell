isPrime :: Integral a => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = go 2
  where
    go n
      | n > floor (sqrt (fromIntegral x)) = True
      | otherwise   =
          mod x n /= 0 && go (n + 1)
-- >>> isPrime 1
-- True
-- >>> isPrime 2
-- True
-- >>> isPrime 9
-- False
-- >>> isPrime 19
-- True
-- >>> isPrime 199
-- True

primes = filter isPrime [1..]
-- >>> take 100 primes
-- [1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523]

main = do
         putStrLn "Primes!"
         putStrLn "Type a number..."
         x <- getLine
         let n = read x :: Int
         putStrLn $ "Checking if " ++ show n ++ " is prime..."
         putStrLn $ if isPrime n then "It's prime!" else "It's not prime!"


