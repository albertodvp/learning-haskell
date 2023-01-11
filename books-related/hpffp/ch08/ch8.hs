-- ch8.hs
--
module Ch8 where

-- Bottom

f :: Bool -> Int
f True = 0
f _    = error "Error"


f' :: Bool -> Int
f' True = 0

-- TODO what's the type
--
--

f'' :: Bool -> Maybe Int
f'' False = Just 0
f'' _     = Nothing


-- TODO this is broken
--myFibonacci :: (Num a) => a -> a
--myFibonacci 0 = 1
--myFibonacci 1 = 1
--myFibonacci n = myFibonacci (n-1) + myFibonacci (n-2)

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


mapfib = map fibonacci . enumFromTo 0


type Numberator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numberator -> Denominator -> Quotient
dividedBy n d
  | n < d = 0
  | otherwise = dividedBy (n-d) d + 1


dividedBy' :: Integral a => a -> a -> (a,a)
dividedBy' num denom = go num denom 0
  where go n d count
         | n < d     = (count, n)
         | otherwise = go (n - d) d (count + 1)


recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum n
  | n == 0    = 0
  | otherwise = n + recursiveSum (n-1)


recursiveProd :: (Eq a, Num a) => a -> a -> a
recursiveProd x y
  | y == 0    = 0
  | y == 1    = x
  | otherwise = x + recursiveProd x (y-1)



-- TODO this does not work
-- data Integral a => DivResult a = Result (a, a) | DividedByZero deriving Show
data DivResult = Result (Integer, Integer) | DividedByZero deriving Show
dividedBy'' :: Integer -> Integer -> DivResult
dividedBy'' num den = go num den 0
  where go n d count
         | d == 0    = DividedByZero
         | n < d     = Result (count, n)
         | otherwise = go (n-d) d (count+1)




mc91 :: (Num a, Ord a) => a -> a
mc91 x
  | x >  100  = x - 10
  | x <= 100 = mc91(mc91(x+11))





-- module WordNumber

digitToWord :: Int -> Maybe String
digitToWord 0 = Just "zero"
digitToWord 1 = Just "one"
digitToWord 2 = Just "two"
digitToWord 3 = Just "three"
digitToWord 4 = Just "four"
digitToWord 5 = Just "five"
digitToWord 6 = Just "six"
digitToWord 7 = Just "seven"
digitToWord 8 = Just "eight"
digitToWord 9 = Just "nine"
digitToWord _ = Nothing


-- TODO
digits :: Int -> [Int]
digits x
  | x < 10    = [x]
  | otherwise = digits(d) ++ [m] where (d, m) = divMod x 10



