module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Enum: enumerate elements
-- Bounded: specify maximum and minimum bounds
-- Eq: allow to test for equality
data Direction = North | East | South | West deriving (Eq, Enum, Show, Bounded)
data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Show, Bounded)
instance CyclicEnum Direction
instance CyclicEnum Turn

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = csucc . csucc

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) $ every

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' d = flip rotate d . mconcat

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl $ flip rotate

orientMany :: [Direction] -> [Turn]
orientMany ds@(_:_:_) = zipWith orient ds (tail ds)
orientMany _ = []

instance Semigroup Turn where
  TNone <> t = t
  TLeft <> TLeft = TAround
  TLeft <> TRight = TNone
  TLeft <> TAround = TRight
  TRight <> TRight = TAround
  TRight <> TAround = TLeft
  TAround <> TAround = TNone
  t1 <> t2 = t2 <> t1
  
instance Monoid Turn where
  mempty = TNone

-- When you use a new type, check for typeclass instances of that type

-- Cayley tables: describe  the structure of a finite group by arranging all the possible products of all the group's element
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d
