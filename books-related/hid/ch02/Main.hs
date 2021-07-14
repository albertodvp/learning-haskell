{-# LANGUAGE DeriveAnyClass #-}

module Main where

data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Show, CyclicEnum)
data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Bounded, Show)

-- rotate determine a new antenna direction after rotating
rotate :: Turn -> Direction -> Direction
rotate TNone   = id
rotate TLeft   = cpred
rotate TRight  = csucc
rotate TAround = cpred . cpred


every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

-- find a rotation to cha nge an orientation from the first given direction to the second one
orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2)
--  [TNone, TLeft, TRight, TAround]
--  [TNone .. TAround]
--  [minBound .. maxBound]
--  (enumFrom minBound)
  every

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl $ flip rotate

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl $ flip rotate

orientMany :: [Direction] -> [Turn]
orientMany ds@(_:_:_) = zipWith orient ds (tail ds)
orientMany _          = []

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

main :: IO ()
main = undefined

