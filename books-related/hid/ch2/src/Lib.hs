{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}


module Lib (module Lib) where

import           Control.Monad          (replicateM, unless)
import           Data.List
import           Fmt
import           System.Exit            (exitFailure)
import           System.Random.Stateful


-- Enum: enumerate elements
-- Bounded: specify maximum and minimum bounds
-- Eq: allow to test for equality
data Direction = North | East | South | West deriving (Eq, Enum, Show, Bounded)
data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Show, Bounded)

-- Broken
instance Ord Turn

instance CyclicEnum Direction
rotate :: Turn -> Direction -> Direction
rotate TNone   = id
rotate TLeft   = cpred
rotate TRight  = csucc
rotate TAround = csucc . csucc

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

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
orientMany _          = []

instance Semigroup Turn where
  TNone <> t         = t
  TLeft <> TLeft     = TAround
  TLeft <> TRight    = TNone
  TLeft <> TAround   = TRight
  TRight <> TRight   = TAround
  TRight <> TAround  = TLeft
  TAround <> TAround = TNone
  t1 <> t2           = t2 <> t1

instance Monoid Turn where
  mempty = TNone

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

-- IO

deriving instance Read Direction
deriving instance Read Turn

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fp = do
  f <- readFile fp
  let turns = map read $ lines f
      finalDir = rotateMany' dir turns
      dirs = rotateManySteps dir turns
  fmtLn $ "Final direction: " +|| finalDir ||+ ""
  fmt $ nameF "Intermediate directions" (unwordsF dirs)

orientFromFile :: FilePath -> IO ()
orientFromFile fp = do
  f <- readFile fp
  let dirs = map read (lines f)
      turns = orientMany dirs
  fmt $ nameF "All turns" (unwordsF turns)


instance Buildable Direction where
  build North = "N"
  build East  = "E"
  build South = "S"
  build West  = "W"

instance Buildable Turn where
  build TNone   = "--"
  build TLeft   = "<-"
  build TRight  = "->"
  build TAround = "||"


instance UniformRange Direction where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
    pure $ toEnum res

instance Uniform Direction where
  uniformM = uniformRM (minBound, maxBound)

-- Equals? Is there a way to write this once?
-- instance CyclicEnum a where
instance UniformRange Turn where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
    pure $ toEnum res

instance Uniform Turn where
  uniformM = uniformRM (minBound, maxBound)

uniformIO :: Uniform a => IO a
uniformIO = getStdRandom uniform

uniformsIO :: Uniform a => Int -> IO [a]
uniformsIO n = replicateM n uniformIO

randomTurns :: Int -> IO [Turn]
randomTurns = uniformsIO

randomDirections :: Int -> IO [Direction]
randomDirections = uniformsIO

writeRandomFile :: (Uniform a, Show a) => Int -> (Int -> IO [a]) -> FilePath -> IO ()
writeRandomFile n gen fname = do
  xs <- gen n
  writeFile fname $ unlines $ map show xs


-- Tests
test_allTurnsInUse =
  sort (nub [ orient d1 d2 | d1 <- every, d2 <- every ])
   == every

test_rotationsMonoidAgree :: [Turn] -> Bool
test_rotationsMonoidAgree ts = and [rotateMany d ts == rotateMany' d ts | d <- every]

test_orientRotateAgree :: [Direction] -> Bool
test_orientRotateAgree []       = True
test_orientRotateAgree ds@(d:_) = ds == rotateManySteps d (orientMany ds)

main :: IO ()
main = do
  ds <- randomDirections 1000
  ts <- randomTurns 1000
  unless (and [test_allTurnsInUse, test_orientRotateAgree ds, test_rotationsMonoidAgree ts]) exitFailure



-- When you use a new type, check for typeclass instances of that type

-- Cayley tables: describe  the structure of a finite group by arranging all the possible products of all the group's element

-- deriving instance StandaloneDeriving

-- The standard generator is splittable, meaning that we can produce two generators from the one we have and use them independently.

-- RandomGen vs StaatefulGen


-- Test mi va in e
