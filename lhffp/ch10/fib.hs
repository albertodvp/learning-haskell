module Fibonacci where

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x


fibs20 = take 20 fibs

fibsLess100 = takeWhile (<100) fibs
