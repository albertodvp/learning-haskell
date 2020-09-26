module P6 where

range :: [Integer]
range = [1..100]

sum_squares :: Integer
sum_squares = sum $ map (^2) range

square_sums :: Integer
square_sums = (sum range) ^ 2

res :: Integer
res = square_sums - sum_squares

p6 :: Integer
p6 = sum [ 2 * x * y | x <- range, y <- range , x > y]
  


