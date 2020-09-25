module P5 where

range :: [Integer]
range = [1..20]

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = case a `rem` b of
  0 -> b
  r -> b `mcd` r

mcm :: Integer -> Integer -> Integer
mcm a b = a * b `quot` (a `mcd` b)


p5 :: Integer
p5 = foldl1 mcm range
-- p5 = foldl1 lcm range
