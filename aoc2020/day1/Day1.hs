type Resolver = [Int] -> Int

-- Correct answer: 440979
-- Time complexity: O(n) = n^2
findRes1 :: Resolver
findRes1 xs = head [x*y
                   | x <- xs,
                     y <- xs,
                     x + y == 2020, x /= y]

-- Correct answer: 82498112
-- Time complexity: O(n) = n^3
findRes2 :: Resolver
findRes2 xs = head [x*y*z
                   | x <- xs,
                     y <- xs,
                     z <- xs,
                     x + y + z == 2020, x /= y, y /= z, z /= x]

day1 :: Resolver -> String -> String
day1 p s = show $ p nums
  where
    nums = map read (lines s)

main :: IO()
main = interact $ day1 findRes2
