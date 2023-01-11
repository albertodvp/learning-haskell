module P4 where
import           Data.List (sortBy)

-- 12321
-- 1 -> *10 + 2
mirrored :: Integer -> Integer
mirrored x = foldl (\a y -> 10*a + y) 0 (f x)
  where
    f 0 = []
    f x = x `rem` 10:f(x `quot` 10)

mirrored' :: Integer -> Integer
mirrored' x = mir 0 x
  where
    mir acc 0 = acc
    mir acc n = mir (acc * 10 + mod n 10) (div n 10)
-- isPalindrome :: Integer -> Bool
-- isPalindrome x = let y = show x in y == reverse y
isPalindrome  :: Integer -> Bool
--isPalindrome x = mirrored x == x
isPalindrome x = mirrored' x == x


r = [100..999]


p4 :: Integer
p4 = foldl1 max  [a*b | a <- r, b <- r, isPalindrome (a*b)]
