module P4 where
import Data.List (sort)
r = [100..999]
rr = reverse $ sort [a*b | a <- r, b <- r]

p4 :: Integer
p4 = read $ head $ filter (\x -> x == reverse x) $ map show $ rr
