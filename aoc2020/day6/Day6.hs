import           Data.List       (intersect, union)
import           Data.List.Split (splitOn)

p1 :: Eq a => [a] -> [a] -> [a]
p1 = union

p2 :: Eq a => [a] -> [a] -> [a]
p2 = intersect

main :: IO ()
main = readFile "./input" >>=  print . sum . map (length . foldr1 p2 . lines) . splitOn "\n\n"
