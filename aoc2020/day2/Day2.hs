import           Data.List.Split

-- Example:
-- 17-19 p: pwpzpfbrcpppjppbmppp
data Row = Row {
  x        :: Int,
  y        :: Int,
  char     :: Char,
  password :: String
  } deriving Show

mkRow :: String -> Row
mkRow s = Row x' y' c p
  where
    [x,y,cs,_,p] = splitOneOf "- :" s
    x' = read x
    y' = read y
    c = head cs

countOcc :: Eq a => a -> [a] -> Int
countOcc x = length . filter (x==)


-- Correct answer: 620⏎
valid1 :: Row -> Bool
valid1 (Row lb ub c pass) = (lb <= cs) && (cs <= ub)
  where
    cs = countOcc c pass

-- a xor b = a and not b or b =nd not a
-- Correct answer: 727
valid2 :: Row -> Bool
valid2 (Row p1 p2 c pass) = countOcc c [pass !! (p1-1), pass !! (p2-1)] == 1


day2 :: String -> String
day2 = show . length . filter valid2 . fmap mkRow . lines

main :: IO()
main = interact day2
