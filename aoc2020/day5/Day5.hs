import           Control.Applicative
import           Data.List           (sort)
import qualified Data.Map            as M
import           Data.Maybe          (fromJust)

type Op = Double -> Double -> Double

data Position = Position
  { getRow :: Integer
  , getCol :: Integer
  }

getId :: Position -> Integer
getId p = r * 8 + c
  where
    r = getRow p
    c = getCol p

ops :: M.Map Char Op
ops = M.fromList
  [ ('F',flip (-))
  , ('B', (+))
  , ('L',flip (-))
  , ('R', (+))
  ]

 -- Here I assume the "input" is safe
pickOp :: Char -> Op
pickOp = (M.!) ops

pows :: Int -> [Double]
pows bits = reverse $ take bits $ 0.5: ((2^) <$> [0..])

parse :: Int -> String -> Integer
parse bits s = round $ foldl f start (os <*> ps)
  where
    f = flip ($)
    start = 2^(bits-1) - 0.5
    os = ZipList $ pickOp <$> s
    ps = ZipList $ pows bits



rowBits :: Int
rowBits = 7

maxBits :: Int
maxBits = 10

parseLine :: String -> Position
parseLine s = Position row col
  where
    row = parse rowBits rowS
    rowS = take rowBits s
    col = parse (maxBits - rowBits) colS
    colS = drop rowBits s

p1 :: [Position] -> Integer
p1 = maximum . fmap getId

p2 :: [Position] -> Integer
p2 xs = fst $ head $ dropWhile (uncurry (/=)) pairs
  where
    presentIDs = sort $ fmap getId xs
    firstID = head presentIDs
    validIDs = [firstID,firstID+8..]
    pairs = zip validIDs presentIDs


main :: IO ()
main = readFile "./input" >>= print . p2 . fmap parseLine . lines

