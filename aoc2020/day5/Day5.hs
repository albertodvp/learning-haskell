import           Control.Applicative
import           Control.Monad       (guard)
import           Data.List           (nub, sort)
import qualified Data.Map            as M
import           Data.Maybe          (fromJust)

type Op = Double -> Double -> Double

data Position = Position
  { getRow :: Int
  , getCol :: Int
  }

getID :: Position -> Int
getID p = r * 8 + c
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

parse :: Int -> String -> Int
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

p1 :: [Position] -> Int
p1 = maximum . fmap getID

validIDs :: [Int]
validIDs = nub $ getID <$> liftA2 Position [0..2^rowBits] [0..2^(maxBits - rowBits)]

p2 :: [Position] -> Int
p2 xs = fst $ head $ dropWhile (uncurry (==)) pairs
  where
    presentIDs = sort $ fmap getID xs
    firstID = head presentIDs
    pairs = zip (filter (>= firstID) validIDs) presentIDs



main :: IO ()
main = readFile "./input" >>= print . p2 . fmap parseLine . lines

