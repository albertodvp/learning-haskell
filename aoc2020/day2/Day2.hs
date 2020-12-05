import           Data.Functor    ((<&>))
import           Data.List.Split

data Row = Row {
  x        :: Int,
  y        :: Int,
  char     :: Char,
  password :: String
  } deriving Show

type Input = [Row]
type Resolver = Input -> Int


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

-- Correct answer: 727
valid2 :: Row -> Bool
valid2 (Row p1 p2 c pass) = countOcc c [pass !! (p1-1), pass !! (p2-1)] == 1


resolvers :: [Resolver]
resolvers = (length .) . filter <$> [valid1, valid2]


inputFile :: FilePath
inputFile = "./input"


inputs :: IO Input
inputs = fmap mkRow . lines <$> readFile inputFile


main :: IO()
main = inputs >>= print . (resolvers <&>) . flip ($)
