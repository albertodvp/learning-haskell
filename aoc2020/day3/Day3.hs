import           Data.Functor ((<&>))

type Slope = Float
type Row = String
type Index = Int


prepMap :: [(Slope, Row)] -> [(Row, Index)]
prepMap = map ((,) . snd <*> round . fst)


candidateMap :: Slope -> [Row] -> [(Row, Index)]
candidateMap s = prepMap . filter (((==) . floor <*> ceiling) . fst) . zip [0,s..] . map cycle


trees :: Slope -> [Row] -> Int
trees s = length . filter (== '#') . map (uncurry (!!)) . candidateMap s


main :: IO ()
main = lines <$> readFile "./input" >>= print . product . ((trees <$> [1,3,5,7,0.5]) <&>) . flip ($)

