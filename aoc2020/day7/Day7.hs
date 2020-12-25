{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set   as S
import qualified Data.Text  as T

newtype Color = Color T.Text deriving (Eq, Show, Ord)
data Bag = Bag
  { getN     ::Int
  , getColor :: Color
  } deriving (Eq, Show, Ord)


type RuleMap = M.Map Color [Bag]

removeLastWord :: T.Text -> T.Text
removeLastWord = T.unwords . init . T.words

parseLine :: T.Text -> (Color, [Bag])
parseLine s = (container, contents)
  where
    [a, b] = T.splitOn " contain " s
    container = Color $ removeLastWord a
    mkBag ts = let s = T.words ts
                   n = read $ T.unpack $ head s
                   c = Color $ T.unwords $ init $ tail s
               in Bag n c


    contents = case T.init b of
      "no other bags" -> []
      xs              -> mkBag <$> T.splitOn ", " xs

parseLineColorsOnly :: T.Text -> (Color, [Color])
parseLineColorsOnly t = fmap getColor <$> parseLine t

contentToContainers :: [T.Text] -> RuleMap
contentToContainers = foldr f M.empty
  where
    f row m = let (container, contents) = parseLineColorsOnly row
              in foldr (g container) m contents
    g container content = M.insertWith (++) content [Bag 1 container]

containerToContents :: [T.Text] -> RuleMap
containerToContents = foldr f M.empty
  where
    f row = let (container, bags) = parseLine row
              in M.insert container bags


bfs :: Color -> RuleMap -> [Bag]
bfs c m = S.toList $ fst $ help (S.empty, S.singleton (Bag 1 c))
  where
    help :: (S.Set Bag, S.Set Bag) -> (S.Set Bag, S.Set Bag)
    help (visited, toVisit)
      | S.null toVisit = (visited, S.empty)
      | otherwise = help (visited', toVisit')
      where
        c = S.findMin toVisit
        validBags = S.fromList $ fromMaybe [] (m M.!? getColor c)
        toVisit' = S.deleteMin toVisit `S.union` validBags
        visited' = S.union  visited validBags

c = Color "shiny gold"

p1 :: [T.Text] -> Int
p1 = length . bfs c . contentToContainers

main :: IO ()
main = readFile "./input" >>= print . p1 . T.lines . T.pack

