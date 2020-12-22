{-# LANGUAGE OverloadedStrings #-}
import           Data.List  (nub)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text  as T
newtype Color = Color T.Text deriving (Eq, Show, Ord)

type RuleMap = M.Map Color [Color]

removeLastWord :: T.Text -> T.Text
removeLastWord = T.unwords . init . T.words

parseLine :: T.Text -> (Color, [Color])
parseLine s = (container, contents)
  where
    [a, b] = T.splitOn " contain " s
    container = Color $ removeLastWord a
    contents = case T.init b of
      "no other bags" -> []
      _ -> map (Color . T.drop 2 . removeLastWord) (T.splitOn ", " (T.init b))

-- containerToContents :: [T.Text] -> RuleMap
-- containerToContents = M.fromList . map parseLine

contentToContainers :: [T.Text] -> RuleMap
contentToContainers = foldr f M.empty
  where
    f row m = let (container, contents) = parseLine row
              in foldr (g container) m contents
    g container content = M.insertWith (++) content [container]

canBeContained :: Color -> RuleMap -> [Color]
canBeContained c m = help
  where
    help :: [Color]
    help = foldr f [] (c:help)
    f :: Color -> [Color] -> [Color]
    f c cs = case M.lookup c m of
      Just cs' -> cs' ++ cs
      Nothing  -> []

testMap :: RuleMap
testMap = M.fromList
  [ (Color "bright aqua",[Color "bright blue", Color "mirrored aqua"])
  , (Color "bright blue",[Color "shiny green",Color "dark yellow",Color "muted silver",Color "bright bronze",Color "pale aqua",Color "dotted black",Color "drab beige"])
  , (Color "bright bronze",[Color "bright blue",Color "posh lavender",Color "vibrant brown"])
  , (Color "shiny green", [Color "end"])
  ]

c2 = Color "bright aqua"
test = canBeContained c2 testMap

c1 :: Color
c1 = Color "shiny gold"

c3 = Color "clear brown"

main :: IO ()
main = do
  input <- readFile "./input"
  let lines = T.lines $ T.pack input
  let ctcs = contentToContainers lines
  print $ length $ nub $ tail $ canBeContained c1 ctcs

