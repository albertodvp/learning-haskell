{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map  as M
import qualified Data.Text as T

newtype Color = Color T.Text deriving (Eq, Show, Ord)

type RuleMap = M.Map Color [Color]

parseLine :: T.Text -> (Color, [Color])
parseLine s = (container, contents)
  where
    [a, b] = T.splitOn " contain " s
    container = Color a
    contents = map (Color . T.drop 2) (T.splitOn ", " (T.init b))

containerToContents :: [T.Text] -> RuleMap
containerToContents = M.fromList . map parseLine

contentsToContainer :: RuleMap -> RuleMap
contentsToContainer = M.foldrWithKey f M.empty
  where
    f container contents m' = foldr (g container) m' contents
    g container content m' = M.insertWith (++) container [content] m'

main :: IO ()
main = readFile "./input" >>= print . head . M.toList . contentsToContainer . containerToContents . T.lines . T.pack
