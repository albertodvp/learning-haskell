module C11 where

import           Data.Char
import           Data.List

main :: IO ()
main = do
  text <- readFile "hamlet.txt"
  print $ length (head <$> group <$> filter isLetter <$> fmap toLower <$> words text)

