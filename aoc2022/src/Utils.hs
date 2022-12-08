module Utils where

import Data.List

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n y xs = take n xs ++ [y] ++ drop (n + 1) xs
