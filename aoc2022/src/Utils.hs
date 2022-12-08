module Utils(replaceAt) where

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n y xs = take n xs ++ [y] ++ drop (n + 1) xs
