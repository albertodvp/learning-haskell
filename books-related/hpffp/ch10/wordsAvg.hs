module WordsAvg where

seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))


seekritFunc' x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))


