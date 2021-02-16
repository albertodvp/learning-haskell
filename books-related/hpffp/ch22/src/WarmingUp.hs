-- |

module WarmingUp where

import           Data.Char

cap :: [Char] ->  [Char]
cap = map toUpper

rev :: [Char] ->  [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap


tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap


tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  x <- rev
  y <- cap
  return (x, y)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev >>=  (. cap) <$> (,)

