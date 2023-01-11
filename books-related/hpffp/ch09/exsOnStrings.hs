module ExsOnStrings where

import           Data.Char

onlyUpperChar :: [Char] -> [Char]
onlyUpperChar xs = filter isUpper xs

capitalizeFirst :: [Char] -> [Char]
capitalizeFirst (x:xs) = toUpper x : xs

capitalizeRecursive :: [Char] -> [Char]
capitalizeRecursive []     = []
capitalizeRecursive (x:xs) = toUpper x : capitalizeRecursive xs

returnFirstCapitalized :: [Char] -> Char
returnFirstCapitalized (x:_) = toUpper x

returnFirstCapitalized' :: [Char] -> Char
returnFirstCapitalized' xs = toUpper $ head xs

returnFirstCapitalized'' :: [Char] -> Char
returnFirstCapitalized'' xs = (toUpper . head) xs

returnFirstCapitalized''' :: [Char] -> Char
returnFirstCapitalized''' = toUpper . head
