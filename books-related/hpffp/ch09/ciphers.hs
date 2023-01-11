module Ciphers where

import           Data.Char

wrapCharIndex :: Int -> Int
wrapCharIndex x = mod (x - ord 'A') (ord 'z') + ord 'A'

cesarCipher :: Int -> [Char] -> [Char]
cesarCipher _ [] = []
cesarCipher key (x:xs) = (chr . wrapCharIndex . (+key) . ord) x: cesarCipher key xs

cesarDecipher :: Int -> [Char] -> [Char]
cesarDecipher _ [] = []
cesarDecipher key (x:xs) = (chr . wrapCharIndex . (flip (-) key) . ord) x : cesarDecipher key xs


cd = cesarDecipher
cc = cesarCipher

idc :: Int -> [Char] -> [Char]
idc key = cd key . cc key
