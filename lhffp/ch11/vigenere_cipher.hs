module VigenereChipher where

import Data.Char

type Keyword = String

type Alphabet = String
a :: Alphabet
a = ['A'..'Z']


test =
  if encrypt "ATTACKATDAWN" "LEMON" == "LXFOPVEFRNHR" then putStr "Pass" else error "Fail"
  if encrypt "MEET AT DAWN" "ALLY" == "MPPR AE OYWY" then putStr "Pass" else error "Fail"


repetedKeywordOrd :: Keyword -> [Int]
repetedKeywordOrd = map ord . concat . repeat

lowerBoundOrd :: Alphabet -> Int
lowerBoundOrd = ord . minimum

subLowerBoundOrd :: Alphabet -> Int -> Int
subLowerBoundOrd = flip (-) . lowerBoundOrd

keysFromKeyword :: Alphabet -> Keyword -> [Int]
keysFromKeyword a = map (subLowerBoundOrd a) . repetedKeywordOrd

encryptChar :: Int -> Char -> Int

encrypt :: Alphabet -> Keyword -> String -> String
encrypt a kw = zipWith encryptChar (keysFromKeyword a)
-- TODO try with fold


decrypt :: Alphabet -> Keyword -> String -> String
decrypt = undefined
