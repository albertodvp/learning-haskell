module VigenereChipher where

import           Data.Char

type Keyword = String
type Alphabet = String

a :: Alphabet
a = ['A'..'Z']

-- Tests
test1 = if encrypt a "LEMON" "ATTACKATDAWN" == "LXFOPVEFRNHR" then print "Pass" else error "Fail"
test2 = if encrypt a "ALLY" "MEET AT DAWN" == "MPPR AE OYWY" then print "Pass" else error "Fail"


-- Utilities
repetedKeywordOrd :: Keyword -> [Int]
repetedKeywordOrd = map ord . concat . repeat

lowerBoundOrd :: Alphabet -> Int
lowerBoundOrd = ord . minimum

upperBoundOrd :: Alphabet -> Int
upperBoundOrd = ord . maximum


subLowerBoundOrd :: Alphabet -> Int -> Int
subLowerBoundOrd = flip (-) . lowerBoundOrd

keysFromKeyword :: Alphabet -> Keyword -> [Int]
keysFromKeyword a = map (subLowerBoundOrd a) . repetedKeywordOrd


-- Utilities for managing separators
separators = "-./ "

removeSeps :: String -> String
removeSeps = filter (flip notElem separators)

insertSeps :: String -> String -> String
insertSeps [] [] = []
insertSeps base s = if elem (head base) separators
                    then head base: insertSeps (tail base) s
                    else head s: insertSeps (tail base) (tail s)

-- Char level
encdecChar :: (Int -> Int -> Int)
           -> Alphabet -> Int -> Char -> Char
encdecChar f a key c = shiftedChar
  where
    shiftedChar = chr (zeroShiftedCharOrd + lower)
    zeroShiftedCharOrd = mod (f zeroCenteredCharOrd key) alphabet_dim
    zeroCenteredCharOrd = ord c - lower
    alphabet_dim = upper - lower + 1
    lower = lowerBoundOrd a
    upper = upperBoundOrd a

encryptChar :: Alphabet -> Int -> Char -> Char
encryptChar = encdecChar (+)

decryptChar :: Alphabet -> Int -> Char -> Char
decryptChar = encdecChar (-)


-- Sentence level
encdec :: (Alphabet -> Int -> Char -> Char)
       -> Alphabet -> Keyword -> String -> String
encdec f a kw s = insertSeps s $ (zipWith (f a) keys) (removeSeps s)
  where keys = keysFromKeyword a kw


encrypt :: Alphabet -> Keyword -> String -> String
encrypt = encdec encryptChar

decrypt :: Alphabet -> Keyword -> String -> String
decrypt = encdec decryptChar
