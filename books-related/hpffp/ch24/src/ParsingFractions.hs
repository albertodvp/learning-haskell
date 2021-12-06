{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.Trifecta

badFranction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be 0"
    _ -> return (numerator % denominator)


main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty

  print $ parseFraction' badFranction
  print $ parseFraction' alsoBad
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork

-- Unit of success
s1 = parseString integer mempty "123abc"

f1 = parseString (integer >> eof) mempty "123abc"

s2 = parseString (integer >> eof) mempty "123"


s3 = parseString (integer <* eof) mempty "123"
