{-# LANGUAGE OverloadedStrings #-}
module ParseIP where
import           Control.Applicative
import           Control.Monad
import           Data.List.Split
import           IPTypes
import           Text.Read


guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded f x = if f x then pure x else empty

parseIP :: String -> Maybe IP
parseIP = guarded (4 `isLengthOf`) . splitOn "."
          >=> mapM (readMaybe >=> guarded fitsOctet)
          >=> pure . buildIP
  where
    fitsOctet x = 0 <= x && x < 256

parseIPRange :: String -> Maybe IPRange
parseIPRange = guarded (2 `isLengthOf`) . splitOn ","
                >=> mapM parseIP
                >=> listToIPRange
  where
     listToIPRange [a,b] = pure $ IPRange a b
     listToIPRange _     = empty


parseIPRanges :: String -> Either ParseError IPRangeDB
parseIPRanges = fmap IPRangeDB . mapM parseLine . zip [1..] . lines
  where
    parseLine (ln, s) = case parseIPRange s of
                          Nothing  -> Left (ParseError ln)
                          Just ipr -> Right ipr

isLengthOf :: Int -> [a] -> Bool
isLengthOf n xs = length xs == n


buildIP :: [Int] -> IP
buildIP = IP . fst . foldr f (0,0)
  where
    f x (s, i) = (s + fromIntegral x * 256^i, i+1)
