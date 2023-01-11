{-# LANGUAGE NoImplicitPrelude #-}

module Day06 (day06) where

import           Protolude

import qualified Data.List  as L
import           Data.Maybe (fromJust)
import qualified Data.Set   as S
import           Data.Text  (unpack)

firstNDiff :: Int -> [Char] -> Maybe Int
firstNDiff dim = go 0
  where
    go _ [] = Nothing
    go index xs
        | S.size (S.fromList $ take dim xs) == dim = Just $ index + dim
        | otherwise = go (index + 1) $ L.tail xs

p1 :: [Char] -> Int
p1 = fromJust . firstNDiff 4
p2 :: [Char] -> Int
p2 = fromJust . firstNDiff 14

day06 :: IO ()
day06 = readFile "inputs/day06.txt" >>= print . liftA2 (,) p1 p2 . unpack
