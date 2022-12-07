{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day07 (day07) where

import Protolude

data FSNode = FSNode
    { isFile :: Bool
    , name :: Text
    , size :: Int
    , childs :: [FSNode]
    }
    deriving (Show)

emptyRoot :: FSNode
emptyRoot = FSNode False "" 0 []

day07 :: IO ()
day07 = readFile "inputs/day07.txt" >>= print
