module Main (main) where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)

days :: [IO ()]
days =
    [ day01
    , day02
    , day03
    , day04
    , day05
    , day06
    ]

main :: IO [()]
main = sequence days
