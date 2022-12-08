module Main where

import Day07Test (day07Tests)
import Day08Test (day08Tests)
import UtilsTest (utilsTests)

import Test.HUnit

main :: IO Counts
main = runTestTT $ test (utilsTests ++ day07Tests ++ day08Tests)
