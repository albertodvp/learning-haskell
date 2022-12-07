module Main where

import Test.HUnit
import Day07Test(day07_tests)

tests = test $ concat [day07_tests]

main :: IO Counts
main = runTestTT tests
