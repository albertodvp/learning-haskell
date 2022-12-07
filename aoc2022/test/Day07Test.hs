module Day07Test(day07_tests) where

import Test.HUnit

import Day07


day07_tests = [ "a" ~: "1" ~: True ~=? True
              ,  "a" ~: "2" ~: True ~=? True
              ,  "b" ~: "1" ~: True ~=? True
              ,  "b" ~: "2" ~: True ~=? True                                          
  ]

