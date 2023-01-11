module UtilsTest (utilsTests) where

import           Utils

import           Test.HUnit

utilsTests =
    [ "replaceAt" ~: "" ~: replaceAt 0 1 [42, 2, 3] ~=? [1, 2, 3]
    , "replaceAt" ~: "" ~: replaceAt (-1) 42 [1, 2, 3] ~=? [42, 1, 2, 3]
    , "replaceAt" ~: "" ~: replaceAt 99 42 [1, 2, 3] ~=? [1, 2, 3, 42]
    ]
