module Experiment where

import Data.Char
import Control.Monad.Reader

strIsEnoughtToLower :: Reader [Char] Bool
strIsEnoughtToLower = Reader $ \s -> length s > 5

strLenFilterLower :: Bool -> Reader [Char] Int
strLenFilterLower x = Reader $  length . filter (if x then isLower else isUpper)

--runReader strIsEnoughtToLower "abcde"

--runReader strIsEnoughtToLower "abcdef"

--strIsEnoughtToLower >>= strLenFilterLower
--runReader (strIsEnoughtToLower >>= strLenFilterLower) "abcde"

--runReader (strIsEnoughtToLower >>= strLenFilterLower) "abcdef"
