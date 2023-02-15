-- |

module Main where

import           Control.Monad.Extra (zipWithM_)
import           IORefs
import           System.Environment
main :: IO ()
main = do
  args <- getArgs
  counts <- traverse fileCount args
  zipWithM_ printRes args counts
  where
    printRes fp c = putStrLn $ fp ++ ":\t" ++ show c

