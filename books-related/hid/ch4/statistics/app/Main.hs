module Main (main) where

import           Options.Applicative
import           Params              (parseParams)
main :: IO ()
main = do
  params <- execParser (info parseParams mempty)
  print params
