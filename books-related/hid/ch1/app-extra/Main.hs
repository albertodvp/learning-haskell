{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Protolude

import qualified Prelude

import Data.Text (unpack)

import System.Environment ( getArgs )

-- TODO why this import for data constructors
import Stems (query, Population(Population), Stem(Stem, unStem), getStems)


-- | read input from a file e.g "./data/hamlet.txt/"
population :: FilePath -> IO (Population Char)
population p = readFile p <&> Population . getStems

main :: IO b
main = do
  [fp, n'] <- getArgs
  p <- population fp
  let q = query (Prelude.read n') p
  forever $ do
    x <- Prelude.getLine
    traverse (Prelude.putStrLn . unStem) $ q $ Stem x

