{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Protolude

import qualified Prelude

import Data.Text (unpack)

import System.Environment ( getArgs )

-- TODO why this import for data constructor
import Stems (query, Population(Population), Stem(Stem, unStem))


-- | read input from a file, e.g.
-- word1
-- word2
-- word3
population :: FilePath -> IO (Population Char)
population p = readFile p <&> Population . map (Stem . unpack) . lines

main :: IO b
main = do
  [fp, n'] <- getArgs
  p <- population fp
  let q = query (Prelude.read n') p
  forever $ do
    x <- Prelude.getLine
    traverse (Prelude.putStrLn . unStem) $ q $ Stem x

