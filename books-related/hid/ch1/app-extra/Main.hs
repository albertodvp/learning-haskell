{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Protolude

import qualified Prelude

import Stems


-- | read input from a file e.g "./data/hamlet.txt/"
population :: FilePath -> IO (Population Char)
population p = readFile p <&> Population . getStems

-- TODO why IO b
main :: IO b
main = do
  args <- getArgs
  case args of
    [fp, n'] -> do
      p <- population fp
      let q = query (Prelude.read n') p
      forever $ do
        Prelude.putStrLn "> Input:"                
        x <- Prelude.getLine
        Prelude.putStrLn "> Search:"        
        traverse (Prelude.putStrLn . unStem) $ q $ Stem x
        Prelude.putStrLn "---"
    _ -> Prelude.error "Bad usage"

