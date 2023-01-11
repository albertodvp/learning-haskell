{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where


import           Protolude


import qualified Prelude

import           Stems


population :: FilePath -> IO (Population Char)
population p = readFile p <&> Population . getStems

main :: IO b
main = do
  args <- getArgs
  case args of
    [fp, n'] -> do
      p <- population fp
      let q = queryTrie (Prelude.read n') (mkTrie p)
      forever $ do
        Prelude.putStrLn "> Input:"
        x <- Prelude.getLine
        Prelude.putStrLn "> Search:"
        traverse (Prelude.putStrLn . unStem) $ q $ Stem x
        Prelude.putStrLn "---"
    _ -> Prelude.error "Bad usage"

