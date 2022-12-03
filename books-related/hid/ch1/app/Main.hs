module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment ( getArgs )
import Control.Monad (when)
import Lib (extractVocab, allWordsReport, wordsCountReport, frequentWordsReport)

processTextFile :: FilePath -> Bool -> Int ->  IO ()
processTextFile fname withAllWords n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n  


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", fname, num] -> processTextFile fname True (read num)
    [fname, num] -> processTextFile fname False (read num)
    _ -> putStrLn "Usage [-a] filename frequ_words_num"
