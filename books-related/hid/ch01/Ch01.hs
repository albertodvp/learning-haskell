{-# LANGUAGE OverloadedStrings #-}
-- New to me:
-- -> Down data const
-- -> Fmt

module Main where

import           Control.Applicative (liftA2)
import           Control.Monad       (when)
import           Data.Char
import           Data.List           (group, sort, sortBy)
import           Data.Ord
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Fmt
import           System.Environment

main1 :: IO ()
main1 = do
  text <- readFile "data/texts/hamlet.txt"
  print $ take 7 (map head $ group $ sort $ words $ map toLower text)


main2 :: IO ()
main2 = do
  [fname] <- getArgs
  text <- TIO.readFile fname
  let ws = map head $ group $ sort $ map T.toCaseFold $ filter (not . T.null) $ map (T.dropAround $ not. isLetter) $ T.words text
  print $ length ws


type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab = map buildEntry . group . sort . ws
  where
    ws = map T.toCaseFold . filter (not . T.null)  . map cleanWord . T.words
    buildEntry xs@(x:_) = (x, length xs)
    buildEntry _        = undefined
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab

allWordsReport :: Vocabulary -> Text
allWordsReport vocab =
  fmt $ nameF "All words" $ unlinesF (map fst vocab)

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab = fmt $
  "Total number of words: " +| total |+
  "\nNumber of unique words: " +| unique |+ "\n"
  where
    (total, unique) = wordsCount vocab
    wordsCount = liftA2 (,) (sum . map snd) length


frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab num = fmt $
  nameF "Frequent words" $ blockListF' "*" fmtEntry reportData
  where
    reportData = take num $ wordsByFrequency vocab
    fmtEntry (t,n) = ""+|t|+": "+|n|+""


wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

processTextFile :: FilePath -> Bool -> Int -> IO ()
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
    ["-a", fname , num] -> processTextFile fname True (read num)
    [fname , num]       -> processTextFile fname False (read num)
    _                   -> putStrLn "Bad usage"


