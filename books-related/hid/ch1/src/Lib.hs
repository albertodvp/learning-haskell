{-# LANGUAGE OverloadedStrings #-}
module Lib (extractVocab, allWordsReport, wordsCountReport, frequentWordsReport) where
import           Data.Text (Text)

import qualified Data.Text as T
import           Fmt

import           Data.Char (isLetter)
import           Data.List (group, sort, sortBy)
import           Data.Ord

type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws''
  where
    ws = map cleanWord (T.words t)
    ws' = filter (not . T.null) ws
    ws'' = map T.toCaseFold ws'
    cleanWord = T.dropAround (not . isLetter)
    buildEntry xs@(x:_) = (x, length xs)


allWords :: Vocabulary -> [Text]
allWords = map fst

wordsCount :: Vocabulary -> (Int, Int)
wordsCount v = (total, unique)
  where
    total = sum $ map snd v
    unique = length $ map fst v

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

-- Reports
allWordsReport :: Vocabulary -> Text
allWordsReport v = fmt $ nameF "All words" $ unlinesF (allWords v)

wordsCountReport :: Vocabulary -> Text
wordsCountReport v = fmt $
  "Total number of words" +|total|+
  "\nNumber of unique words: " +|unique|+ "\n"
  where
    (total, unique) = wordsCount v

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport v n =
  fmt $ nameF "Frequent words"
      $ blockListF' "" fmtEntry reportData
  where
    reportData = take n $ wordsByFrequency v
    fmtEntry (t, n) = ""+|t|+": "+|n|+ ""

