module Main where

import Data.Foldable (toList)
import Data.Csv (decodeByName)
import qualified Data.ByteString.Lazy as BL
import QuoteData

main :: IO ()
main = putStrLn "Stock quotes processing project"

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fp = do
  csvData <- BL.readFile fp
  case decodeByName csvData of
    Left _ -> pure []
    Right (_, qd) -> pure $ toList qd
