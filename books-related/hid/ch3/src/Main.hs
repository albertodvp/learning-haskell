{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable (toList)
import Data.Csv (decodeByName)
import qualified Data.ByteString.Lazy as BL
import QuoteData
import Data.Text (unpack)
import Params
import Control.Monad (unless, when)
import HtmlReport
import StatReport
import Charts

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes


generateReports :: (Functor t, Foldable t)  => Params -> t QuoteData -> IO ()
generateReports Params {..} quotes = do
  unless silent $ putStr textRpt
  when chart $ plotChart title quotes chartFname
  saveHtml htmlFile htmlRpt
  where
    statInfo' = statInfo quotes
    textRpt = textReport statInfo'
    htmlRpt = htmlReport title quotes statInfo' [chartFname | chart]
    withCompany prefix = maybe mempty (prefix <>) company
    chartFname = unpack $ "chart" <> withCompany "_" <> ".svg"
    title = unpack $ "Historical Quotes" <> withCompany " for "
    saveHtml Nothing _ = pure ()
    saveHtml (Just f) html = BL.writeFile f html
    
