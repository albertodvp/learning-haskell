module Main (main) where

import qualified Data.ByteString.Lazy as BL
import           Options.Applicative
import           Params               (Params (..), parseParams)
import           Parsing              (decodePersonVector)

main :: IO ()
main = do
  (Params filePath mode) <- execParser (info parseParams mempty)
  csvData <- BL.readFile filePath
  print $ decodePersonVector csvData
