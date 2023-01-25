-- |

module Params (parseParams, Params(..)) where


import           Options.Applicative

data Mode = Simple | Foldl | Streaming deriving (Show, Read)
data Params = Params
  { filepath :: FilePath
  , mode     :: Mode } deriving Show


parseParams :: Parser Params
parseParams = Params
              <$> strOption (long "file" <> help "File containing the csv content")
              <*> (read <$> strOption (long "mode" <> help "The choosen approach to compute statistics"))
