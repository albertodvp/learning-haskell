{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module QuoteData where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (decodeByName, FromNamedRecord, FromField (..))

data QuoteData = QuoteData {
  day :: Day,
  volume :: Int,
  open :: Double,
  close :: Double,
  high :: Double,
  low :: Double
  } deriving (Generic, FromNamedRecord, Show)


instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%M-%d" . unpack
  


data QField = Open | Close | High | Low | Volume
  deriving (Eq, Ord, Show, Enum, Bounded)

field2fun :: QField -> QuoteData -> Double
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = fromIntegral . volume
