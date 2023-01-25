{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing(decodePersonVector) where

import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy  as BL
import           Data.Csv
import           Data.Time             (Day, defaultTimeLocale, parseTimeM)
import qualified Data.Vector           as V
import           GHC.Generics          (Generic)
data Gender = M | F deriving Show

data Person = Person
  { name      :: String
  , gender    :: Gender
  , birthDate :: Day
  } deriving (Generic, FromRecord, Show)

instance FromField Gender where
  parseField "Male"   = pure M
  parseField "Female" = pure F

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

decodePersonVector :: BL.ByteString -> V.Vector Person
decodePersonVector csvData = case decode NoHeader csvData of
  Left err -> error err
  Right v  -> v
