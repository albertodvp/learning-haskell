{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing(decodePersonVector, Person(..), Gender(..)) where

import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy  as BL
import           Data.Csv
import           Data.Time             (Day, defaultTimeLocale, parseTimeM)
import qualified Data.Vector           as V
import           GHC.Generics          (Generic)
import           Lib                   (simple)
data Gender = M | F deriving (Show, Eq)

data Person = Person
  { name      :: String
  , gender    :: Gender
  , birthDate :: Day
  } deriving (Generic, FromRecord, Show, Eq)

instance FromField Gender where
  parseField "Male"   = pure M
  parseField "Female" = pure F

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

decodePersonVector :: BL.ByteString -> V.Vector Person
decodePersonVector csvData = case decode NoHeader csvData of
  Left err -> error err
  Right v  -> v

instance Ord Person where
  p1 <= p2 = birthDate p1 <= birthDate p2
