{-# LANGUAGE DeriveAnyClass #-}
module IPTypes where
import           Control.Exception (Exception)
import           Data.Word

newtype IP = IP { unIP :: Word32 } deriving (Eq, Ord, Show)

data IPRange = IPRange IP IP deriving (Eq, Show)

newtype IPRangeDB = IPRangeDB [IPRange] deriving (Eq, Show)

type LineNumber = Int

newtype ParseError = ParseError LineNumber deriving (Show, Eq)

data InvalidArgsExceptions = LoadIPRangesError ParseError
                           | InvalidIP String
                           deriving Exception

instance Show InvalidArgsExceptions where
  show (LoadIPRangesError (ParseError idx)) = "Error loading ip range database (line: " ++ show idx ++ ")"
  show (InvalidIP s) = "Invalid IP address to check: " ++ s


