{-# LANGUAGE DeriveAnyClass #-}
module IPTypes where
import           Control.Exception (Exception)
import           Data.List
import           Data.Word

newtype IP = IP { unIP :: Word32 } deriving (Eq, Ord)

data IPRange = IPRange IP IP deriving Eq

newtype IPRangeDB = IPRangeDB [IPRange] deriving Eq

type LineNumber = Int

newtype ParseError = ParseError LineNumber deriving (Show, Eq)

data InvalidArgsExceptions = LoadIPRangesError ParseError
                           | InvalidIP String
                           deriving Exception

instance Show InvalidArgsExceptions where
  show (LoadIPRangesError (ParseError idx)) = "Error loading ip range database (line: " ++ show idx ++ ")"
  show (InvalidIP s) = "Invalid IP address to check: " ++ s



instance Show IP where
  show (IP ip) = intercalate "." (map show [b4, b3, b2, b1])
    where
      (ip1, b1) = ip `divMod` 256
      (ip2, b2) = ip1 `divMod` 256
      (b4, b3) = ip2 `divMod` 256


instance Show IPRange where
  show (IPRange ip1 ip2) = show ip1 ++ "," ++ show ip2

instance Show IPRangeDB where
  show (IPRangeDB iprs) = unlines $ map show iprs
