{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
module Lib () where
import           Data.Aeson   (ToJSON, encode)
import           Data.Monoid
import           GHC.Generics
newtype Age where
  Age :: {age :: Int} -> Age
  deriving stock (Show, Generic)
  deriving newtype Num
  deriving anyclass ToJSON



a :: Age
a = 42

b :: Age
b = 4
main :: IO ()
main = do
  print a
  print $ a + b
  print $ encode a


newtype MAge = MAge (Maybe Int)
  deriving stock Show
  -- deriving (Semigroup, Monoid) via (Alt Maybe Int)
  deriving (Semigroup, Monoid) via (Dual (Alt Maybe Int))
