{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module GenericSql () where

import           Data.Text    (Text)
import qualified Data.Text.IO as TIO
import           GHC.Generics
import           TextShow

data Status = Ok | Err deriving (Generic, Show)

fo = from Ok

fe = from Err


evalErr :: D1 ('MetaData "Status" "GenericSql" "main" 'False) (C1 ('MetaCons "Ok" 'PrefixI 'False) U1 :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1) x
evalErr = M1 (R1 (M1 U1))


err :: Status
err = to evalErr


data Request = Request String Int deriving (Show, Generic)


data Student = Student {studendId :: Int, name :: Text, year :: Int}
  deriving stock Generic
  deriving anyclass ToSQL

data Course = Course {courseId :: Int, title :: Text, instructor :: Text}
  deriving stock Generic
  deriving anyclass ToSQL


class ToColumnsValuesLists f where
  toColumnsValues :: f a -> ([Builder], [Builder])

instance ToColumnsValuesLists U1 where
  toColumnsValues _ = ([], [])

instance (ToColumnsValuesLists a, ToColumnsValuesLists b) => ToColumnsValuesLists (a :*: b)  where
  toColumnsValues (a :*: b) = (columns1 <> columns2, values1 <> values2)
    where
      (columns1, values1) = toColumnsValues a
      (columns2, values2) = toColumnsValues b

instance ToColumnsValuesLists a => ToColumnsValuesLists (M1 i c a) where
  toColumnsValues (M1 a) = toColumnsValues a

instance {-# OVERLAPPING #-} (ToColumnsValuesLists a, Selector c) => ToColumnsValuesLists (M1 S c a) where
  toColumnsValues s@(M1 a) = (fromString (selName s):columns, values)
    where
      (columns, values) = toColumnsValues a

instance TextShow a => ToColumnsValuesLists (K1 i a) where
  toColumnsValues (K1 a) = ([], [showb a])

insertIntoDefault :: (Generic a, ToColumnsValuesLists (Rep a)) => Text -> a -> Text
insertIntoDefault table val =
  toText $ "INSERT INTO " <> fromText table <> " " <> buildersToList columns <> " VALUES " <> buildersToList values
  where
    (columns, values) = toColumnsValues (from val)



buildersToList :: [Builder] -> Builder
buildersToList [] = "()"
buildersToList (x:xs) = singleton '(' <> x <> go xs
  where
    go (y:ys) = showbCommaSpace <> y <> go ys
    go []     = singleton ')'

class ToSQL a where
  insertInto :: Text -> a -> Text
  default insertInto :: (Generic a, ToColumnsValuesLists (Rep a)) => Text -> a -> Text
  insertInto = insertIntoDefault



main :: IO ()
main = do
  TIO.putStrLn $ insertInto "students" $ Student 12345 "Mario" 1980
  TIO.putStrLn $ insertInto "courses" $ Course 1 "Math" "Giovanni"



