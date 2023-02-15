module Lib(simple) where

import qualified Data.Vector as V
import           Parsing     (Gender (..), Person (..), decodePersonVector)

data Result = Result
  { nFemale      :: Int
  , oldestPerson :: Maybe Person
  }


compute :: Result -> Person -> Result
 (Result nFemale mOldest) newPerson = Result newCount $ Just newOld
  where
    newCount = case gender newPerson of
      M -> nFemale
      F -> nFemale + 1
    newOld = case mOldest of
      Nothing        -> newPerson
      Just accPerson -> min accPerson newPerson

simple :: V.Vector Person -> Result
simple = foldl compute (Result 0 Nothing)

