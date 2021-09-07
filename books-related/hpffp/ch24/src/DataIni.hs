{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Ini where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Char           (isAlpha)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Test.Hspec

import           Text.RawString.QQ
import           Text.Trifecta


headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String deriving (Show, Ord, Eq)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)


