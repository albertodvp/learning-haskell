-- |

module SemanticVersion where

import           Control.Applicative
import           Control.Monad       (guard)
import           Lexer               (ParserFlags)
import           Text.Trifecta
-- Strings
data NumberOrString = NOIS Integer | NOSS String deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq)

positiveInteger :: Parser Integer
positiveInteger = do
  x <- integer
  guard $ x > 0
  return x


parseNOS :: Parser NumberOrString
parseNOS = try (NOIS <$> positiveInteger) <|> (NOSS <$> some (char '-' <|> letter))

parseNOS' :: Parser NumberOrString
parseNOS' = try (NOIS <$> positiveInteger) <|> (NOSS <$> some (char '-' <|> alphaNum))

parseRelease :: Parser Release
parseRelease = do
  _ <- char '-'
  head <- parseNOS
  tail <- many (char '.' *> parseNOS)
  return $ head:tail


parseMetadata :: Parser Metadata
parseMetadata = do
  _ <- char '+'
  head <- parseNOS'
  tail <- many (char '.' *> parseNOS')
  return $ head:tail


parseSemVer :: Parser SemVer
parseSemVer = do
    major <- integer
    _ <- char '.'
    minor <- integer
    _ <- char '.'
    patch <- integer
    release <- try parseRelease <|> pure []
    metadata <- try parseMetadata <|> pure []
    return $ SemVer major minor patch release metadata


-- This is incomplete, see https://semver.org
instance Ord SemVer where
  compare (SemVer maj min patch pr md) (SemVer maj' min' patch' pr' md') =
    let base = (maj, min, patch)
        base' = (maj', min', patch')
    in compare base base'
