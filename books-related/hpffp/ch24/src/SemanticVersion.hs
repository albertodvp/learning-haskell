-- |

module SemanticVersion where

import           Control.Applicative
import           Text.Trifecta

-- Strings
data NumberOrString = NOIS Integer | NOSS String deriving Show

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving Show

parserRelease :: Parser
parserRelease = (letter <|> digit)

parseStringNOS :: Parser NumberOrString
parseStringNOS = NOSS <$> some (letter <|> digit)


parseNumberNOS :: Parser NumberOrString
parseNumberNOS = NOIS <$> (integer <* eof)


parseNOSForRelease :: Parser NumberOrString
parseNOSForRelease =  try parseNumberNOS <|> (parseStringNOS <* eof)


parseMetadata :: Parser Metadata
parseMetadata = undefined


-- TODO remove
parseNumberOrString :: Parser NumberOrString
parseNumberOrString = (NOIS <$> integer) <|> (NOSS <$> some (noneOf ".")) -- TODO

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- integer
    _ <- char '.'
    minor <- integer
    _ <- char '.'
    patch <- integer
    release <- try (some (char '-' *> parseNOSForRelease <* char '.')) <|> pure []
    return $ SemVer major minor patch release []
