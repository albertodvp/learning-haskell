import           Control.Monad (guard)
import           Data.List
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes, mapMaybe)
data Passport = Passport
  { getBirthYear      :: String
  , getIssueYear      :: String
  , getExpirationYear :: String
  , getHeight         :: String
  , getHairColor      :: String
  , getEyeColor       :: String
  , getPassportID     :: String
  , getCountryID      :: Maybe String
  } deriving Show


fields :: [String]
fields =
  [ "cid" -- this is special
  , "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  ]

parsePassport :: M.Map String String -> Maybe Passport
parsePassport pm = do
  let extractedFields = flip M.lookup pm `mapMaybe` tail fields
  guard $ length extractedFields == length fields - 1

  let [birth, issue, expiration, height, hair, eye, passport] = extractedFields
  pure $ Passport birth issue expiration height hair eye passport $ M.lookup (head fields) pm

parseEntries :: [String] -> [Maybe Passport]
parseEntries [] = []
parseEntries xs = parsePassport pm : parseEntries rest
  where
    pm = M.fromList $ splitAt 3 <$> words entryRow
    entryRow = unwords $ takeWhile (/= "") xs
    rest = case dropWhile (/= "") xs of
      [] -> []
      xs -> tail $ dropWhile (/= "") xs

main :: IO ()
main = readFile "./input" >>= print . length . catMaybes . parseEntries . lines
