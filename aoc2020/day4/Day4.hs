import           Control.Monad (guard)
import           Data.Char     (isAlphaNum, isNumber)
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes, mapMaybe)
import           Data.Monoid
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
type Validator = String -> Bool

validateField :: String -> [Validator] -> Bool
validateField s = getAll . foldMap (All . ($s))

isHeightOk :: String -> Bool
isHeightOk s = case mis of
  "cm" -> 150 <= num && num <= 193
  "in" -> 59 <= num && num <= 76
  _    -> False
  where
    num = read $ takeWhile isNumber s
    mis = dropWhile isNumber s

-- The order follows `tail fields`
validators :: [[Validator]]
validators =
  [ [(== 4) . length, (>= 1920) . read, (<= 2002) . read]
  , [(== 4) . length, (>= 2010) . read, (<= 2020) . read]
  , [(== 4) . length, (>= 2020) . read, (<= 2030) . read]
  , [isHeightOk]
  , [(== 7) . length, (== '#') . head, (== []) . dropWhile (flip elem $ ['0'..'9'] ++ ['a'..'f']) . tail]
  , [flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]]
  , [(== 9) . length]
  ]

parsePassport :: M.Map String String -> Maybe Passport
parsePassport pm = do
  -- part1
  let extracted = flip M.lookup pm `mapMaybe` tail fields
  guard $ length extracted == length fields - 1

  let extractedFields@[birth, issue, expiration, height, hair, eye, passport] = tail <$> extracted

  -- part2
  guard $ getAll $ foldMap All $ uncurry validateField <$> zip extractedFields validators

  let cid = M.lookup (head fields) pm
  pure $ Passport birth issue expiration height hair eye passport cid

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
