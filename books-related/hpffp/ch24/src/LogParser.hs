{-# LANGUAGE QuasiQuotes #-}

-- Assumptions:
-- 1) there are not negative integer on dates/time

module LogParser where
import           Control.Applicative
import           Control.Monad
import           Data.Bool           (Bool)
import qualified Data.Map            as M
import           Data.Map.Strict     (insertWith)


import           Data.Sort
import           GHC.Float           (integerLogBase)
import           Text.RawString.QQ
import           Text.Trifecta

data Date = Date {
  year  :: Integer,
  month :: Integer,
  day   :: Integer
  }
-- # TODO see time problem
instance Show Date where
  show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

data Time = Time {
  hour    :: Integer,
  minutes :: Integer
}

-- # TODO 8:0 /= 8:00
instance Show Time where
  show (Time h m) = show h ++ ":" ++ show m

type Activity = String

data DayEntry = DayEntry {
  time     :: Time,
  activity :: Activity
  }

data DayEntryMin = DayEntryMin {
  minuteOfDay :: Integer,
  activity'   :: Activity
} deriving Show

entriesToEntriesMin :: [DayEntry] -> [DayEntryMin]
entriesToEntriesMin = map f
  where
    f (DayEntry t a) = DayEntryMin (time2Minutes t) a

instance Show DayEntry where
  show (DayEntry time activity) = show time ++  " " ++ activity

data Day = Day {
  date    :: Date,
  entries :: [DayEntry]
  } deriving Show

type ActivitiesPerDay = M.Map Activity Integer

type Sums = M.Map Activity Integer
type Averages = M.Map Activity Double

time2Minutes :: Time -> Integer
time2Minutes (Time h m) = h * 60 + m

sortedEntriesMin :: [DayEntryMin] -> [DayEntryMin]
sortedEntriesMin = sortOn minuteOfDay


fixStart :: [DayEntryMin] -> [DayEntryMin]
fixStart [] = []
fixStart l@((DayEntryMin m a):_)
  | m == 0 = l
  | otherwise = DayEntryMin 0 "_":l

entriesMin2Averages :: [DayEntryMin] -> ActivitiesPerDay
entriesMin2Averages xs = foldr f M.empty (zip xs (tail xs))
  where
    f :: (DayEntryMin, DayEntryMin) -> ActivitiesPerDay -> ActivitiesPerDay
    f (DayEntryMin m a, DayEntryMin m' a')= insertWith (+) a' (m' - m)

computeActivitiesPerDay :: [DayEntry] -> ActivitiesPerDay
computeActivitiesPerDay =  entriesMin2Averages . fixStart . sortedEntriesMin . entriesToEntriesMin


computeAverages :: [ActivitiesPerDay] -> Averages
computeAverages = sum2Avg . foldr f (M.empty, 0)
  where
    sum2Avg :: (Sums, Integer) -> Averages
    sum2Avg (sums, len) = (/fromInteger len) . fromInteger <$> sums
    f :: ActivitiesPerDay -> (Sums, Integer) -> (Sums, Integer)
    f apd (sums, count) = (M.unionWith (+) apd sums, count+1)

computeAveragesOnDays :: [Day] -> Averages
computeAveragesOnDays = computeAverages . fmap computeActivitiesPerDay . fmap entries

-- Parsing and validation

parseDate :: Parser Date
parseDate = do
  y <- integer
  guard $ y >= 0
  _ <- char '-'
  m <- integer
  guard $ m < 12
  _ <- char '-'
  d <- integer
  guard $ validateDay y m d
  return $ Date y m d

isLeapYear :: Integer -> Bool
isLeapYear year
  | year `mod` 400 == 0 = True
  | year `mod` 4 == 0 && year `mod` 100 /= 0 = True
  | otherwise = False


validateDay :: Integer -> Integer -> Integer -> Bool
validateDay year month day
  | month == 2 = if isLeapYear year
                 then day < 30
                 else day < 29
  | month `elem` [4, 6, 9, 11] = day < 31
  | otherwise = day < 32

parseTime :: Parser Time
parseTime = do
  h <- integer
  guard $ h < 24
  _ <- char ':'
  m <- integer
  guard $ m < 60
  return $ Time h m

parseDateLine :: Parser Date
parseDateLine = do
  spaces
  _ <- char '#'
  spaces
  d <- parseDate
  skipOptional comment
  return d

exceptNewline :: Parser Char
exceptNewline = notChar '\n'

comment :: Parser ()
comment = do
  _ <- skipMany $ char ' '
  _ <- string "--"
  _ <- skipSome exceptNewline
  _ <- newline
  return ()

parseDayEntry :: Parser DayEntry
parseDayEntry = do
  t <- parseTime
  a <- try (manyTill exceptNewline comment) <|> some exceptNewline
  _ <- skipOptional newline
  return $ DayEntry t a

newlineOrComment :: Parser ()
newlineOrComment = try comment <|> (newline >> mempty)

parseDay :: Parser Day
parseDay = do
  skipMany newlineOrComment
  d <- parseDateLine
  skipMany newlineOrComment
  entries <- many parseDayEntry
  return $ Day d entries

parseFile :: Parser [Day]
parseFile = many parseDay


file = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

main :: IO ()
main = case computeAveragesOnDays <$> parseString parseFile mempty file of
  Success x -> print x
  _         -> error "something went wrong"
