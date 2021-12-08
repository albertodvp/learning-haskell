{-# LANGUAGE QuasiQuotes #-}

module LogParser where
import           Control.Applicative
import           Data.Map            (Map)
import           Text.RawString.QQ
import           Text.Trifecta
-- sum the time spent in each activity
-- provide an alternative aggregation of the data that provides average time spent per activity per day.

data Date = Date {
  year  :: Integer,
  month :: Integer,
  day   :: Integer
  } deriving Show
type Activity = String
type StartingTime = Integer -- seconds from the begin of the day

type RawEntry = (StartingTime, Activity)
type LogFileRaw = [(Date, [RawEntry])]

type TimeSpent = Integer -- delta seconds
type Entries = Map StartingTime Activity

type EntriesPerDate = Map Activity TimeSpent
newtype LogFile = LogFile (Map Date [EntriesPerDate]) deriving Show

-- PARSER
-- 1) String -> LogFileRaw
-- Assumption 1: before the first entry, the activity is Sleep => every list has an implicit 00:00 Sleep in it's head
-- 2) LogFileRaw -> LogFileRaw

--LogFileRow -> LogFile
-- Assumption 2: a day does not re-occur on log


-- Parsers
-- 08:00 Breakfast
parseRawEntry :: Parser RawEntry
parseRawEntry = do
  hour <- integer
  _ <- char ':'
  minutes <- integer
  spaces -- TODO, the parser works even without these, don't know why, integer results in TokenParsing, that could be it
  activity <- some letter

  let startingTime = hour * 60 + minutes
  many newline
  return (startingTime, activity)

-- Parse date
-- # 2025-02-05
parseDate :: Parser Date
parseDate = liftA3
              Date
              (string "# " *> integer <* char '-')
              (integer <* char '-')
              integer

p :: Parser [Char]
p =  some letter

skipComments :: Parser ()
skipComments = do
  spaces
  string "--"
  many $ noneOf "\n"
  return ()

parseLogFile :: Parser LogFileRaw
parseLogFile = many singleDayParser
  where
    singleDayParser = do
      many newline
      date <- parseDate
      many newline
      entries <- many parseRawEntry
      return (date, entries)
simpleFile = [r|
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
# 2025-02-07
08:00 Breakfast
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
