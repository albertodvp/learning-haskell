{-# LANGUAGE QuasiQuotes #-}

module LogParser where
import           Control.Applicative
import           Data.Bool           (Bool)
import           Data.Map            (Map)
import           GHC.Float           (integerLogBase)
import           Text.RawString.QQ
import           Text.Trifecta
-- sum the time spent in each activity
-- provide an alternative aggregation of the data that provides average time spent per activity per day.

data Date = Date {
  year  :: Integer,
  month :: Integer,
  day   :: Integer
  }
instance Show Date where
  show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

data Time = Time {
  hour    :: Integer,
  minutes :: Integer
}

instance Show Time where
  show (Time h m) = show h ++ ":" ++ show m

type Activity = String

parseDate :: Parser Date
parseDate = do
  y <- integer
  guard $ y >= 0
  _ <- char '-'
  m <- integer
  guard $ m >= 1 && m < 12
  _ <- char '-'
  d <- integer
  guard $ validateDay m d

isLeapYear :: Integer -> Bool
isLeapYear year
  | year `mod` 400 == 0 = True
  | year `mod` 4 == 0 && year `mod` 100 /= 0 = True
  | otherwise = False

-- TODO Positive integer
validateDay :: Integer -> Integer -> Integer -> Bool
validateDay year month day
  | month == 2 = if isLeapYear year
                 then day >= 1  && day < 31
                 else day >= 1  && day < 31
  | month `elem` [4, 6, 9, 11] = day >= 1  && day < 31




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
