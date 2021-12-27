{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    -- ( someFunc
    -- )
where

import Control.Applicative
import Text.RawString.QQ
import Text.Regex.TDFA

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Time
import Data.List.Split
import Text.Read (readMaybe)

data PrePeriod = PrePeriod
    { start :: !String
    , end:: !String
    , non_working:: !String
    , total:: !String
    } deriving (Show)

instance FromNamedRecord PrePeriod where
    parseNamedRecord r = PrePeriod <$> r .: "Start" <*> r .: "End" <*> r .: "Non-working" <*> r .: "Total"

data Period = Period
    { pstart :: Day
    , pend:: Day
    } deriving (Show, Eq)
instance Ord Period where
  (Period s1 _) `compare` (Period s2 _) = s1 `compare` s2

getDate :: String
getDate = [r|[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]|]

splitDate :: String -> (String, String, String)
splitDate d = (year, month, day)
  where year = head s
        month = s !! 1
        day = s !! 2
        s = splitOn "-" (d =~ getDate :: String)

fromPreToPeriod :: PrePeriod -> Maybe Period
fromPreToPeriod p = do
  yearS <- readMaybe yearS :: Maybe Integer
  monthS <- readMaybe monthS :: Maybe Int
  dayS <- readMaybe dayS :: Maybe Int
  start <- fromGregorianValid yearS monthS dayS
  yearE <- readMaybe yearE :: Maybe Integer
  monthE <- readMaybe monthE :: Maybe Int
  dayE <- readMaybe dayE :: Maybe Int
  end <- fromGregorianValid yearE monthE dayE
  return Period {pstart = start, pend = end}
  where (yearS, monthS, dayS) = splitDate (start p)
        (yearE, monthE, dayE) = splitDate (end p)

countWeekends :: Day -> Day -> Integer
countWeekends s e = toInteger $ length (filter isweekend list)
  where list = takeWhile (<= e) (iterate (addDays 1) s)
        isweekend d = dayOfWeek d == Saturday || dayOfWeek d == Sunday

howManyWorking :: Day -> Day -> Integer
howManyWorking s e = total - weekend
  where  total = diffDays e s + 1
         weekend = countWeekends s e

daysoffPerMonth :: Float
daysoffPerMonth = 2.5

generatedOffDays :: Day -> Day -> Integer -> Integer
generatedOffDays s e usedDays = if generatedDays - usedDays > flooredDays then flooredDays
                                else generatedDays - usedDays
  where currentMonths = diffGregorianDurationClip e s
        generatedDays = floor $ fromInteger (cdMonths currentMonths) * daysoffPerMonth
        flooredDays = floor $ 12 * daysoffPerMonth

readfile :: IO (Maybe [PrePeriod])
readfile = do
  csvData <- BL.readFile "data/calendar1.csv"
  case helper csvData of
    Left err -> return Nothing
    Right v -> return $ Just $ V.toList v
    where helper :: BL.ByteString -> Either String (V.Vector PrePeriod)
          helper = fmap snd . decodeByName
