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
import Data.List
import Text.Read (readMaybe)

data PrePeriod = PrePeriod
    { start :: !String
    , end:: !String
    } deriving (Show)

instance FromNamedRecord PrePeriod where
    parseNamedRecord r = PrePeriod <$> r .: "Start" <*> r .: "End"

data Period = Period
    { pstart :: Day
    , pend :: Day
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

getPeriods :: [PrePeriod] -> Maybe [Period]
getPeriods l = sort <$> mapM fromPreToPeriod l

countWeekends :: Day -> Day -> Integer
countWeekends s e = toInteger $ length (filter isweekend list)
  where list = takeWhile (<= e) (iterate (addDays 1) s)
        isweekend d = dayOfWeek d == Saturday || dayOfWeek d == Sunday

computeWorkingDays :: Period -> Integer
computeWorkingDays p = howManyWorking (pstart p) (pend p)

howManyWorking :: Day -> Day -> Integer
howManyWorking s e = total - weekend
  where  total = diffDays e s + 1
         weekend = countWeekends s e

spentDays :: [Period] -> Integer
spentDays = sum . map computeWorkingDays

daysoffPerMonth :: Float
daysoffPerMonth = 2.5

remainingOffDays :: Day -> Day -> Integer -> Integer
remainingOffDays s e usedDays = min (generatedDays - usedDays) flooredDays
  where currentMonths = diffGregorianDurationClip e s
        generatedDays = floor $ fromInteger (cdMonths currentMonths) * daysoffPerMonth
        flooredDays = floor $ 12 * daysoffPerMonth

computeOffDays :: Day -> Period -> Integer
computeOffDays i0 p = min generatedDays flooredDays
  where s = pstart p
        e = pend p
        currentMonths = diffGregorianDurationClip s i0
        generatedDays = floor $ fromInteger (cdMonths currentMonths) * daysoffPerMonth
        flooredDays = floor $ 12 * daysoffPerMonth

data OffDaysInfo = OffDaysInfo { lastUpdate :: Day,
                                 availableDays :: Integer,
                                 usedDays :: Integer } deriving (Show)

updateOffDays :: OffDaysInfo -> Period -> OffDaysInfo
updateOffDays i p = OffDaysInfo { lastUpdate = addDays (negate (cdDays currentMonths)) (pend p),
                                  availableDays = min newAvailableDays flooredDays - newUsedDays,
                                  usedDays = newUsedDays }
  where newUsedDays = computeWorkingDays p
        currentMonths = diffGregorianDurationClip (pend p) (lastUpdate i)
        newAvailableDays = computeOffDays (lastUpdate i) p + availableDays i
        flooredDays = floor $ 12 * daysoffPerMonth

readfile :: String -> IO (Maybe [PrePeriod])
readfile f = do
  csvData <- BL.readFile f
  case helper csvData of
    Left err -> return Nothing
    Right v -> return $ Just $ V.toList v
    where helper :: BL.ByteString -> Either String (V.Vector PrePeriod)
          helper = fmap snd . decodeByName

preProcessPeriods :: [PrePeriod] -> Maybe [Period]
preProcessPeriods = fmap sort.mapM fromPreToPeriod

computeOffDaySeq :: String -> [Period] -> Maybe OffDaysInfo
computeOffDaySeq s l = do
        yearS <- readMaybe year :: Maybe Integer
        monthS <- readMaybe month :: Maybe Int
        dayS <- readMaybe day :: Maybe Int
        date0 <- fromGregorianValid yearS monthS dayS
        return $ foldl updateOffDays OffDaysInfo {lastUpdate = date0, availableDays = 0, usedDays = 0} l
  where (year, month, day) = splitDate s
