{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    -- ( someFunc
    -- )
where

import Control.Applicative
import Text.RawString.QQ
import Text.Regex.TDFA

import Data.Time
import Data.List.Split
import Data.List
import Text.Read (readMaybe)
import Data.Csv (FromNamedRecord, parseNamedRecord, (.:))

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

data MyError = DateStringInvalid String | OverlappingPeriods [Period] deriving Show

type Merror a = Either MyError a

getDate :: String
getDate = [r|[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]|]

splitDate :: String -> (String, String, String)
splitDate d = (year, month, day)
  where year = head s
        month = s !! 1
        day = s !! 2
        s = splitOn "-" (d =~ getDate :: String)

dayFromStringMaybe :: String -> Maybe Day
dayFromStringMaybe s = do
  yearS <- readMaybe year :: Maybe Integer
  monthS <- readMaybe month :: Maybe Int
  dayS <- readMaybe day :: Maybe Int
  fromGregorianValid yearS monthS dayS
  where (year, month, day) = splitDate s

dayFromString :: String -> Merror Day
dayFromString s = case dayFromStringMaybe s of
                    Nothing -> Left (DateStringInvalid s)
                    Just d -> Right d

fromPreToPeriod :: PrePeriod -> Merror Period
fromPreToPeriod p = do
  start <- dayFromString (start p)
  end <- dayFromString (end p)
  return Period {pstart = start, pend = end}

getPeriods :: [PrePeriod] -> Merror [Period]
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

preProcessPeriods :: [PrePeriod] -> Merror [Period]
preProcessPeriods = fmap sort.mapM fromPreToPeriod

-- checkPeriodsPrecond :: [Period] -> Bool
-- checkPeriodsPrecond =

computeOffDaySeqFromString :: String -> [Period] -> Merror OffDaysInfo
computeOffDaySeqFromString s l = do
        date0 <- dayFromString s
        return $ computeOffDaySeq date0 l

computeOffDaySeq :: Day -> [Period] -> OffDaysInfo
computeOffDaySeq d = foldl updateOffDays startInfo
  where startInfo = OffDaysInfo {lastUpdate = d, availableDays = 0, usedDays = 0}

computeOffDaySeqTrace :: Day -> [Period] -> [OffDaysInfo]
-- computeOffDaySeqTrace d l = map (computeOffDaySeq d) (inits l)
computeOffDaySeqTrace d = scanl updateOffDays startInfo
  where startInfo = OffDaysInfo {lastUpdate = d, availableDays = 0, usedDays = 0}

isCorrectOffDays :: [OffDaysInfo] -> Bool
isCorrectOffDays = all ((0 <=) . availableDays)
