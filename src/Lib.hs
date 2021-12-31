{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    -- ( someFunc
    -- )
where

import Control.Applicative
import Control.Monad.Reader
import Text.RawString.QQ
import Text.Regex.TDFA

import Data.Time
import Data.List.Split
import Data.List
import Data.Foldable
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

data MyEnv = MyEnv {daysCoeff :: Float} deriving Show

type EnvReader a = Reader MyEnv a

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

computeOffDays :: Day -> Period -> EnvReader Integer
computeOffDays i0 p = do
  env <- ask
  return $ min (generatedDays (daysCoeff env)) (flooredDays (daysCoeff env))
  where s = pstart p
        e = pend p
        currentMonths = diffGregorianDurationClip s i0
        generatedDays c = floor $ fromInteger (cdMonths currentMonths) * c
        flooredDays c = floor $ 12 * c

data OffDaysInfo = OffDaysInfo { lastUpdate :: Day,
                                 availableDays :: Integer,
                                 usedDays :: Integer } deriving (Show)

updateOffDays :: OffDaysInfo -> Period -> EnvReader OffDaysInfo
updateOffDays i p = do
  env <- ask
  cOffDas <- computeOffDays (lastUpdate i) p
  return $ OffDaysInfo { lastUpdate = addDays (negate (cdDays currentMonths)) (pend p),
                         availableDays = min (newAvailableDays cOffDas) (flooredDays (daysCoeff env)) - newUsedDays,
                         usedDays = newUsedDays }
  where newUsedDays = computeWorkingDays p
        currentMonths = diffGregorianDurationClip (pend p) (lastUpdate i)
        newAvailableDays offdays = offdays + availableDays i
        flooredDays c = floor $ 12 * c

preProcessPeriods :: [PrePeriod] -> Merror [Period]
preProcessPeriods = fmap sort.mapM fromPreToPeriod

-- checkPeriodsPrecond :: [Period] -> Bool
-- checkPeriodsPrecond =

computeOffDaySeqFromString :: String -> [Period] -> EnvReader (Merror OffDaysInfo)
computeOffDaySeqFromString s l = case dayFromString s of
                                   Left err -> return $ Left err
                                   Right d -> mapReader Right (computeOffDaySeq d l)


computeOffDaySeq :: Day -> [Period] -> EnvReader OffDaysInfo
computeOffDaySeq d = foldlM updateOffDays startInfo
  where startInfo = OffDaysInfo {lastUpdate = d, availableDays = 0, usedDays = 0}

computeOffDaySeqTrace :: Day -> [Period] -> EnvReader [OffDaysInfo]
-- computeOffDaySeqTrace d l = map (computeOffDaySeq d) (inits l)
-- computeOffDaySeqTrace d = scanl updateOffDays startInfo
computeOffDaySeqTrace d l = mapM (foldlM updateOffDays startInfo) (inits l)
  where startInfo = OffDaysInfo {lastUpdate = d, availableDays = 0, usedDays = 0}

isCorrectOffDays :: [OffDaysInfo] -> Bool
isCorrectOffDays = all ((0 <=) . availableDays)
