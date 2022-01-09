{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

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
import Control.Monad.Trans.Except
import Data.Maybe

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

data MyEnv = MyEnv {daysCoeff :: Float, generalHolidays :: [Period]} deriving Show

type EnvReader a = Reader MyEnv a

data MyError = DateStringInvalid String | OverlappingPeriods [Period]
             | NegativeAvailableDays [OffDaysInfo] deriving Show

type Merror a = Either MyError a

type EnvError a = ExceptT (Either MyError a) (Reader MyEnv) a

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

(⊂) :: Period -> Period -> Bool
(⊂) p0 p1 = start1 <= start0 && end0 <= end1
  where start0 = pstart p0
        end0 = pend p0
        start1 = pstart p1
        end1 = pend p1
complement :: Period -> Period -> Period
complement p p'
  | p <= p' = case intersection of
                Nothing -> p
                Just pInter -> Period {pstart = pstart p, pend = pend pInter}
  | otherwise = case intersection of
                  Nothing -> p
                  Just pInter -> Period {pstart = pend pInter, pend = pend p}
  where
      intersection = intersectP p p'
      complementAux p0 p1 = case intersection of
                              Nothing -> p0
                              Just pInter -> Period {pstart = pstart p0, pend = pend pInter}
intersectP :: Period -> Period -> Maybe Period
intersectP p p'
  | p1 ⊂ p0 = Just p1
  | pend p0 < pstart p1 = Nothing
  | otherwise = Just Period {pstart = pstart p1, pend = pend p0}
  where
      (p0, p1) = if p <= p' then (p, p') else (p', p)

periodLength :: Period -> Integer
periodLength p = pend p - pstart p + 1

countWeekends :: Day -> Day -> Integer
countWeekends s e = toInteger $ length (filter isweekend list)
  where list = takeWhile (<= e) (iterate (addDays 1) s)
        isweekend d = dayOfWeek d == Saturday || dayOfWeek d == Sunday

computeWorkingDays :: Period -> Integer
computeWorkingDays p = howManyAccountable (pstart p) (pend p)

howManyAccountable :: Day -> Day -> Integer
howManyAccountable s e = total - general
  where  total = diffDays e s + 1
         general = countGeneralOffDays e

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
                         usedDays = usedDays i + newUsedDays }
  where newUsedDays = computeWorkingDays p
        currentMonths = diffGregorianDurationClip (pend p) (lastUpdate i)
        newAvailableDays offdays = offdays + availableDays i
        flooredDays c = floor $ 12 * c

preProcessPeriods :: [PrePeriod] -> Merror [Period]
preProcessPeriods = fmap sort.mapM fromPreToPeriod

checkPeriodsPrecond :: [Period] -> Bool
checkPeriodsPrecond l = and $ zipWith (<=) ends starts
  where starts = drop 1 $ map pstart l
        ends = map pend l

checkOffDaysCorrect :: [OffDaysInfo] -> Bool
checkOffDaysCorrect = all ((0 <= ) . availableDays)

-- type EnvError a = ExceptT (Either MyError a) (Reader MyEnv) a
computeOffDaySeqFromString :: String -> [Period] -> EnvError OffDaysInfo
computeOffDaySeqFromString s l = case dayFromString s of
                                   Left err -> throwE $ Left err
                                   Right d -> lift $ computeOffDaySeq d l


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

splitPeriodList :: Period
                     -> [Period] -- ^ Assumed ascending order
                     -> ([Period], [Period]) -- ^ fst is the list of intersections of the periods with the first argument period
splitPeriodList p l
  | null reversedPeriods = ([], snd <$> tailPeriods)
  | otherwise = (reverse reversedPeriods, complement (head reversedPeriods) p : (snd <$> tailPeriods))
  where (headPeriods, tailPeriods) =  span testingFunc $ map (\x -> (intersectP p x, x)) l
        reversedPeriods = reverse $ catMaybes $ fst <$> headPeriods
        testingFunc p' = (isJust . fst) p' || (pend . snd) p' <= pend p

computeDaysInPeriod :: [Period] -> Integer
computeDaysInPeriod = sum.map periodLength
