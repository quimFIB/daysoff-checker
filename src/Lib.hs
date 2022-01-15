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
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans.State.Lazy (State)
import Control.Monad.Identity (Identity)

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

data MyEnv = MyEnv {daysCoeff :: Float, gHolidays :: [Period]} deriving Show

type EnvReader a = Reader MyEnv a

data MyError = DateStringInvalid String | OverlappingPeriods [Period]
             | NegativeAvailableDays [OffDaysInfo] deriving Show

type Merror a = Either MyError a

-- type EnvError a = ExceptT (Either MyError a) (Reader MyEnv) a
type EnvError a = ExceptT (Either MyError a) (StateT MyEnv Identity) a

type MyState a = StateT MyEnv Identity a

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

-- generalPeriod :: [Period] -> Maybe Period
-- generalPeriod [] = Nothing
generalPeriod :: [Period] -> Period
generalPeriod ps = Period {pstart = pstart (head sortedps), pend = pend (last sortedps)}
  where sortedps = sort ps
generateWeekends :: Period -> [Period]
generateWeekends p = zipWith Period saturdays sundays
  where list = takeWhile (<= checkedEnd) (iterate (addDays 1) checkedStart)
        isweekend d = dayOfWeek d == Saturday || dayOfWeek d == Sunday
        weekends = zip [1..] $ filter isweekend list
        checkedStart = if dayOfWeek (pstart p) == Sunday then addDays (-1) (pstart p)
                       else pstart p
        checkedEnd = if dayOfWeek (pend p) == Saturday then addDays 1 (pend p)
                     else pend p
        saturdays = map snd $ filter (odd.fst) weekends
        sundays = map snd $ filter (even.fst) weekends
periodLength :: Period -> Integer
periodLength p = diffDays (pend p) (pstart p) + 1

countWeekends :: Day -> Day -> Integer
countWeekends s e = toInteger $ length (filter isweekend list)
  where list = takeWhile (<= e) (iterate (addDays 1) s)
        isweekend d = dayOfWeek d == Saturday || dayOfWeek d == Sunday

computeWorkingDays :: [Period] -> Period -> Integer
computeWorkingDays ps p = howManyAccountable ps (pstart p) (pend p)

howManyAccountable :: [Period] -> Day -> Day -> Integer
howManyAccountable ps s e = total - general
  where  total = diffDays e s + 1
         general = sum $ map periodLength ps

spentDays :: [Period] -> [Period] -> Integer
spentDays ps = sum . map (computeWorkingDays ps)

computeOffDays :: Day -> Period -> State MyEnv Integer
computeOffDays i0 p = do
  s <- get
  return $ min (generatedDays (daysCoeff s)) (flooredDays (daysCoeff s))
  where s = pstart p
        e = pend p
        currentMonths = diffGregorianDurationClip s i0
        generatedDays c = floor $ fromInteger (cdMonths currentMonths) * c
        flooredDays c = floor $ 12 * c

data OffDaysInfo = OffDaysInfo { lastUpdate :: Day,
                                 availableDays :: Integer,
                                 usedDays :: Integer } deriving (Show)

updateOffDays :: OffDaysInfo -> Period -> MyState OffDaysInfo
updateOffDays i p = do
  s <- get
  (before, after) <- return $ splitPeriodList p (gHolidays s)
  put MyEnv {daysCoeff = daysCoeff s, gHolidays = after}
  cOffDas <- computeOffDays (lastUpdate i) p
  let newUsedDays = computeWorkingDays before p
  return $ OffDaysInfo { lastUpdate = addDays (negate (cdDays currentMonths)) (pend p),
                         availableDays = min (newAvailableDays cOffDas) (flooredDays (daysCoeff s)) - newUsedDays,
                         usedDays = usedDays i + newUsedDays }
  where currentMonths = diffGregorianDurationClip (pend p) (lastUpdate i)
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


computeOffDaySeq :: Day -> [Period] -> MyState OffDaysInfo
computeOffDaySeq d = foldlM updateOffDays startInfo
  where startInfo = OffDaysInfo {lastUpdate = d, availableDays = 0, usedDays = 0}

computeOffDaySeqTrace :: Day -> [Period] -> MyState [OffDaysInfo]
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
