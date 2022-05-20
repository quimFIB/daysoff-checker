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
import Debug.Trace

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

data MyError = InfoStringInvalid String | DateStringInvalid String | OverlappingPeriods [Period]
             | NegativeAvailableDays OffDaysInfo deriving Show

type Merror a = Either MyError a

-- type EnvError a = ExceptT (Either MyError a) (Reader MyEnv) a
type EnvError a = ExceptT (Either MyError a) (StateT [Period] (Reader Float)) a

type MyState a = StateT ([OffDaysInfo],[Period]) (Reader Float) a

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
getInfo :: String
getInfo = [r|[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9];[0-9]*;[0-9]*|]

splitInfo :: String -> (String, String, String)
splitInfo i = (date, available, used)
  where date = head s
        available = s !! 1
        used = s !! 2
        s = splitOn ";" (i =~ getInfo :: String)

infoFromStringMaybe :: String -> Maybe OffDaysInfo
infoFromStringMaybe s = do
  dateS <- dayFromStringMaybe date
  availableS <- readMaybe available :: Maybe Integer
  usedS <- readMaybe used :: Maybe Integer
  return OffDaysInfo {lastUpdate = dateS, availableDays = availableS, usedDays = usedS}
  where (date, available, used) = splitInfo s

infoFromString :: String -> Merror OffDaysInfo
infoFromString s = case infoFromStringMaybe s of
                    Nothing -> Left (InfoStringInvalid s)
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
-- howManyAccountable ps s e = total*10000000 + general
  where  total = diffDays e s + 1
         general = sum $ map periodLength ps

-- spentDays :: [Period] -> [Period] -> Integer
-- spentDays ps = sum . map (computeWorkingDays ps)

computeOffDays :: Day -> Period -> Reader Float Integer
computeOffDays i0 p = do
  dCoeff <- ask
  return $ min (generatedDays dCoeff) (flooredDays dCoeff)
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
  (l,gHolidays) <- get
  -- (before, after) <- traceShow (splitPeriodList p gHolidays) return $ splitPeriodList p gHolidays
  (before, after) <- return $ splitPeriodList p gHolidays
  -- put (,after)
  cOffDas <- lift $ computeOffDays (lastUpdate i) p
  dCoeff <- lift ask
  -- let newUsedDays = traceShow before computeWorkingDays before p
  let newUsedDays = computeWorkingDays before p
  let result = OffDaysInfo { lastUpdate = addDays (negate (cdDays currentMonths)) (pend p),
                             -- availableDays = traceShow (newAvailableDays cOffDas, flooredDays dCoeff) min (newAvailableDays cOffDas) (flooredDays dCoeff) - newUsedDays,
                             availableDays = min (newAvailableDays cOffDas) (flooredDays dCoeff) - newUsedDays,
                             usedDays = usedDays i + newUsedDays }
               -- usedDays = newUsedDays }
  put (result : l,after)
  return result
  where currentMonths = diffGregorianDurationClip (pend p) (lastUpdate i)
        newAvailableDays offdays = offdays + availableDays i
        flooredDays c = floor $ 12 * c


preProcessPeriods :: [PrePeriod] -> Merror [Period]
preProcessPeriods = fmap sort . mapM fromPreToPeriod

checkPeriodsPrecond :: [Period] -> Bool
checkPeriodsPrecond l = and $ zipWith (<=) ends starts
  where starts = drop 1 $ map pstart l
        ends = map pend l

checkOffDaysCorrect :: [OffDaysInfo] -> Bool
checkOffDaysCorrect = all ((0 <= ) . availableDays)

-- type EnvError a = ExceptT (Either MyError a) (Reader MyEnv) a
-- computeOffDaySeqFromString :: String -> [Period] -> EnvError OffDaysInfo
-- computeOffDaySeqFromString s l = case dayFromString s of
--                                    Left err -> throwE $ Left err
--                                    Right d -> lift $ computeOffDaySeqInit d l

computeOffDaySeq :: OffDaysInfo -> [Period] -> MyState [OffDaysInfo]
computeOffDaySeq i l = do
  x <- foldlM updateOffDays i l
  (t,_) <- get
  return $ reverse t
computeOffDaySeqInit :: Day -> [Period] -> MyState [OffDaysInfo]
computeOffDaySeqInit d = computeOffDaySeq startInfo
  where startInfo = OffDaysInfo {lastUpdate = d, availableDays = 0, usedDays = 0}

-- computeOffDaySeqTrace :: Day -> [Period] -> MyState [OffDaysInfo]
-- -- computeOffDaySeqTrace d l = map (computeOffDaySeq d) (inits l)
-- -- computeOffDaySeqTrace d = scanl updateOffDays startInfo
-- computeOffDaySeqTrace d l = mapM (foldlM updateOffDays startInfo) (inits l)
--   where startInfo = OffDaysInfo {lastUpdate = d, availableDays = 0, usedDays = 0}

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

negativeCheck :: [OffDaysInfo] -> Merror [OffDaysInfo]
-- negativeCheck l = if any ((< 0).availableDays) l then Left (NegativeAvailableDays l) else Right l
negativeCheck l = case find ((< 0).availableDays) l of
                    Nothing -> Right l
                    Just o -> Left $ NegativeAvailableDays o
-- filterRelevantDates :: OffDaysInfo -> [Period] -> [Period]
-- filterRelevantDates o l = dropWhile (<= lastUpdate) l
