{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Time
import System.Environment
import Options.Applicative
import Options.Applicative.Help.Pretty
import Text.Read (readMaybe)
import Data.List
import Lib


test :: Maybe [PrePeriod] -> Maybe [Period]
test l = case l of
           Nothing -> Nothing
           Just l' -> mapM fromPreToPeriod l'

-- Dummy function, just to remind me of how this works
test2 :: IO (Maybe [PrePeriod]) -> IO (Maybe [Period])
test2 l = test <$> l

-- test3 :: Maybe [Period] -> Maybe [Period]
-- test3 l = case l of
--             Nothing -> Nothing
--             Just l' -> return $ sort l'

-- preProcess :: Maybe [PrePeriod] -> Maybe [Period]
-- preProcess l = fmap sort $ l >>= mapM fromPreToPeriod

-- what = map (map periodToWorkingDays) <$> days
initDate = fromGregorian 2020 10 01
todayDate = fromGregorian 2021 12 27
s = fromGregorian 2021 12 08
s2 = fromGregorian 2021 12 11
e = fromGregorian 2021 12 16

-- days = aux <$> readfile
-- od0 = OffDaysInfo {lastUpdate = initDate, availableDays = 0, usedDays = 0}
-- updateInfo = (days, od0)

-- subseq = fmap inits <$> days
-- mytrace = fmap (map (foldl updateOffDays od0))  <$> subseq

-- main :: IO ()
-- main = do
--   dates <- readfile
--   case list of
--     Nothing -> putStrLn "nothing to show"
--     Just l -> print $ fmap (map (foldl updateOffDays od0)) list

data Options = Options {
  _initDate :: String,
  _datesFile :: FilePath
  }

parseOptions :: Parser Options
parseOptions = Options
               <$> strOption
               (long "initial-date"
                <> short 'd'
                <> metavar "init_date"
                <> showDefault
                <> value "build"
                <> help "Date to start calculating offdays")
               <*> strOption
               (metavar "DATES_FILE"
                <> short 'f'
                <> help "Path to a csv with the offdays dates")

descr :: ParserInfo Options
descr = info (parseOptions <**> helper)
       (fullDesc
        <> progDesc "Parse an org mode file with a datalang specification and compile it to all available formats."
        <> headerDoc (Just $ dullblue $ bold $ underline "datalang help")
        <> footerDoc (Just foot)
       )
  where
  foot :: Doc
  foot = bold "maintainer: " <> "Quim"


go :: Options -> IO ()
go Options{..} = do
  dates <- readfile _datesFile
  case dates of
    Nothing -> print "Something went wrong"
    Just d -> print $ preProcessPeriods d >>= computeOffDaySeq _initDate

main :: IO ()
main = execParser descr >>= go
