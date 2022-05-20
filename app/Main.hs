{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv (decodeByName)
import qualified Data.Vector as V
import Data.Time
import System.Environment
import Options.Applicative as O
import Options.Applicative.Help.Pretty
import Text.Read (readMaybe)
import Data.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.State.Lazy (State)
import Lib

import Text.RawString.QQ
import Text.Regex.TDFA

data Mode = Single | Trace
data Options = Options {
  _initDate :: String,
  _datesFile :: FilePath,
  _holidaysFile :: FilePath,
  _daysCoeff :: String,
  _mode :: Mode
  }

parseOptions :: Parser Options
parseOptions = Options
               <$> strOption
               (long "initial-info"
                <> short 'd'
                <> metavar "init_info"
                <> help "Date, starting used days and starting available days")
               <*> strOption
               (long "file"
                <> metavar "dates_file"
                <> short 'f'
                <> help "Path to a csv with the offdays dates")
               <*> strOption
               (long "file_holidays"
                <> metavar "holidays_file"
                <> short 'h'
                <> value "#.no-file"
                <> help "Path to a csv with the general holidays")
               <*> strOption
               (long "daysoff-coeff"
                <> metavar "daysoff_coef"
                <> short 'c'
                <> help "daysoff generated per month")
               <*> flag Single Trace
               ( long "traced-computation"
                 <> short 't'
                 <> help "Return the trace of the computation" )

descr :: ParserInfo Options
descr = info (parseOptions <**> helper)
       (fullDesc
        <> progDesc "Given a set of request dates for days off, computes whether that request is valid, i.e if the employee has enough offdays generated"
        <> headerDoc (Just $ dullblue $ bold $ underline "program help")
        <> footerDoc (Just $ bold "maintainer: " <> "Quim")
       )

readfile :: String -> IO (Either String [PrePeriod])
readfile f = do
  csvData <- BL.readFile f
  case helper csvData of
    Left err -> return $ Left err
    Right v -> return $ Right (V.toList v)
    where helper :: BL.ByteString -> Either String (V.Vector PrePeriod)
          helper = fmap snd . decodeByName


compute :: Options -> [PrePeriod] -> [PrePeriod] -> Merror [OffDaysInfo]
-- compute :: Options -> [PrePeriod] -> [PrePeriod] -> Merror [Period]
compute Options {..} d h = case readMaybe _daysCoeff :: Maybe Float of
      Nothing -> Left $ DateStringInvalid _daysCoeff
      Just c -> do
        h1 <- preProcessPeriods h
        l <- preProcessPeriods d
        -- Right $ h1 ++ generateWeekends (generalPeriod l)
        if checkPeriodsPrecond l then
                do
                info <- infoFromString _initDate
                let weekends = generateWeekends (generalPeriod l)
                let hList = (sort.nub) (h1 ++ weekends)
                case _mode of
                        Single -> do
                                -- Right $ return $ runReader (evalStateT (computeOffDaySeq day l) (generateWeekends (generalPeriod l))) c
                                let r = map (\l -> runReader (evalStateT (computeOffDaySeq info l) hList) c) (inits l)
                                case negativeCheck r of
                                  Left err -> Left err
                                  Right l -> Right [last l]
                        Trace -> do
                                -- Right $ runReader (evalStateT (computeOffDaySeqTrace day l) (generateWeekends (generalPeriod l))) c
                                -- Right $ mapM (\l -> runReader (evalStateT (computeOffDaySeqTrace day l) (h ++ weekends)) c)
                                -- Right $ map (\l -> runReader (evalStateT (computeOffDaySeq info l) (h ++ weekends)) c) (inits l)
                                let r = map (\l -> runReader (evalStateT (computeOffDaySeq info l) hList) c) (inits l)
                                -- Right r
                                negativeCheck r
        else Left $ OverlappingPeriods l

go :: Options -> IO ()
go o@Options{..} = do
  dates <- readfile _datesFile
  holidays <- readfile _holidaysFile
  case dates of
    Left err -> print $ "Something went wrong " ++ err
    Right d ->  case holidays of
      Left err -> print $ "Something went wrong " ++ err
      Right h -> case compute o d h of
                 Left err -> print err
                 Right result -> print result

main :: IO ()
main = execParser descr >>= go
