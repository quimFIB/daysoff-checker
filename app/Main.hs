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
import Lib

data Mode = Single | Trace
data Options = Options {
  _initDate :: String,
  _datesFile :: FilePath,
  _daysCoeff :: String,
  _mode :: Mode
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
               <*> strOption
               (metavar "daysoff_coef"
                <> short 'c'
                <> help "daysoff generated per month")
               <*> flag Single Trace
               ( long "Traced computation"
                 <> short 't'
                 <> help "Return the trace of the computation" )

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


readfile :: String -> IO (Either String [PrePeriod])
readfile f = do
  csvData <- BL.readFile f
  case helper csvData of
    Left err -> return $ Left err
    Right v -> return $ Right (V.toList v)
    where helper :: BL.ByteString -> Either String (V.Vector PrePeriod)
          helper = fmap snd . decodeByName

compute :: Options -> [PrePeriod] -> Merror [OffDaysInfo]
compute Options{..} d = case (readMaybe _daysCoeff) :: Maybe Float of
      Nothing -> Left $ DateStringInvalid _daysCoeff
      Just c -> do
                l <- preProcessPeriods d
                if checkPeriodsPrecond l then
                  do
                    day <- dayFromString _initDate
                    case _mode of
                      Single -> do
                        Right $ return $ runReader (computeOffDaySeq day l) (MyEnv {daysCoeff = c})
                      Trace -> do
                        Right $ runReader (computeOffDaySeqTrace day l) (MyEnv {daysCoeff = c})
                else Left $ OverlappingPeriods l
go :: Options -> IO ()
go o@Options{..} = do
  dates <- readfile _datesFile
  case dates of
    Left err -> print $ "Something went wrong " ++ err
    Right d ->  case compute o d of
      Left err -> print err
      Right result -> print result

main :: IO ()
main = execParser descr >>= go
