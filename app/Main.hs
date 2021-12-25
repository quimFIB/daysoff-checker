{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Lib
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Text.RawString.QQ
import Text.Regex.TDFA

data Holiday = Holiday
    { start :: !String
    , end:: !String
    , non_working:: !String
    , total:: !String
    }

instance FromNamedRecord Holiday where
    parseNamedRecord r = Holiday <$> r .: "Start" <*> r .: "End" <*> r .: "Non-working" <*> r .: "Total"

-- readfile :: IO String
readfile = do
  csvData <- BL.readFile "data/calendar1.csv"
  case decodeByName csvData of
    Left err -> return err
    Right (_, v) -> V.forM_ v $ \ p -> return (start p ++ end p ++ non_working p ++ total p)

main :: IO ()
main = do
    csvData <- BL.readFile "data/calendar1.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ start p ++ end p ++ non_working p ++ total p
