module Main where

import Lib
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Text.RawString.QQ
import Text.Regex.TDFA
import Data.Time
import Data.List
-- import Data.Vector


test :: Maybe [PrePeriod] -> Maybe [Period]
test l = case l of
           Nothing -> Nothing
           Just l' -> mapM fromPreToPeriod l'

-- Dummy function, just to remind me of how this works
test2 :: IO (Maybe [PrePeriod]) -> IO (Maybe [Period])
test2 l = test <$> l

test3 :: Maybe [Period] -> Maybe [Period]
test3 l = case l of
            Nothing -> Nothing
            Just l' -> return $ sort l'

aux :: Maybe [PrePeriod] -> Maybe [Period]
aux l = fmap sort $ l >>= mapM fromPreToPeriod

initDate = fromGregorian 2020 10 01
todayDate = fromGregorian 2021 12 27
s = fromGregorian 2021 12 08
s2 = fromGregorian 2021 12 11
e = fromGregorian 2021 12 16

main :: IO ()
main = do
  list <- readfile
  case list of
    Nothing -> putStrLn "nothing to show"
    Just l -> mapM_ (print . fromPreToPeriod) l
-- main :: IO ()
-- main = do
--     csvData <- BL.readFile "data/calendar1.csv"
--     case decodeByName csvData of
--         Left err -> putStrLn err
--         Right (_, v) -> V.forM_ v $ \ p ->
--             putStrLn $ start p ++ end p ++ non_working p ++ total p
