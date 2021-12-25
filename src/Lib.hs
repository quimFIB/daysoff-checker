{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( someFunc
    ) where

import Text.RawString.QQ
import Text.Regex.TDFA

someFunc :: IO ()
someFunc = putStrLn "someFunc"
