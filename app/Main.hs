{-|
Module      : Main
Description : This module contains the application
Copyright   : (c) Edgar Giovanni Lepe, 2018
License     : BSD3
Portability :
-}
module Main where

import           Lib
import           Text.Parsec   (parse)

main :: IO ()
main = do
  input <- getLine
  case input of
    "quit" -> return ()
    _      -> putStrLn (interpret input) >> main

interpret :: String -> String
interpret s = case parse parseTerm "" s of
                Left err  -> show err
                Right val -> show val
