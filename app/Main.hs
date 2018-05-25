{-|
Module      : Main
Description : This module contains the application
Copyright   : (c) Edgar Giovanni Lepe, 2018
License     : BSD3
Maintainer  : lepe.edgar10@gmail.com

-}
module Main where

import qualified Data.Map  as Map
import           Lib
import           System.IO (hFlush, stdout)

main :: IO ()
main = repl emptyState
  where emptyState = Map.empty

repl userState = do
  printPrompt
  input <- getLine
  case input of
    "quit" -> return ()
    _ -> do
      let (output, st) = interpret input userState
      putStrLn $ "=> " ++ output
      repl st
  where printPrompt = putStr "Interpreter> " >> hFlush stdout
