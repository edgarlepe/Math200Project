{-|
Module      : Main
Description : This module contains the application
Copyright   : (c) Edgar Giovanni Lepe, 2018
License     : BSD3
Portability :
-}
module Main where

import           Lib
import           Text.Parsec   (runParser)
import qualified Data.Map as Map
import           System.IO (stdout, hFlush)

main :: IO ()
main = do
  printPrompt
  runInterpreter emptyState
  where emptyState = Map.empty

runInterpreter userState = do
  input <- getLine
  case input of
    "quit" -> return ()
    _ -> case runParser mainParser userState "" input of
      (Right (output, newState)) -> do
        putStrLn $ "=> " ++ output
        printPrompt
        runInterpreter newState
      (Left err) -> do
        print err
        printPrompt
        runInterpreter userState

printPrompt :: IO ()
printPrompt = putStr "RunInterpreter> " >> hFlush stdout
