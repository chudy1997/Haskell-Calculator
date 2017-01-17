{-|
Module      : Main
Description : main module of calculator
Copyright   : (c) Karol Bartyzel, 2017
License     : GPL-3
Maintainer  : karolbartyzel@interia.pl
Stability   : experimental
Portability : POSIX
-}

module Main where

import BasicDerivatives
import Calculator
import ParsingFunctions

-- * Main Functions
-- |Function which prints welcoming text and calls parseFunctionsNames
main :: IO ()
main = do
  putStr "\nCalculator of derivatives\n\nRules:\n1. You have to separable input with spaces!\n2. You can only use one operation!\n3. Follow the instructions precisely\n\n"
  parseFunctionsNames

-- |Function which depending on length of argument line calls appropriate function
parseFunctionsNames :: IO ()
parseFunctionsNames = do
  putStrLn "Enter your function to differentiation and press enter:"
  functions <- getLine
  case length (words functions) of
    1 -> parseNumberOne functions
    2 -> parseNumberTwo functions
    3 -> parseNumberThree functions
    4 -> parseNumberFour functions
    5 -> parseNumberFive functions
    _ -> parseNumberZero

-- |Function which takes argument and if there's a valid number it calculate the result and print
parseArgumentsName :: [Char] -> IO ()
parseArgumentsName functions = do
  putStrLn "Enter the argument x and press enter:"
  argument <- getLine
  if argument == ":q" || argument == ":Q"
    then return ()
  else if not $ isNumber argument
    then do
      parseNumberZero
  else do
    putStrLn $ "(" ++ (if last functions == ' ' then init functions else functions) ++ ")' " ++ "(" ++ argument ++ ")" ++ (function functions argument)
    parseFunctionsNames

-- * Other Functions

-- |Function which shows message and start again
parseNumberZero :: IO ()
parseNumberZero = do
  putStrLn "\nSorry, but You broke the rules :(\nYou gave too much or too less arguments!\nTry again\n"
  parseFunctionsNames

-- |Function which checks correctness of given functions' names and if positive calls parseArgumentsName in case of bad arguments in case of 1-word commandline
parseNumberOne :: [Char] -> IO ()
parseNumberOne functions = do
  if e functions 0 0 == Nothing && not (functions == ":q" || functions == ":Q")
    then do
      parseNumberZero
  else do
    if functions == ":q" || functions == ":Q"
      then return ()
    else do
      parseArgumentsName functions

-- |Function which checks correctness of given functions' names and if positive calls parseArgumentsName in case of bad arguments in case of 2-words commandline
parseNumberTwo :: [Char] -> IO ()
parseNumberTwo functions = do
  if (head (words functions) /= "aToX" && head (words functions) /= "loga" && head (words functions) /= "xToN") || not (isNumber $ last $ words functions)
    then do
      parseNumberZero
  else do
    if functions == ":q" || functions == ":Q"
      then return ()
    else do
      parseArgumentsName functions

-- |Function which checks correctness of given functions' names and if positive calls parseArgumentsName in case of bad arguments in case of 3-words commandline
parseNumberThree :: [Char] -> IO ()
parseNumberThree functions = do
  if e (head (words functions)) 0 0 == Nothing || e (last (words functions)) 0 0 == Nothing || sign (head (tail (words functions))) 2 1 == 0
    then do
      parseNumberZero
  else do
    if functions == ":q" || functions == ":Q"
      then return ()
    else do
      parseArgumentsName functions

-- |Function which checks correctness of given functions' names and if positive calls parseArgumentsName in case of bad arguments in case of 4-words commandline
parseNumberFour :: [Char] -> IO ()
parseNumberFour functions = do
  if not (sign (head (tail (words functions))) 2 1 == 0)
    then do
      if e (head (words functions)) 0 0 == Nothing || (last (init (words functions)) /= "aToX" && last (init (words functions)) /= "loga" && last (init (words functions)) /= "xToN") ||
        not (isNumber $ last $ words functions)
        then do
          parseNumberZero
      else do
        if functions == ":q" || functions == ":Q"
          then return ()
        else do
          parseArgumentsName functions
  else if not (sign (last (init (words functions))) 2 1 == 0)
    then do
      if e (last (words functions)) 0 0 == Nothing || (head (words functions) /= "aToX" &&  head (words functions) /= "loga" &&  head (words functions) /= "xToN") ||
        not (isNumber $ head $ tail $ words functions)
        then do
          parseNumberZero
      else do
        if functions == ":q" || functions == ":Q"
          then return ()
        else do
          parseArgumentsName functions
  else do
    parseNumberZero


-- |Function which checks correctness of given functions' names and if positive calls parseArgumentsName in case of bad arguments in case of 5-words commandline
parseNumberFive :: [Char] -> IO ()
parseNumberFive functions = do
  if (head (words functions) /= "aToX" && head (words functions) /= "loga" && head (words functions) /= "xToN") || (last (init (words functions)) /= "aToX" && last (init (words functions)) /= "loga" && last (init (words functions)) /= "xToN") ||
    not (isNumber $ last $ words functions) || not (isNumber $ head $ tail $ words functions)
    then do
      parseNumberZero
  else do
    if functions == ":q" || functions == ":Q"
      then return ()
    else do
      parseArgumentsName functions


{-
haddock -h ~/Pulpit/Informatyka/drugiRok/Haskell/calculator/app/Main.hs ~/Pulpit/Informatyka/drugiRok/Haskell/calculator/src/BasicDerivatives.hs ~/Pulpit/Informatyka/drugiRok/Haskell/calculator/src/Calculator.hs ~/Pulpit/Informatyka/drugiRok/Haskell/calculator/src/ParsingFunctions.hs
-}
