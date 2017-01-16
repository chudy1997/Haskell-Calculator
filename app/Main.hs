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

-- |Function which prints welcoming text and calls programMain
main :: IO ()
main = do
  putStr "\nCalculator of derivatives\n\nRules:\n1. You have to separable input with spaces!\n2. You can only use one operation!\n3. Follow the instructions precisely\n\n"
  programMain

-- |Function which takes arguments and call other functions and print the result
programMain :: IO ()
programMain = do
  putStrLn "Enter your function to differentiation and press enter:"
  functions <- getLine
  if functions == ":q" || functions == ":Q"
    then return ()
  else do
    putStrLn "Enter the argument x and press enter:"
    argument <- getLine
    if argument == ":q" || argument == ":Q"
    then return ()
    else do
      putStrLn $ "( " ++ functions ++ ")' " ++ "(" ++ argument ++ ")" ++ (function functions argument)
      programMain

--dołożyć wyjątki 


{-
haddock -h ~/Pulpit/Informatyka/drugiRok/Haskell/calculator/app/Main.hs ~/Pulpit/Informatyka/drugiRok/Haskell/calculator/src/BasicDerivatives.hs ~/Pulpit/Informatyka/drugiRok/Haskell/calculator/src/Calculator.hs ~/Pulpit/Informatyka/drugiRok/Haskell/calculator/src/ParsingFunctions.hs
-}
