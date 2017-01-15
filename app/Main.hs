{-|
Module      : Main
Description : Main module of calculator
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

-- |Function which takes arguments and call other functions and print the result
main :: IO ()
main = do
  putStrLn "Enter your function to differentiation and press enter:"
  functions <- getLine
  putStrLn "Enter the argument x and press enter:"
  argument <- getLine
  if functions == ":q" || functions == ":Q" || argument == ":q" || argument == ":Q"
    then return ()
    else do putStrLn $ "(" ++ functions ++ ")' " ++ "(" ++ argument ++ ")" ++ (function functions argument)
            main
