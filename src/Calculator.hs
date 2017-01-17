{-|
Module      : Calculator
Description : module with functions parsing the input and calculating the output
Copyright   : (c) Karol Bartyzel, 2017
License     : GPL-3
Maintainer  : karolbartyzel@interia.pl
Stability   : experimental
Portability : POSIX
-}

module Calculator
(

  -- *Calculating function
  function

) where

import BasicDerivatives
import ParsingFunctions

-- |Function which calls 'calculate' to calculate result and shows right side of output
function :: [Char] -> [Char] -> [Char]

-- |Function which calculates result of differentiation of given functions and argument
calculate :: [[Char]] -> Double -> Double
-- |Function which checks if we need second argument to function
isArgumentNeeded :: [Char] -> Bool
-- |Function which checks if argument is a math sign or space
isSignOrSpace :: Char -> Bool
-- |Function which split string into math functions names and math signs
words' :: [Char] -> [[Char]]

calculate (x1:[]) arg = takeMaybePM (d x1 0 arg)

calculate (x1:x2:[]) arg = takeMaybePM (d x1 (read x2) arg)

calculate (x1:x2:x3:[]) arg
  |x2 == "+" || x2 == "-" = f1 (x1:x2:x3:[]) arg
  |x2 == "*" = f2 (x1:x2:x3:[]) arg
  |x2 == "/" = f3 (x1:x2:x3:[]) arg
    where f1 (x1:x2:x3:[]) arg = (sign x2) (takeMaybePM $ d x1 0 arg) (takeMaybePM $ d x3 0 arg)
          f2 (x1:x2:x3:[]) arg = (takeMaybePM $ d x1 0 arg) * (takeMaybeTD $ e x3 0 arg) + (takeMaybeTD $ e x1 0 arg) * (takeMaybePM $ d x3 0 arg)
          f3 (x1:x2:x3:[]) arg = ((takeMaybePM $ d x1 0 arg) * (takeMaybeTD $ e x3 0 arg) - (takeMaybeTD $ e x1 0 arg) * (takeMaybePM $ d x3 0 arg))/(takeMaybePM $ e x3 0 arg)^2

calculate (x1:x2:x3:x4:[]) arg =
  if isArgumentNeeded x1
    then
      if x3 == "+" || x3 == "-" then f11 (x1:x2:x3:x4:[]) arg
      else if x3 == "*" then f12 (x1:x2:x3:x4:[]) arg
      else if x3 == "/" then f13 (x1:x2:x3:x4:[]) arg
      else 0
    else
      if x2 == "+" || x2 == "-" then f21 (x1:x2:x3:x4:[]) arg
      else if x2 == "*" then f22 (x1:x2:x3:x4:[]) arg
      else if x2 == "/" then f23 (x1:x2:x3:x4:[]) arg
      else 0
        where
          f11 (x1:x2:x3:x4:[]) arg = (sign x3) (takeMaybePM $ d x1 (read x2) arg) (takeMaybePM $ d x4 0 arg)
          f12 (x1:x2:x3:x4:[]) arg = (takeMaybePM $ d x1 (read x2) arg) * (takeMaybeTD $ e x4 0 arg) + (takeMaybeTD $ e x1 (read x2) arg) * (takeMaybePM $ d x4 0 arg)
          f13 (x1:x2:x3:x4:[]) arg = ((takeMaybePM $ d x1 (read x2) arg) * (takeMaybeTD $ e x4 0 arg) - (takeMaybeTD $ e x1 (read x2) arg) * (takeMaybePM $ d x4 0 arg))/(takeMaybePM $ e x4 0 arg)^2
          f21 (x1:x2:x3:x4:[]) arg = (sign x2) (takeMaybePM $ d x1 0 arg) (takeMaybePM $ d x3 (read x4) arg)
          f22 (x1:x2:x3:x4:[]) arg = (takeMaybePM $ d x1 0 arg) * (takeMaybeTD $ e x3 (read x4) arg) + (takeMaybeTD $ e x1 0 arg) * (takeMaybePM $ d x3 (read x4) arg)
          f23 (x1:x2:x3:x4:[]) arg = ((takeMaybePM $ d x1 0 arg) * (takeMaybeTD $ e x3 (read x4) arg) - (takeMaybeTD $ e x1 0 arg) * (takeMaybePM $ d x3 (read x4) arg))/(takeMaybePM $ e x3 (read x4) arg)^2

calculate (x1:x2:x3:x4:x5:[]) arg
  |isSignOrSpace $ head x3 =
    if x3 == "+" || x3 == "-"
      then f1 (x1:x2:x3:x4:x5:[]) arg
    else if x3 == "*"
      then f2 (x1:x2:x3:x4:x5:[]) arg
    else if x3 == "/"
      then f3 (x1:x2:x3:x4:x5:[]) arg
    else 0
  |otherwise = 0
    where f1 (x1:x2:x3:x4:x5:[]) arg = (sign x3) (takeMaybePM $ d x1 (read x2) arg) (takeMaybePM $ d x4 (read x5) arg)
          f2 (x1:x2:x3:x4:x5:[]) arg = (takeMaybePM $ d x1 (read x2) arg) * (takeMaybeTD $ e x4 (read x5) arg) + (takeMaybeTD $ e x1 (read x2) arg) * (takeMaybePM $ d x4 (read x5) arg)
          f3 (x1:x2:x3:x4:x5:[]) arg = ((takeMaybePM $ d x1 (read x2) arg) * (takeMaybeTD $ e x4 (read x5) arg) - (takeMaybeTD $ e x1 (read x2) arg) * (takeMaybePM $ d x4 (read x5) arg))/(takeMaybePM $ e x4 (read x5) arg)^2

calculate (x1:x2:x3:x4:x5:xs) arg = 0

function functions arg = " = " ++ (show $ calculate (words functions) (read arg))

isArgumentNeeded arg = arg=="aToX" || arg=="loga" || arg == "xToN"

isSignOrSpace c = c == ' ' || c == '+' || c == '-' || c == '*' || c == '/'

words' [] = []
words' cs =
  let (pre, suf) = break isSignOrSpace cs
  in if not $ null pre
    then pre:(f suf)
    else f suf
      where f suf = case suf of
                      (' ':rest) -> words' rest
                      ('+':rest) -> "+" : words' rest
                      ('-':rest) -> "-" : words' rest
                      ('*':rest) -> "*" : words' rest
                      ('/':rest) -> "/" : words' rest
                      _ -> []
