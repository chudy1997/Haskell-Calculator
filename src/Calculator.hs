{-|
Module      : Calculator
Description : Module with functions parsing the input and calculating the output
Copyright   : (c) Karol Bartyzel, 2017
License     : GPL-3
Maintainer  : karolbartyzel@interia.pl
Stability   : experimental
Portability : POSIX
-}

module Calculator
(

  calculate,
  function,
  isSignOrSpace,
  words'

) where

import BasicDerivatives
import ParsingFunctions

-- |Function which
calculate :: [[Char]] -> Double -> Double
-- |Function which
function :: [Char] -> [Char] -> [Char]
-- |Function which
isSignOrSpace :: Char -> Bool
-- |Function which
words' :: [Char] -> [[Char]]

calculate (x1:[]) arg = takeMaybePM (d x1 arg)
calculate (x1:z:x2:[]) arg
  |z == "+" || z == "-" = f1 (x1:z:x2:[]) arg
  |z == "*" = f2 (x1:z:x2:[]) arg
  |z == "/" = f3 (x1:z:x2:[]) arg
    where f1 (x1:z:x2:[]) arg = (sign z) (takeMaybePM $ d x1 arg) (takeMaybePM $ d x2 arg)
          f2 (x1:z:x2:[]) arg = (takeMaybePM $ d x1 arg) * (takeMaybeTD $ e x2 arg) + (takeMaybeTD $ e x1 arg) * (takeMaybePM $ d x2 arg)
          f3 (x1:z:x2:[]) arg = ((takeMaybePM $ d x1 arg) * (takeMaybeTD $ e x2 arg) - (takeMaybeTD $ e x1 arg) * (takeMaybePM $ d x2 arg))/(takeMaybePM $ e x2 arg)^2
calculate (x1:z:x2:xs) arg
  |z == "+" || z == "-" = f1 (x1:z:x2:xs) arg
  |z == "*" = f2 (x1:z:x2:xs) arg
  |z == "/" = f3 (x1:z:x2:xs) arg
    where f1 (x1:z:x2:xs) arg = (sign z) (takeMaybePM $ d x1 arg) (calculate (x2:xs) arg)
          f2 (x1:z:x2:xs) arg = (sign $ head xs) ((takeMaybePM $ d x1 arg) * (takeMaybeTD $ e x2 arg) + (takeMaybeTD $ e x1 arg) * (takeMaybePM $ d x2 arg)) (calculate (x2:xs) arg)
          f3 (x1:z:x2:xs) arg = (sign $ head xs) (((takeMaybePM $ d x1 arg) * (takeMaybeTD $ e x2 arg) - (takeMaybeTD $ e x1 arg) * (takeMaybePM $ d x2 arg))/(takeMaybePM $ e x2 arg)^2) (calculate (x2:xs) arg)

function functions arg = " = " ++ (show $ calculate wor (read arg))
    where
      --func = init wor
      wor = words functions --if (last $ init $ words' xs) == "-" then init (init $ words' xs) ++ ['-':(last $ words' xs)] else words' xs

isSignOrSpace c = c == ' ' || c == '+' || c == '-' || c == '*' || c == '/'

words' [] = []
words' cs =
  let (pre, suf) = break isSignOrSpace cs
  in if null pre == False
    then pre:(f suf)
    else f suf
      where f suf = case suf of
                      (' ':rest) -> words' rest
                      ('+':rest) -> "+" : words' rest
                      ('-':rest) -> "-" : words' rest
                      ('*':rest) -> "*" : words' rest
                      ('/':rest) -> "/" : words' rest
                      _ -> []
