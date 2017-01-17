{-|
Module      : ParsingFunctions
Description : module with functions parsing math functions names
Copyright   : (c) Karol Bartyzel, 2017
License     : GPL-3
Maintainer  : karolbartyzel@interia.pl
Stability   : experimental
Portability : POSIX
-}

module ParsingFunctions
(

-- *Parsing functions
  d,
  e,
  isNumber,
  sign

) where

import BasicDerivatives
import Data.Char (isDigit)

-- |Function which takes name of function (eventually additional number) and returns it's derivative
d :: [Char] -> Double -> (Double -> Maybe Double)
-- |Function which takes name of function (eventually additional number) and returns function itself
e :: [Char] -> Double -> (Double -> Maybe Double)
-- |Function which checks if string is a number
isNumber :: [Char] -> Bool
-- |Function which checks math signs
sign :: [Char] -> (Double -> Double -> Double)

d f a =
 if isNumber f then dc 0
 else case f of
   "acos" -> dacos 0
   "acosh" -> dacosh 0
   "acot" -> dacot 0
   "acoth" -> dacoth 0
   "acsc" -> dacsc 0
   "acsch" -> dacsch 0
   "asec" -> dasec 0
   "asech" -> dasech 0
   "asin" -> dasin 0
   "asinh" -> dasinh 0
   "atan" -> datan 0
   "atanh" -> datanh 0
   "aToX" -> daToX a
   "cos" -> dcos 0
   "cosh" -> dcosh 0
   "cot" -> dcot 0
   "coth" -> dcoth 0
   "csc" -> dcsc 0
   "csch" -> dcsch 0
   "exp" -> dexp 0
   "log" -> dlog 0
   "loga" -> dloga a
   "sec" -> dsec 0
   "sech" -> dsech 0
   "sin" -> dsin 0
   "sinh" -> dsinh 0
   "tan" -> dtan 0
   "tanh" -> dtanh 0
   "xToN" -> dxToN a
   "xToX" -> dxToX 0
   _ -> (\e -> Nothing)

e f a =
 if isNumber f then (\_ -> Just $ read f)
 else case f of
   "acos" -> acos' 0
   "acosh" -> acosh' 0
   "acot" -> acot 0
   "acoth" -> acoth 0
   "acsc" -> acsc 0
   "acsch" -> acsch 0
   "asec" -> asec 0
   "asech" -> asech 0
   "asin" -> asin' 0
   "asinh" -> asinh' 0
   "atan" -> atan' 0
   "atanh" -> atanh' 0
   "aToX" -> aToX a
   "cos" -> cos' 0
   "cosh" -> cosh' 0
   "cot" -> cot 0
   "coth" -> coth 0
   "csc" -> csc 0
   "csch" -> csch 0
   "exp" -> exp' 0
   "log" -> log' 0
   "loga" -> loga a
   "sec" -> sec 0
   "sech" -> sech 0
   "sin" -> sin' 0
   "sinh" -> sinh' 0
   "tan" -> tan' 0
   "tanh" -> tanh' 0
   "xToN" -> xToN a
   "xToX" -> xToX 0
   _ -> (\e ->Nothing)

isNumber (x:xs) = if x=='-' then isNumber' xs else isNumber' (x:xs)
  where
    isNumber' (x:[]) = isDigit x
    isNumber' (x:xs) = isDigit x && isNumber' xs

sign s = case s of
  "+" -> (+)
  "-" -> (-)
  "*" -> (*)
  "/" -> (/)
  _ -> (\e f -> 0)
