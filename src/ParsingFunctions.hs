{-|
Module      : ParsingFunctions
Description : Module with functions parsing math functions names
Copyright   : (c) Karol Bartyzel, 2017
License     : GPL-3
Maintainer  : karolbartyzel@interia.pl
Stability   : experimental
Portability : POSIX
-}

module ParsingFunctions
(

  d,
  e,
  isNumber,
  sign

) where

import BasicDerivatives
import Data.Char (isDigit)

-- |Function which
d :: [Char] -> (Double -> Maybe Double)
-- |Function which
e :: [Char] -> (Double -> Maybe Double)
-- |Function which
isNumber :: [Char] -> Bool
-- |Function which 
sign :: [Char] -> (Double -> Double -> Double)

d f =
 if isNumber f then dc

 else case f of

 --"loga" -> loga
 --"xToN" -> xToN
 --"aToX" -> aToX

   "acos" -> dacos
   "acosh" -> dacosh
   "acot" -> dacot
   "acoth" -> dacoth
   "acsc" -> dacsc
   "acsch" -> dacsch
   "asec" -> dasec
   "asech" -> dasech
   "asin" -> dasin
   "asinh" -> dasinh
   "atan" -> datan
   "atanh" -> datanh
   "cos" -> dcos
   "cosh" -> dcosh
   "cot" -> dcot
   "coth" -> dcoth
   "csc" -> dcsc
   "csch" -> dcsch
   "exp" -> dexp
   "log" -> dlog
   "log10" -> dlog10
   "sec" -> dsec
   "sech" -> dsech
   "sin" -> dsin
   "sinh" -> dsinh
   "tan" -> dtan
   "tanh" -> dtanh
   "xToX" -> dxToX
   _ -> (\e -> Nothing)

e f =
 if isNumber f then (\_ -> Just $ read f)

 else case f of

--"loga" -> loga
--"xToN" -> xToN
--"aToX" -> aToX

   "acos" -> acos'
   "acosh" -> acosh'
   "acot" -> acot
   "acoth" -> acoth
   "acsc" -> acsc
   "acsch" -> acsch
   "asec" -> asec
   "asech" -> asech
   "asin" -> asin'
   "asinh" -> asinh'
   "atan" -> atan'
   "atanh" -> atanh'
   "cos" -> cos'
   "cosh" -> cosh'
   "cot" -> cot
   "coth" -> coth
   "csc" -> csc
   "csch" -> csch
   "exp" -> exp'
   "log" -> log'
   "log10" -> log10
   "sec" -> sec
   "sech" -> sech
   "sin" -> sin'
   "sinh" -> sinh'
   "tan" -> tan'
   "tanh" -> tanh'
   "xToX" -> xToX
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
