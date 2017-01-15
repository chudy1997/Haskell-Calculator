{-|
Module      : BasicDerivatives
Description : Module with math functions used in calculator
Copyright   : (c) Karol Bartyzel, 2017
License     : GPL-3
Maintainer  : karolbartyzel@interia.pl
Stability   : experimental
Portability : POSIX
-}

module BasicDerivatives
(

{-
  aToX,
  loga,
  xToN,

  daToX,
  dloga,
  dxToN,
-}

  acos',
  acosh',
  acot,
  acoth,
  acsc,
  acsch,
  asec,
  asech,
  asin',
  asinh',
  atan',
  atanh',
  cos',
  cosh',
  cot,
  coth,
  csc,
  csch,
  exp',
  log',
  log10,
  sec,
  sech,
  sin',
  sinh',
  tan',
  tanh',
  xToX,

  dacos,
  dacosh,
  dacot,
  dacoth,
  dacsc,
  dacsch,
  dasec,
  dasech,
  dasin,
  dasinh,
  datan,
  datanh,
  dc,
  dcos,
  dcosh,
  dcot,
  dcoth,
  dcsc,
  dcsch,
  dexp,
  dlog,
  dlog10,
  dsec,
  dsech,
  dsin,
  dsinh,
  dtan,
  dtanh,
  dxToX,

  takeMaybeTD,
  takeMaybePM

) where



--Deklaracje


-- |Function which
aToX :: Double -> Double -> Maybe Double
-- |Function which
loga :: Double -> Double -> Maybe Double
-- |Function which
xToN :: Double ->Double -> Maybe Double

-- |Function which
daToX :: Double -> Double -> Maybe Double
-- |Function which
dloga :: Double -> Double -> Maybe Double
-- |Function which
dxToN :: Double -> Double -> Maybe Double

-- |Function which
acos' :: Double -> Maybe Double
-- |Function which
acosh' :: Double -> Maybe Double
-- |Function which
acot :: Double -> Maybe Double
-- |Function which
acoth :: Double -> Maybe Double
-- |Function which
acsc :: Double -> Maybe Double
-- |Function which
acsch :: Double -> Maybe Double
-- |Function which
asec :: Double -> Maybe Double
-- |Function which
asech :: Double -> Maybe Double
-- |Function which
asin' :: Double -> Maybe Double
-- |Function which
asinh' :: Double -> Maybe Double
-- |Function which
atan' :: Double -> Maybe Double
-- |Function which
atanh' :: Double -> Maybe Double
-- |Function which
cos' :: Double -> Maybe Double
-- |Function which
cosh' :: Double -> Maybe Double
-- |Function which
cot :: Double -> Maybe Double
-- |Function which
coth :: Double -> Maybe Double
-- |Function which
csc :: Double -> Maybe Double
-- |Function which
csch :: Double -> Maybe Double
-- |Function which
exp' :: Double -> Maybe Double
-- |Function which
log' :: Double -> Maybe Double
-- |Function which
log10 :: Double -> Maybe Double
-- |Function which
sec :: Double -> Maybe Double
-- |Function which
sech :: Double -> Maybe Double
-- |Function which
sin' :: Double -> Maybe Double
-- |Function which
sinh' :: Double -> Maybe Double
-- |Function which
tan' :: Double -> Maybe Double
-- |Function which
tanh' :: Double -> Maybe Double
-- |Function which
xToX :: Double -> Maybe Double

-- |Function which
dacos :: Double -> Maybe Double
-- |Function which
dacosh :: Double -> Maybe Double
-- |Function which
dacot :: Double -> Maybe Double
-- |Function which
dacoth :: Double -> Maybe Double
-- |Function which
dacsc :: Double -> Maybe Double
-- |Function which
dacsch :: Double -> Maybe Double
-- |Function which
dasec :: Double -> Maybe Double
-- |Function which
dasech :: Double -> Maybe Double
-- |Function which
dasin :: Double -> Maybe Double
-- |Function which
dasinh :: Double -> Maybe Double
-- |Function which
datan :: Double -> Maybe Double
-- |Function which
datanh :: Double -> Maybe Double

-- |Function which
dc :: Double -> Maybe Double
-- |Function which
dcos :: Double -> Maybe Double
-- |Function which
dcosh :: Double -> Maybe Double
-- |Function which
dcot :: Double -> Maybe Double
-- |Function which
dcoth :: Double -> Maybe Double
-- |Function which
dcsc :: Double -> Maybe Double
-- |Function which
dcsch :: Double -> Maybe Double
-- |Function which
dexp :: Double -> Maybe Double
-- |Function which
dlog :: Double -> Maybe Double
-- |Function which
dlog10 :: Double -> Maybe Double
-- |Function which
dsec :: Double -> Maybe Double
-- |Function which
dsech :: Double -> Maybe Double
-- |Function which
dsin :: Double -> Maybe Double
-- |Function which
dsinh :: Double -> Maybe Double
-- |Function which
dtan :: Double -> Maybe Double
-- |Function which
dtanh :: Double -> Maybe Double
-- |Function which
dxToX :: Double -> Maybe Double

-- |Function which
takeMaybeTD :: Maybe Double -> Double --Time/Divide
-- |Function which
takeMaybePM :: Maybe Double -> Double --Plus/Minus

--Definicje



aToX a x
  |a>0 && a/= 1 = Just (a ** x)
  |otherwise = Nothing
loga a x
  |a>0 && a/=1 && x>0 = Just (logBase a x)
  |otherwise = Nothing
xToN x n = Just (x ** n)

daToX a x
  |a>0 && a/= 1 = Just (a**x * (log a))
dloga a x
  |a <= 0 || a == 1 || x <= 0 = Nothing
  |otherwise = Just (1/(x*(log a)))
dxToN n x = Just (n* x**(n-1))

acos' x
  |abs (x) < 1 = Just (acos x) -- <=
  |otherwise = Nothing
acosh' x
  |abs(x) > 1 = Just (acosh x)
  |otherwise = Nothing
acot x
  |x /= 0 = Just (atan $ 1/x)
  |otherwise = Just (pi/2)
acoth x
  |abs(x) > 1 = Just (atanh $ 1/x)
  |otherwise = Nothing
acsc x
  |abs(x) > 1 = Just (asin $ 1/x)
  |otherwise = Nothing
acsch x
  |x /= 0 = Just (asinh $ 1/x)
  |otherwise = Nothing
asec x
  |abs(x) > 1 = Just (acos $ 1/x)
  |otherwise = Nothing
asech x
  |x /= 0 && abs(x) < 1 = Just (acosh $ 1/x)
  |otherwise = Nothing
asin' x
  |abs (x) < 1 = Just (asin x) -- <=
  |otherwise = Nothing
asinh' x = Just (asinh x)
atan' x = Just (atan x)
atanh' x
  |abs(x) < 1 = Just (atanh x)
  |otherwise = Nothing
cos' x = Just (cos x)
cosh' x = Just (cosh x)
cot x
  |sin x /= 0 = Just ((cos x)/(sin x))
  |otherwise = Nothing
coth x
  |sinh x /= 0 = Just ((cosh x)/(sinh x))
  |otherwise = Nothing
csc x
  |sin x /= 0 = Just (1/(sin x))
  |otherwise = Nothing
csch x
  |sinh x /= 0 = Just (1/(sinh x))
  |otherwise = Nothing
exp' x = Just (exp x)
log' x
  |x>0 = Just (log x)
  |otherwise = Nothing
log10 x
  |x>0 = Just (logBase 10 x)
  |otherwise = Nothing
sec x
  |cos x /= 0 = Just (1/(cos x))
  |otherwise = Nothing
sech x
  |cosh x /= 0 = Just (1/(cosh x))
  |otherwise = Nothing
sin' x = Just (sin x)
sinh' x = Just (sinh x)
tan' x
  |cos x /= 0 = Just (tan x)
  |otherwise = Nothing
tanh' x = Just (tanh x)
xToX x
  |x > 0 = Just (x ** x)
  |otherwise = Nothing

dacos x
  |abs(x) < 1 = Just (-1/sqrt(1-x^2))
  |otherwise = Nothing
dacosh x
  |abs(x) > 1 = Just (1/sqrt(x^2-1))
  |otherwise = Nothing
dacot x = Just (-1/(1+x^2))
dacoth x
  |abs(x) > 1 = Just (-1/(x^2-1))
  |otherwise = Nothing
dacsc x
  |abs(x) > 1 = Just (-1/(x*sqrt(x^2-1)))
  |otherwise = Nothing
dacsch x
  |x /= 0 = Just (-1/(x*sqrt(1+x^2)))
  |otherwise = Nothing
dasec x
  |abs(x) > 1 = Just (1/(x*sqrt(x^2-1)))
  |otherwise = Nothing
dasech x
  |x /= 0 && abs(x)<1 = Just (-1/(x*sqrt(1-x^2)))
  |otherwise = Nothing
dasin x
  |abs(x) < 1 = Just (1/sqrt(1-x^2))
  |otherwise = Nothing
dasinh x = Just (1/sqrt(1+x^2))
datan x = Just (1/(1+x^2))
datanh x
  |abs(x) < 1 = Just (1/(1-x^2))
  |otherwise = Nothing
dc c = Just 0
dcos x = Just (-(sin x))
dcosh x = Just (sinh x)
dcot x
  |sin x /= 0 = Just (-(takeMaybePM (csc x))^2)
  |otherwise = Nothing
dcoth x
  |sinh x /= 0 = Just (-(takeMaybePM (csch x))^2)
  |otherwise = Nothing
dcsc x
  |sin x /= 0 = Just (-(cos x)/(sin x)^2)
  |otherwise = Nothing
dcsch x
  |sinh x /= 0 = Just (-(takeMaybePM (csch x))*(takeMaybePM (coth x)))
  |otherwise = Nothing
dexp x = Just (exp x)
dlog x
  |x > 0 = Just (1/x)
  |otherwise = Nothing
dlog10 x = dloga 10 x
dsec x
  |cos x /= 0 = Just ((sin x)/(cos x)^2)
  |otherwise = Nothing
dsech x
  |cosh x /= 0 = Just (-(takeMaybePM (sech x))*(tanh x))
  |otherwise = Nothing
dsin x = Just (cos x)
dsinh x = Just (cosh x)
dtan x
  |cos x /= 0 = Just ((takeMaybePM (sec x))^2)
  |otherwise = Nothing
dtanh x = Just ((takeMaybePM (sech x))^2)
dxToX x
  |x > 0 = Just (x**x * (1+log x))
  |otherwise = Nothing

takeMaybeTD (Just x) = x
takeMaybeTD Nothing = 1

takeMaybePM (Just x) = x
takeMaybePM Nothing = 0
