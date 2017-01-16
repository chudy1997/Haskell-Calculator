{-|
Module      : BasicDerivatives
Description : module with math functions used in calculator
Copyright   : (c) Karol Bartyzel, 2017
License     : GPL-3
Maintainer  : karolbartyzel@interia.pl
Stability   : experimental
Portability : POSIX
-}

module BasicDerivatives
(

-- * Plain functions
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
  aToX,
  cos',
  cosh',
  cot,
  coth,
  csc,
  csch,
  exp',
  log',
  loga,
  sec,
  sech,
  sin',
  sinh',
  tan',
  tanh',
  xToN,
  xToX,

  -- * Derivatives of functions

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
  daToX,
  dc,
  dcos,
  dcosh,
  dcot,
  dcoth,
  dloga,
  dcsc,
  dcsch,
  dexp,
  dlog,
  dsec,
  dsech,
  dsin,
  dsinh,
  dtan,
  dtanh,
  dxToN,
  dxToX,

  -- *Opening Maybe functions

  takeMaybeTD,
  takeMaybePM

) where



--Deklaracje

-- |Math f(x) = acos(x)
acos' :: Double -> Double -> Maybe Double
-- |Math f(x) = acosh(x)
acosh' :: Double -> Double -> Maybe Double
-- |Math f(x) = acot(x)
acot :: Double -> Double -> Maybe Double
-- |Math f(x) = acoth(x)
acoth :: Double -> Double -> Maybe Double
-- |Math f(x) = acsc(x)
acsc :: Double -> Double -> Maybe Double
-- |Math f(x) = acsch(x)
acsch :: Double -> Double -> Maybe Double
-- |Math f(x) = asec(x)
asec :: Double -> Double -> Maybe Double
-- |Math f(x) = asech(x)
asech :: Double -> Double -> Maybe Double
-- |Math f(x) = asin(x)
asin' :: Double -> Double -> Maybe Double
-- |Math f(x) = asinh(x)
asinh' :: Double -> Double -> Maybe Double
-- |Math f(x) = atan(x)
atan' :: Double -> Double -> Maybe Double
-- |Math f(x) = atanh(x)
atanh' :: Double -> Double -> Maybe Double
-- |Math f(a,x) = a^x
aToX :: Double -> Double -> Maybe Double
-- |Math f(x) = cos(x)
cos' :: Double -> Double -> Maybe Double
-- |Math f(x) = cosh(x)
cosh' :: Double -> Double -> Maybe Double
-- |Math f(x) = cot(x)
cot :: Double -> Double -> Maybe Double
-- |Math f(x) = coth(x)
coth :: Double -> Double -> Maybe Double
-- |Math f(x) = csc(x)
csc :: Double -> Double -> Maybe Double
-- |Math f(x) = csch(x)
csch :: Double -> Double -> Maybe Double
-- |Math f(x) = exp(x)
exp' :: Double -> Double -> Maybe Double
-- |Math f(x) = loge(x)
log' :: Double -> Double -> Maybe Double
-- |Math f(a,x) = loga(x)
loga :: Double -> Double -> Maybe Double
-- |Math f(x) = sec(x)
sec :: Double -> Double -> Maybe Double
-- |Math f(x) = sech(x)
sech :: Double -> Double -> Maybe Double
-- |Math f(x) = sin(x)
sin' :: Double -> Double -> Maybe Double
-- |Math f(x) = sinh(x)
sinh' :: Double -> Double -> Maybe Double
-- |Math f(x) = tan(x)
tan' :: Double -> Double -> Maybe Double
-- |Math f(x) = tanh(x)
tanh' :: Double -> Double -> Maybe Double
-- |Math f(n,x) = x^n
xToN :: Double -> Double -> Maybe Double
-- |Math f(x) = x^x
xToX :: Double -> Double -> Maybe Double

-- |Math d(acos(x))/dx
dacos :: Double -> Double -> Maybe Double
-- |Math d(acosh(x))/dx
dacosh :: Double -> Double -> Maybe Double
-- |Math d(acot(x))/dx
dacot :: Double -> Double -> Maybe Double
-- |Math d(acoth(x))/dx
dacoth :: Double -> Double -> Maybe Double
-- |Math d(acsc(x))/dx
dacsc :: Double -> Double -> Maybe Double
-- |Math d(acsch(x))/dx
dacsch :: Double -> Double -> Maybe Double
-- |Math d(asec(x))/dx
dasec :: Double -> Double -> Maybe Double
-- |Math d(asech(x))/dx
dasech :: Double -> Double -> Maybe Double
-- |Math d(asin(x))/dx
dasin :: Double -> Double -> Maybe Double
-- |Math d(asinh(x))/dx
dasinh :: Double -> Double -> Maybe Double
-- |Math d(atan(x))/dx
datan :: Double -> Double -> Maybe Double
-- |Math d(atanh(x))/dx
datanh :: Double -> Double -> Maybe Double
-- |Math d(a^x(a,x))/dx
daToX :: Double -> Double -> Maybe Double
-- |Math dc/dx
dc :: Double -> Double -> Maybe Double
-- |Math d(cos(x))/dx
dcos :: Double -> Double -> Maybe Double
-- |Math d(cosh(x))/dx
dcosh :: Double -> Double -> Maybe Double
-- |Math d(cot(x))/dx
dcot :: Double -> Double -> Maybe Double
-- |Math d(coth(x))/dx
dcoth :: Double -> Double -> Maybe Double
-- |Math d(csc(x))/dx
dcsc :: Double -> Double -> Maybe Double
-- |Math d(csch(x))/dx
dcsch :: Double -> Double -> Maybe Double
-- |Math d(exp(x))/dx
dexp :: Double -> Double -> Maybe Double
-- |Math d(loge(x))/dx
dlog :: Double -> Double -> Maybe Double
-- |Math d(loga(a,x))/dx
dloga :: Double -> Double -> Maybe Double
-- |Math d(sec(x))/dx
dsec :: Double -> Double -> Maybe Double
-- |Math d(sech(x))/dx
dsech :: Double -> Double -> Maybe Double
-- |Math d(sin(x))/dx
dsin :: Double -> Double -> Maybe Double
-- |Math d(sinh(x))/dx
dsinh :: Double -> Double -> Maybe Double
-- |Math d(tan(x))/dx
dtan :: Double -> Double -> Maybe Double
-- |Math d(tanh(x))/dx
dtanh :: Double -> Double -> Maybe Double
-- |Math d(x^n(n,x))/dx
dxToN :: Double -> Double -> Maybe Double
-- |Math d(x^x(x))/dx
dxToX :: Double -> Double -> Maybe Double

-- |Function which open Maybe monad, when we time or divide
takeMaybeTD :: Maybe Double -> Double --Time/Divide
-- |Function which open Maybe monad, when we plus or minus
takeMaybePM :: Maybe Double -> Double --Plus/Minus



--Definicje

acos' _ x
  |abs (x) < 1 = Just (acos x) -- <=
  |otherwise = Nothing
acosh' _ x
  |abs(x) > 1 = Just (acosh x)
  |otherwise = Nothing
acot _ x
  |x /= 0 = Just (atan $ 1/x)
  |otherwise = Just (pi/2)
acoth _ x
  |abs(x) > 1 = Just (atanh $ 1/x)
  |otherwise = Nothing
acsc _ x
  |abs(x) > 1 = Just (asin $ 1/x)
  |otherwise = Nothing
acsch _ x
  |x /= 0 = Just (asinh $ 1/x)
  |otherwise = Nothing
asec _ x
  |abs(x) > 1 = Just (acos $ 1/x)
  |otherwise = Nothing
asech _ x
  |x /= 0 && abs(x) < 1 = Just (acosh $ 1/x)
  |otherwise = Nothing
asin' _ x
  |abs (x) < 1 = Just (asin x) -- <=
  |otherwise = Nothing
asinh' _ x = Just (asinh x)
atan' _ x = Just (atan x)
atanh' _ x
  |abs(x) < 1 = Just (atanh x)
  |otherwise = Nothing
aToX a x
  |a>0 && a/= 1 = Just (a ** x)
  |otherwise = Nothing
cos' _ x = Just (cos x)
cosh' _ x = Just (cosh x)
cot _ x
  |sin x /= 0 = Just ((cos x)/(sin x))
  |otherwise = Nothing
coth _ x
  |sinh x /= 0 = Just ((cosh x)/(sinh x))
  |otherwise = Nothing
csc _ x
  |sin x /= 0 = Just (1/(sin x))
  |otherwise = Nothing
csch _ x
  |sinh x /= 0 = Just (1/(sinh x))
  |otherwise = Nothing
exp' _ x = Just (exp x)
log' _ x
  |x>0 = Just (log x)
  |otherwise = Nothing
loga a x
  |a>0 && a/=1 && x>0 = Just (logBase a x)
  |otherwise = Nothing
sec _ x
  |cos x /= 0 = Just (1/(cos x))
  |otherwise = Nothing
sech _ x
  |cosh x /= 0 = Just (1/(cosh x))
  |otherwise = Nothing
sin' _ x = Just (sin x)
sinh' _ x = Just (sinh x)
tan' _ x
  |cos x /= 0 = Just (tan x)
  |otherwise = Nothing
tanh' _ x = Just (tanh x)
xToN n x = Just (x ** n)
xToX _ x
  |x > 0 = Just (x ** x)
  |otherwise = Nothing

dacos _ x
  |abs(x) < 1 = Just (-1/sqrt(1-x^2))
  |otherwise = Nothing
dacosh _ x
  |abs(x) > 1 = Just (1/sqrt(x^2-1))
  |otherwise = Nothing
dacot _ x = Just (-1/(1+x^2))
dacoth _ x
  |abs(x) > 1 = Just (-1/(x^2-1))
  |otherwise = Nothing
dacsc _ x
  |abs(x) > 1 = Just (-1/(x*sqrt(x^2-1)))
  |otherwise = Nothing
dacsch _ x
  |x /= 0 = Just (-1/(x*sqrt(1+x^2)))
  |otherwise = Nothing
dasec _ x
  |abs(x) > 1 = Just (1/(x*sqrt(x^2-1)))
  |otherwise = Nothing
dasech _ x
  |x /= 0 && abs(x)<1 = Just (-1/(x*sqrt(1-x^2)))
  |otherwise = Nothing
dasin _ x
  |abs(x) < 1 = Just (1/sqrt(1-x^2))
  |otherwise = Nothing
dasinh _ x = Just (1/sqrt(1+x^2))
datan _ x = Just (1/(1+x^2))
datanh _ x
  |abs(x) < 1 = Just (1/(1-x^2))
  |otherwise = Nothing
daToX a x
  |a>0 && a/= 1 = Just (a**x * (log a))
dc _ _ = Just 0
dcos _ x = Just (-(sin x))
dcosh _ x = Just (sinh x)
dcot _ x
  |sin x /= 0 = Just (-(takeMaybePM (csc 0 x))^2)
  |otherwise = Nothing
dcoth _ x
  |sinh x /= 0 = Just (-(takeMaybePM (csch 0 x))^2)
  |otherwise = Nothing
dcsc _ x
  |sin x /= 0 = Just (-(cos x)/(sin x)^2)
  |otherwise = Nothing
dcsch _ x
  |sinh x /= 0 = Just (-(takeMaybePM (csch 0 x))*(takeMaybePM (coth 0 x)))
  |otherwise = Nothing
dexp _ x = Just (exp x)
dlog _ x
  |x > 0 = Just (1/x)
  |otherwise = Nothing
dloga a x
  |a <= 0 || a == 1 || x <= 0 = Nothing
  |otherwise = Just (1/(x*(log a)))
dsec _ x
  |cos x /= 0 = Just ((sin x)/(cos x)^2)
  |otherwise = Nothing
dsech _ x
  |cosh x /= 0 = Just (-(takeMaybePM (sech 0 x))*(tanh x))
  |otherwise = Nothing
dsin _ x = Just (cos x)
dsinh _ x = Just (cosh x)
dtan _ x
  |cos x /= 0 = Just ((takeMaybePM (sec 0 x))^2)
  |otherwise = Nothing
dtanh _ x = Just ((takeMaybePM (sech 0 x))^2)
dxToN n x = Just (n* x**(n-1))
dxToX _ x
  |x > 0 = Just (x**x * (1+log x))
  |otherwise = Nothing

takeMaybeTD (Just x) = x
takeMaybeTD Nothing = 1

takeMaybePM (Just x) = x
takeMaybePM Nothing = 0
