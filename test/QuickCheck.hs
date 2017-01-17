module QuickCheck
(

--BasicFunctions/Functions
expTest,
sinTest,

--BasicDerivatives/Derivatives
dexpTest,
dsinTest,

--BasicDerivatives/Others
takeMaybePMTest,
takeMaybeTDTest,

)
where

import BasicDerivatives
import Test.QuickCheck

sinTest :: Double -> Bool
sinTest x = sin' 0 x == Just (sin x)

expTest :: Double -> Bool
expTest x = exp' 0 x == Just (exp x)

dsinTest :: Double -> Bool
dsinTest x = dsin 0 x == Just (cos x)

dexpTest :: Double -> Bool
dexpTest x = dexp 0 x == Just (exp x)

takeMaybePMTest :: Maybe Double -> Bool
takeMaybePMTest Nothing = takeMaybePM Nothing == 0
takeMaybePMTest (Just x) = takeMaybePM (Just x) == x

takeMaybeTDTest :: Maybe Double -> Bool
takeMaybeTDTest Nothing = takeMaybeTD Nothing == 1
takeMaybeTDTest (Just x) = takeMaybeTD (Just x) == x
