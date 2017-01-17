{-module MainTest
(

)
where-}

import QuickCheck
import HUnit
import Test.QuickCheck
import Test.HUnit

main :: IO Counts
main = do
  putStrLn "QuickCheck Tests:\n"

  putStrLn "BasicDerivatives Tests:\n"
  putStrLn "sinTest:"
  quickCheck sinTest
  putStrLn "expTest:"
  quickCheck expTest

  putStrLn "dsinTest"
  quickCheck dsinTest
  putStrLn "dexpTest"
  quickCheck dexpTest

  putStrLn "takeMaybePMTest"
  quickCheck takeMaybePMTest
  putStrLn "takeMaybeTDTest"
  quickCheck takeMaybeTDTest

  putStrLn "HUnit Tests:\n"

  putStrLn "dTest1, dTest2, eTest1, eTest2,isNumberTest1, isNumberTest2"
  runTestTT $ TestList [TestLabel "dTest1" dTest1,TestLabel "dTest2" dTest2,TestLabel "eTest1" eTest1,TestLabel "eTest2" eTest2,TestLabel "isNumberTest1" isNumberTest1,
    TestLabel "isNumberTest2" isNumberTest2,TestLabel "signTest1" signTest1,TestLabel "signTest2" signTest2]
