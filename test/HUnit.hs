module HUnit
(

dTest1,
dTest2,
eTest1,
eTest2,
isNumberTest1,
isNumberTest2,
signTest1,
signTest2

)
where

import ParsingFunctions
import Test.HUnit

dTest1 :: Test
dTest1 = TestCase $ assertEqual "dTest" (d "sin" 0 0) (Just (cos 0))

dTest2 :: Test
dTest2 = TestCase $ assertEqual "dTest" (d "sin" 0 1) (Just (cos 1))

eTest1 :: Test
eTest1 = TestCase $ assertEqual "eTest" (e "exp" 0 0) (Just (exp 0))

eTest2 :: Test
eTest2 = TestCase $ assertEqual "eTest" (e "exp" 0 1) (Just (exp 1))

isNumberTest1 :: Test
isNumberTest1 = TestCase $ assertEqual "isNumberTest" (isNumber "10") True

isNumberTest2 :: Test
isNumberTest2 = TestCase $ assertEqual "isNumberTest" (isNumber "1a") False

signTest1 :: Test
signTest1 = TestCase $ assertEqual "signTest" ((sign "+") 1 2) 3

signTest2 :: Test
signTest2 = TestCase $ assertEqual "signTest" ((sign "*") 3 4) 12
