{- UnitSpec.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 2
   Copyright 2025 Markus Peter

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
  
   http://www.apache.org/licenses/LICENSE-2.0
  
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

import Test.HUnit
import GenericSearch

testList = [1, 5, 15, 15, 15, 15, 20]
testString = "adefz"
testWords = ["john", "mark", "ronald", "sarah"]

testLinearContains = TestCase( do
    assertEqual ("linearContains should return True: " ++ show testList ++ " contains 15")
        (linearContains testList 15) True
    assertEqual ("linearContains should return False: " ++ show testList ++ " contains no 6")
        (linearContains testList 6) False
    )

testBinaryContains = TestCase( do
    assertEqual ("binaryContains should return True: " ++ show testList ++ " contains 15")
        (binaryContains testList 15) True
    assertEqual ("binaryContains should return False: " ++ show testList ++ " contains no 6")
        (binaryContains testList 6) False
    assertEqual ("binaryContains should return True: " ++ show testString ++ " contains f")
        (binaryContains testString 'f') True
    assertEqual ("binaryContains should return False: " ++ show testWords ++ " contains no 'sheila'")
        (binaryContains testWords "sheila") False
    )

tests = TestList [ TestLabel "testLinearContains" testLinearContains
                 , TestLabel "testBinaryContains" testBinaryContains
                 ]

main :: IO Counts
main = do 
    runTestTT tests
