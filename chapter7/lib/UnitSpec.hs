{- UnitSpec.hs
   Some Unit Tests for the Haskell implementation of
   Classic Computer Science Problems in Python/Java Chapter 7
   Copyright 2026 Markus Peter

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
import Utils

testOrthogonalVectors = TestCase( do
    assertEqual "dot product of orthogonal vectors should be 0"
        0 (dotProduct [1, 0, 0] [0, 1, 0])
    )

testNonZeroDotProduct1 = TestCase( do
    assertEqual "dot product of [1, 1, 0] and [0, 1, 1] should be 1"
        1 (dotProduct [1, 1, 0] [0, 1, 1])
    )

testNonZeroDotProduct2 = TestCase( do
    assertEqual "dot product of [1, 2, 3] and [4, 5, 6] should be 32"
        32 (dotProduct [1, 2, 3] [4, 5, 6])
    )

testSplit = TestCase( do
    assertEqual "simpleSplit should split a string on the given delimiter"
        ["a", "b", "c"] (simpleSplit ',' "a,b,c")
    )

tests = TestList [ TestLabel "orthogonal vectors" testOrthogonalVectors
                 , TestLabel "nonZero dot product 1" testNonZeroDotProduct1
                 , TestLabel "nonZero dot product 2" testNonZeroDotProduct2
                 , TestLabel "simpleSplit" testSplit
                 , TestLabel "sigmoid of 0 should be 0.5"
                    (TestCase (assertEqual "sigmoid of 0 should be 0.5" 0.5 (sigmoid 0)))
                 , TestLabel "sigmoid derivative of 0 should be 0.25"
                    (TestCase (assertEqual "sigmoid derivative of 0 should be 0.25" 0.25 (sigmoidDerivative 0)))
                 ]

main :: IO Counts
main = do 
    runTestTT tests