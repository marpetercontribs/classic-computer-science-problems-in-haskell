{- TTTUnitSpec.hs
   Some Unit Tests for the Haskell implementation of TicTacToe of
   Classic Computer Science Problems in Python/Java Chapter 8
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
import TicTacToe
import MiniMax

testEasyPosition = TestCase( do
    let board = TTTBoard { board = [X, O, X, X, E, O, E, E, O], turn = X }
    assertEqual "minimax should find the winning move for X" 6 (findBestMove board 8)
    )

testBlockPosition = TestCase( do
    let board = TTTBoard { board = [X, E, E, E, E, O, E, X, O], turn = X }
    assertEqual "minimax should find the blocking move for X" 2 (findBestMove board 8)
    )

testHardPosition = TestCase( do
    let board = TTTBoard { board = [X, E, E, E, E, O, O, X, E], turn = X }
    assertEqual "minimax should find the winning move for X" 4 (findBestMove board 8)
    )

tests = TestList [
    testEasyPosition,
    testBlockPosition,
    testHardPosition
    ]

main :: IO Counts
main = do 
    runTestTT tests