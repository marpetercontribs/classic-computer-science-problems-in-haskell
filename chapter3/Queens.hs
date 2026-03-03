{- Queens.hs (N-Queens Constraint Satisfaction Problem Example)
   Adapted From Classic Computer Science Problems in Python/Java Chapter 3
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

{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import CSP ( 
    CSP(..),
    Constraint(..),
    makeCSP,
    addToCspConstraint,
    backTrackingSearch)

import qualified Data.Map.Strict as Map

data QueensConstraint = QueensConstraint {
      columns :: [Int] } deriving (Eq, Show)

instance Constraint QueensConstraint Int Int where
    satisfied constraint assignment = -- an assignment is a mapping from column to row
        all ((\(col1, row1, col2, row2) -> row1 /= row2 -- different rows
            && abs (col1 - col2) /= abs (row1 - row2))) -- not on the same diagonal
        [ (col1, row1, col2, assignment Map.! col2) | (col1,row1) <- Map.assocs assignment, col2 <- Map.keys assignment, col1 < col2 ]
    variables constraint = columns constraint

main :: IO ()
main = do
    let n = 8::Int
    let theDomains = Map.fromList [(column, [1..n]) | column <- [1..n]]
    let csp' = makeCSP theDomains
    let csp = addToCspConstraint csp' (QueensConstraint [1..n])
    case backTrackingSearch csp of
        Just solution -> print solution
        Nothing -> putStrLn "No solution found."