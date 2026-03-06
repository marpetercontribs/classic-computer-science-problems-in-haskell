{- SendMoreMoney.hs (Send More Money Constraint Satisfaction Problem Example)
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

import CSP ( 
    CSP(..),
    Constraint(..),
    makeCSP,
    addToCspConstraint,
    backTrackingSearch)

import qualified Data.Map.Strict as Map
import Data.List (nub)

data SendMoreMoneyConstraint = SendMoreMoneyConstraint { letters :: [Char] }  deriving (Eq, Show)
instance Constraint SendMoreMoneyConstraint Char Int where
    variables constraint = letters constraint
    satisfied constraint assignment
        | length (nub (Map.elems assignment)) < Map.size assignment = False -- if there are duplicate values, the constraint is not satisfied
        | Map.size assignment == length (letters constraint) =
            let s = assignment Map.! 'S'
                e = assignment Map.! 'E'
                n = assignment Map.! 'N'
                d = assignment Map.! 'D'
                m = assignment Map.! 'M'
                o = assignment Map.! 'O'
                r = assignment Map.! 'R'
                y = assignment Map.! 'Y'
            in 1000 * s + 100 * e + 10 * n + d + 1000 * m + 100 * o + 10 * r + e
                == 10000 * m + 1000 * o + 100 * n + 10 * e + y
        | otherwise = True -- if not all variables are assigned, we can't check the sum constraint yet, but we can still check for duplicates   

main :: IO ()
main = do
    let letters = "SENDMORY"
    let domains' = Map.fromList [(letter, [0..9]) | letter <- letters]
    let domains = Map.insert 'M' [1] domains' -- M must be one to prevent answers starting with digit 0
    let csp = addToCspConstraint (makeCSP domains) (SendMoreMoneyConstraint letters)
    let solution = backTrackingSearch csp
    case solution of
        Just assignment -> print assignment
        Nothing -> putStrLn "No solution found"