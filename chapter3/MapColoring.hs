{- MapColoring.hs (Map Coloring Constraint Satisfaction Problem Example)
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
    Satisfiable(..),
    makeCSP,
    makeConstraint,
    addToCspConstraint,
    backTrackingSearch)

import qualified Data.Map.Strict as Map

data Color = Red | Green | Blue deriving (Eq, Show)
data Region = WesternAustralia | NorthernTerritory | SouthAustralia |
    Queensland | NewSouthWales | Victoria | Tasmania deriving (Eq, Ord, Show)

instance Satisfiable Region Color where
    isSatisfied constraint assignment = 
        let [var1, var2] = variables constraint in
        case (Map.lookup var1 assignment, Map.lookup var2 assignment) of
            (Just color1, Just color2) -> color1 /= color2
            _ -> True -- if any the variables is not assigned yet, the constraint is satisfied

main :: IO ()
main = do
    let colors = [Red, Green, Blue]
    let theDomains = Map.fromList [
            (WesternAustralia, colors),
            (NorthernTerritory, colors),
            (SouthAustralia, colors),
            (Queensland, colors),
            (NewSouthWales, colors),
            (Victoria, colors),
            (Tasmania, colors)]

    let csp' = makeCSP theDomains
    let csp = foldl addToCspConstraint csp' [
            (makeConstraint [WesternAustralia, NorthernTerritory] :: Constraint Region Color),
            (makeConstraint [WesternAustralia, SouthAustralia] :: Constraint Region Color),
            (makeConstraint [NorthernTerritory, SouthAustralia] :: Constraint Region Color),
            (makeConstraint [NorthernTerritory, Queensland] :: Constraint Region Color),
            (makeConstraint [SouthAustralia, Queensland] :: Constraint Region Color),
            (makeConstraint [SouthAustralia, NewSouthWales] :: Constraint Region Color),
            (makeConstraint [SouthAustralia, Victoria] :: Constraint Region Color),
            (makeConstraint [Queensland, NewSouthWales] :: Constraint Region Color),
            (makeConstraint [NewSouthWales, Victoria] :: Constraint Region Color),
            (makeConstraint [Victoria, Tasmania] :: Constraint Region Color)]
    let solution = backTrackingSearch csp
    case solution of
        Just assignment -> print assignment
        Nothing -> putStrLn "No solution found"
