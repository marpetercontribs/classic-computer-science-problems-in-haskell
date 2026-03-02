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
    Constrainable(..),
    makeCSP,
    addToCspConstraint,
    backTrackingSearch)

import qualified Data.Map.Strict as Map

data Color = Red | Green | Blue deriving (Eq, Show)
data Region = WesternAustralia | NorthernTerritory | SouthAustralia |
    Queensland | NewSouthWales | Victoria | Tasmania deriving (Eq, Ord, Show)

data MapColoringConstraint = MapColoringConstraint {
      region1 :: Region
    , region2 :: Region } deriving (Eq, Show)

instance Constrainable MapColoringConstraint Region Color where
    satisfied constraint assignment = 
        case (Map.lookup (region1 constraint) assignment, Map.lookup (region2 constraint) assignment) of
            (Just color1, Just color2) -> color1 /= color2
            _ -> True -- if any the variables is not assigned yet, the constraint is satisfied
    variables constraint = [region1 constraint, region2 constraint]

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
            (MapColoringConstraint WesternAustralia NorthernTerritory),
            (MapColoringConstraint WesternAustralia SouthAustralia),
            (MapColoringConstraint NorthernTerritory SouthAustralia),
            (MapColoringConstraint NorthernTerritory Queensland),
            (MapColoringConstraint SouthAustralia Queensland),
            (MapColoringConstraint SouthAustralia NewSouthWales),
            (MapColoringConstraint SouthAustralia Victoria),
            (MapColoringConstraint Queensland NewSouthWales),
            (MapColoringConstraint NewSouthWales Victoria),
            (MapColoringConstraint Victoria Tasmania)]
    let solution = backTrackingSearch csp
    case solution of
        Just assignment -> print assignment
        Nothing -> putStrLn "No solution found"
