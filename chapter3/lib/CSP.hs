{- CSP.hs (Constraint Satisfaction Problem)
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
{-# LANGUAGE AllowAmbiguousTypes #-}

module CSP( 
    CSP(..)
  , Constraint(..)
  , Satisfiable(..)
  , makeCSP
  , makeConstraint
  , addToCspConstraint
  , backTrackingSearch
) where

import Data.List (find)
import qualified Data.Map.Strict as Map

{- v is the type of the variable
   d is the type of the domain
-}

data (Ord v) => Constraint v = Constraint {
      variables :: [v] }

{- isSatisfied checks for a constraint and
   an assignment = a map of variables each to a value within the corresponding domain,
   if the assignment satisfies the constraint -}
class (Ord v) => Satisfiable v d where
    isSatisfied :: Constraint v -> Map.Map v d -> Bool
{-     cvariables :: Constraint v -> [v] -- TODO: can explit need for algebraic data type be removed? -}

makeConstraint :: (Ord v) => [v] -> Constraint v
makeConstraint variables = Constraint {
      variables = variables }

data (Ord v, Satisfiable v d) => CSP v d = CSP {
      domains :: Map.Map v [d]
    , constraints :: Map.Map v [Constraint v] }

{- makeCSP creates a new constraint satisfaction problem using
   a mapping of variables to their domains.
   Note that each variable's domain is a list of possible values -}
makeCSP :: (Ord v, Satisfiable v d) => Map.Map v [d] -> CSP v d
makeCSP domains = CSP {
      domains = domains
    , constraints = Map.fromList [(v, []) | v <- Map.keys domains] }


{- addToCspConstraint "adds" a constraint to a CSP -}
addToCspConstraint :: (Ord v, Satisfiable v d) => CSP v d -> Constraint v -> CSP v d
addToCspConstraint csp constraint = foldl' addConstraintToVariable csp (variables constraint)
   where
{-       addConstraintToVariables csp [] _ = csp
      addConstraintToVariables csp (v:vs) constraint =
         addConstraintToVariables (addConstraintToVariable csp constraint v) vs constraint -}
      addConstraintToVariable csp variable =
         if all (`elem` Map.keys (domains csp)) (variables constraint)
         then csp { constraints = Map.insertWith (++) variable [constraint] (constraints csp) }
         else error "Variable in constraint not in CSP"

{-isConsistent checks, for a given CSP and one of its variables, if the given
   assignment = a map of variables each to a value within the corresponding domain,
   is consistent with the CSP -}
isConsistent :: (Ord v, Satisfiable v d) => CSP v d -> v -> Map.Map v d -> Bool
isConsistent csp variable assignment = 
   all (\c -> isSatisfied c assignment)
      ((constraints csp) Map.! variable) -- gets the list of constraints for the variable

{- backtracking_search performs a backtracking search for a given CSP
   to find a solution = assignment for each variable of the CSP to a value within the corresponding domain
   that satisfies all constraints of the given CSP. -}
backTrackingSearch :: (Ord v, Satisfiable v d) => CSP v d -> Maybe (Map.Map v d)
backTrackingSearch csp = internalBackTrackingSearch csp Map.empty

internalBackTrackingSearch :: (Ord v, Satisfiable v d) => CSP v d -> Map.Map v d -> Maybe (Map.Map v d)
internalBackTrackingSearch csp assignment
   | -- assignment is complete if every variable is assigned (our base case)
     length assignment == Map.size (domains csp) = Just assignment
   | otherwise = do
      -- get first unassigned variable
      let firstUnassignedVariable = find (\v -> not (Map.member v assignment)) (Map.keys (domains csp))
      case firstUnassignedVariable of
         Just v -> let domain = (domains csp) Map.! v -- get the unassigned variable's domain
                   -- try to assign each value to the variable and recursively call backtracking search
                   in tryValues csp v domain assignment
         Nothing -> Nothing
   where
      tryValues _ _ [] _  = Nothing 
      tryValues csp variable (value:values) assignment =
         let newAssignment = Map.insert variable value assignment in
         if isConsistent csp variable newAssignment
         then case internalBackTrackingSearch csp newAssignment of
            Just result -> Just result
            Nothing -> tryValues csp variable values assignment
         else tryValues csp variable values assignment
