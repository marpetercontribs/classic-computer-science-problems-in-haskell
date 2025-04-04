{- CSP.hs (Constraint Satisfaction Problem)
   Adapted From Classic Computer Science Problems in Python/Java Chapter 3
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

{-# LANGUAGE MultiParamTypeClasses #-}

module CSP( 
    CSP(..)
  , Constraint(..)
) where

{- v is the type of the variable
   d is the type of the domain
   c is the type of the constraint
   csp is the type of the constraint satisfaction problem
-}

class Constraint c where
  newConstraint :: [v] -> c
  satisfied :: Constraint c => c -> [(v, d)] -> Bool
  variables :: Constraint c => c -> [v]

class CSP csp where
    newCSP :: [v] -> [(v, [d])] -> csp
    add_constraint :: (CSP csp, Constraint c) => csp -> c -> csp
    is_consistent :: CSP csp => csp -> v -> [(v, d)] -> Bool
    backtracking_search :: CSP csp => csp -> Maybe [(v, d)]
