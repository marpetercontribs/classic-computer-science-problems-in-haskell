{- WordSearch.hs (Word Search Constraint Satisfaction Problem Example)
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
{-# LANGUAGE FlexibleInstances #-}

module Main where

import CSP ( 
    CSP(..),
    Constraint(..),
    makeCSP,
    addToCspConstraint,
    backTrackingSearch)

import qualified Data.Map.Strict as Map
import Data.Containers.ListUtils (nubOrd) -- supposedly faster than Data.List nub, removes duplicates while preserving order
import System.Random (RandomGen, uniformListR, getStdGen)

type Grid = [[Char]]

data GridLocation = GridLocation {
    row :: Int,
    col :: Int
} deriving (Eq, Ord, Show)

data WordSearchConstraint = WordSearchConstraint {
      puzzleWords :: [[Char]] } deriving (Eq, Show)

instance Constraint WordSearchConstraint [Char] [GridLocation] where
    variables constraint = puzzleWords constraint
    satisfied constraint assignment = length allLocations == length deduplicatedLocations
        where
            allLocations = foldr (++) [] (Map.elems assignment)
            deduplicatedLocations = nubOrd allLocations

generateDomain :: [Char] -> Grid -> [[GridLocation]]
generateDomain word [] = []
generateDomain word (headLine:lines) = 
    leftToRight ++ topLeftToBottomRight ++ topToBottom ++ topRightToBottomLeft where
    lengthOfWord = length word - 1 -- note that Haskell ranges [0..n] include n, so subtract 1 from all lengths
    width = length headLine -1
    height = length lines -- already subtracted 1 because of the headLine
    leftToRight = [ [GridLocation row (col+letter) | letter <- [0..lengthOfWord] ] 
        | row <- [0..height], col <- [0.. width-lengthOfWord]]
    topLeftToBottomRight = [ [GridLocation (row+letter) (col+letter) | letter <- [0..lengthOfWord] ] 
        | row <- [0.. height-lengthOfWord], col <- [0.. width-lengthOfWord]]
    topToBottom = [ [GridLocation (row+letter) col | letter <- [0..lengthOfWord] ] | 
        row <- [0..height - lengthOfWord], col <- [0..width]]
    topRightToBottomLeft = [ [GridLocation (row+letter) (col-letter) | letter <- [0..lengthOfWord] ] 
        | row <- [0.. height-lengthOfWord], col <- [lengthOfWord..width]]

generateGrid :: RandomGen g => g -> Int -> Int -> Grid
generateGrid rg rows cols = chunksOf cols (fst (randomChars rg (rows * cols))) where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
    randomChars :: RandomGen g => g -> Int -> ( [Char], g) -- pass new generator back to avoid repeating the same random sequence
    randomChars gen length = uniformListR length ('A', 'Z') gen

displayGrid :: Grid -> IO ()
displayGrid grid = mapM_ putStrLn grid

applySolutionToGrid :: Map.Map [Char] [GridLocation] -> Grid -> Grid
applySolutionToGrid solution grid = foldr applyWordToGrid grid (Map.assocs solution) where
    applyWordToGrid (word, locations) grid = foldr (\((GridLocation row col), letter) g -> replaceCharInGrid g row col letter) grid (zip locations word) where
        replaceCharInGrid grid row col char = take row grid ++ [take col (grid !! row) ++ [char] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid

main :: IO ()
main = do
    rg <- getStdGen
    let grid = generateGrid rg 10 10
    let words = ["MATHEW", "JOE", "MARY", "SARAH", "SALLY"]
    let locations = Map.fromList [ (word, generateDomain word grid) | word <- words ]
    let csp' = makeCSP locations
    let csp = addToCspConstraint csp' (WordSearchConstraint words) -- note that we only need one constraint for the whole puzzle, since the constraint checks for all words at once
    case backTrackingSearch csp of
        Just solution -> displayGrid (applySolutionToGrid solution grid)
        Nothing -> putStrLn "No solution found."