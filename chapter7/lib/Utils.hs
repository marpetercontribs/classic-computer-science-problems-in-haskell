{- Utils.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 7
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

module Utils(
      dotProduct
    , sigmoid
    , sigmoidDerivative
    , simpleSplit
    , normalizeByFeatureScaling
    , csvStringTo
    , shuffle
) where


import Data.List(transpose)
import System.Random(RandomGen, randomR)

dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys = sum $ zipWith (*) xs ys

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x)) 

sigmoidDerivative :: Double -> Double
sigmoidDerivative x = sigmoid x * (1 - sigmoid x) where sx = sigmoid x

simpleSplit :: Char -> String -> [String]
simpleSplit delim "" = [""]
simpleSplit delim (c:cs)
   | c == delim = "" : rest
   | otherwise = (c:rh) : rt
   where rest@(rh:rt) = simpleSplit delim cs

csvStringTo :: String -> [[String]]
csvStringTo s = map (simpleSplit ',') (lines s)

normalizeByFeatureScaling :: [[Double]] -> [[Double]]
normalizeByFeatureScaling dataset = map
  (\row -> map (\(x, min, diff) -> (x - min) / diff) (zip3 row mins diffs))
  dataset
  where
    cols = transpose dataset
    mins = map minimum cols
    diffs = zipWith (-) (map maximum cols) mins

-- adapted from https://www.literateprograms.org/fisher-yates_shuffle__haskell_.html
-- probably not very efficient
shuffle :: RandomGen g => ([a],g) -> ([a], g)
shuffle (l, rg) = shuffle' (l,rg) []
  where
    shuffle' ([],rg') acc =  (acc,rg')
    shuffle' (l',rg') acc =
        let (k,rg'') = randomR (0, length l' - 1) rg'
            (lead, x:xs) = splitAt k l'           
        in  shuffle' (lead ++ xs, rg'') (x:acc)