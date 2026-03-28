{- ListCompression.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 5
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

{-# LANGUAGE FlexibleInstances #-} -- so that `instance Chromosome [String]` can be used

module Main (main) where

import GeneticAlgorithm
import System.Random(randomR, getStdGen)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Codec.Compression.GZip(compress)
import qualified Data.ByteString.Lazy as BS
import Data.String.Conversions(cs)

bytesCompressed :: [String] -> Int
bytesCompressed ss = (fromIntegral . BS.length . compress . cs . concat ) ss

-- Helper function to swap elements in a list
swap :: [a] -> Int -> Int -> [a]
swap ls i j = zipWith (\k c ->  if k == i then ls !! j else if k == j then ls !! i else c) [0..] ls

instance Chromosome [String] where
    fitness ss = 1 / (fromIntegral (bytesCompressed ss))
    crossover rg this that = 
        let (idx1, rg') = randomR (0,(length this)-1) rg
            (idx2, rg'') = randomR (0,(length that)-1) rg'
        in ( swap this idx1 (fromJust (findIndex ((==) (that !! idx2)) this))
           , swap that idx2 (fromJust (findIndex ((==) (this !! idx2)) that))
           , rg'')
    randomInstance rg = shuffle (
        ["Michael", "Sarah", "Joshua", "Narine", "David", "Sajid", "Melanie",
         "Daniel", "Wei", "Dean", "Brian", "Murat", "Lisa"]
         ,rg)
    mutate list rg =
        let (idx1, rg') = randomR (0,(length list)-1) rg
            (idx2, rg'') = randomR (0,(length list)-1) rg'
        in (swap list idx1 idx2, rg'')

showWithBytesCompressed :: [String] -> String
showWithBytesCompressed ss = "Order: " ++ (show ss) ++ "\nBytes: " ++ (show (bytesCompressed ss))

main :: IO ()
main = do
    rg <- getStdGen
    let (initialPopulation, rg') = foldl 
             (\(lists, rg') _ -> let (list,rg'') = randomInstance rg' in (list:lists,rg'')) 
             ([],rg) [1..100]
    let ga = newGeneticAlgorithm initialPopulation 0.2 0.7 Tournament
    putStrLn $ showWithBytesCompressed (runGeneticAlgorithm ga rg' 100 1.0)