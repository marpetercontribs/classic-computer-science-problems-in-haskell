{- SendMoreMoney.hs
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

module Main (main) where

import GeneticAlgorithm
import System.Random(randomR, getStdGen)
import Data.List (findIndex)
import Data.Maybe (fromJust)

data SendMoreMoney = SendMoreMoney { letters:: [Char] }
indexOf :: SendMoreMoney -> Char -> Int
indexOf smm c = fromJust $ findIndex ((==) c) (letters smm)

send :: SendMoreMoney -> Int
send smm = let indexOf' = indexOf smm in
    1000 * (indexOf' 'S') + 100 * (indexOf' 'E') + 10 * (indexOf' 'N') + (indexOf' 'D')

more :: SendMoreMoney -> Int
more smm = let indexOf' = indexOf smm in
    1000 * (indexOf' 'M') + 100 * (indexOf' 'O') + 10 * (indexOf' 'R') + (indexOf' 'E')

money :: SendMoreMoney -> Int
money smm = let indexOf' = indexOf smm in
    10000 * (indexOf' 'M') + 1000 * (indexOf' 'O') + 10 * (indexOf' 'N') + 10 * (indexOf' 'E') + (indexOf' 'Y')

-- Helper function to swap characters in a String
swap :: [Char] -> Int -> Int -> [Char]
swap cs i j = zipWith (\k c ->  if k == i then cs !! j else if k == j then cs !! i else c) [0..] cs

instance Chromosome SendMoreMoney where
    fitness smm = 1 / ( 1 + (fromIntegral . abs) (money smm - send smm - more smm))
    crossover rg this@(SendMoreMoney { letters = thisLs }) that@(SendMoreMoney { letters = thatLs }) =
        let (idx1, rg') = randomR (0,(length thisLs)-1) rg
            (idx2, rg'') = randomR (0,(length thatLs)-1) rg'
        in ( SendMoreMoney { letters = swap thisLs idx1 (indexOf this (thatLs !! idx2)) },
           ( SendMoreMoney { letters = swap thatLs idx2 (indexOf that (thisLs !! idx1)) }), rg')
    randomInstance rg = let (ls,rg') = shuffle ("SENDMORY  ",rg) in (SendMoreMoney { letters = ls }, rg')
    mutate SendMoreMoney { letters=ls } rg =
        let (idx1, rg') = randomR (0,(length ls)-1) rg
            (idx2, rg'') = randomR (0,(length ls)-1) rg'        
        in (SendMoreMoney { letters = swap ls idx1 idx2 }, rg'')

instance Show SendMoreMoney where
    show smm = (show (send smm)) ++ " + " ++ (show (more smm)) ++ " = " ++ (show (money smm))

main :: IO ()
main = do
    rg <- getStdGen
    let (initialPopulation, rg') = foldl 
            (\(smms, rg') _ -> let (smm,rg'') = randomInstance rg' in (smm:smms,rg'')) 
            ([],rg) [1..20]
    putStrLn $ show (initialPopulation :: [SendMoreMoney])
    let ga = newGeneticAlgorithm initialPopulation 0.2 0.7 Roulette
    putStrLn $ show (runGeneticAlgorithm ga rg' 1000 1.0)