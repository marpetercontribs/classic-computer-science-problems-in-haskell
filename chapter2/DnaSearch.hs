{- DnaSearchi.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 2
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

module Main where

import Data.Char (toUpper)
import Data.List (sort)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show) -- Ord is needed for binary search
type Codon = (Nucleotide, Nucleotide, Nucleotide)
type Gene = [Codon]

-- helper function that splits a list into a (list of) chunks of size n
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = chunksOf' n (xs,[]) where
    chunksOf' n (xs,chunks)
        | null xs       = reverse chunks -- because the below two cases produce
                                         -- the chunks in reverse order to avoid
                                         -- (++) leading to quadratic time 
        | length xs < n = xs:chunks
        | otherwise     = chunksOf' n (xs', chunk:chunks)
            where (chunk, xs') = splitAt n xs

stringToGene :: String -> Gene
stringToGene s = map stringToCodon (chunksOf 3 (map toUpper s)) where
    stringToCodon :: String -> Codon
    stringToCodon [a,b,c] = (charToNucleotide a, charToNucleotide b, charToNucleotide c) where
        charToNucleotide 'A' = A
        charToNucleotide 'C' = C
        charToNucleotide 'G' = G
        charToNucleotide 'T' = T
        charToNucleotide _   = error "Invalid Nucleotide"

linearContains :: Gene -> Codon -> Bool
linearContains [] _ = False
linearContains (x:xs) codon
    | x == codon = True
    | otherwise  = linearContains xs codon
-- Alternative: use the built-in any function
-- linearContains gene codon = any (== codon) gene

binaryContains :: Gene -> Codon -> Bool
binaryContains gene codon = binaryContains' gene codon 0 (length gene - 1) where
    binaryContains' gene codon low high
        | low > high = False
        | midCodon < codon = binaryContains' gene codon (mid + 1) high
        | midCodon > codon = binaryContains' gene codon low (mid - 1)
        | otherwise = True
        where mid = (low + high) `div` 2
              midCodon = gene !! mid

main :: IO ()
main = do
    let geneString = "ACGTGGCTCTCTAACGTACGTACGTACGGGGTTTATATATACCCTAGGACTCCCTTT"
    let gene = stringToGene geneString
    let acg = (A, C, G) -- :: Codon
    let gat = (G, A, T) -- :: Codon
    putStrLn "Using `linearContains`:"
    putStrLn ("The sample gene contains ACG: " ++ show (linearContains gene acg))
    putStrLn ("The sample gene contains GAT: " ++ show (linearContains gene gat))
    putStrLn "Using `binaryContains` on the `sorted` gene:"
    let sortedGene = sort gene
    putStrLn ("The sample gene contains ACG: " ++ show (linearContains sortedGene acg))
    putStrLn ("The sample gene contains GAT: " ++ show (linearContains sortedGene gat))
