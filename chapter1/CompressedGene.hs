{- CompressedGene.hs - used by TrivialCompression.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 1
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

module CompressedGene
    ( CompressedGene
    , compress
    , decompress
    ) where

{- to be able to throw an exception, we need to import Control.Exception
   and define a custom exception type. We can then throw an exception
   by calling throw with an instance of our custom exception type.
-}
import GHC.Bits
import Data.Char (toUpper)
import Control.Exception
data InvalidNucleotide = InvalidNucleotide Char
instance Show InvalidNucleotide where
    show (InvalidNucleotide nucleotide) = "Invalid nucleotide: " ++ [nucleotide]
instance Exception InvalidNucleotide

type CompressedGene = Integer
compress :: String -> CompressedGene
compress gene = foldl appendNucleotide 1 (map toUpper gene) where
    appendNucleotide compressedGene nucleotide
        = (shiftL compressedGene 2) .|. (nucleotideToBits nucleotide) -- .|. is bitwise OR
    nucleotideToBits nucleotide 
        | nucleotide == 'A' = 0 -- right-most two bits are 00
        | nucleotide == 'C' = 1 -- right-most two bits are 01
        | nucleotide == 'G' = 2 -- right-most two bits are 10
        | nucleotide == 'T' = 3 -- right-most two bits are 11
        | otherwise = throw (InvalidNucleotide  nucleotide)

decompress :: CompressedGene -> String
decompress compressedGene = decompressGene compressedGene [] where
    decompressGene compressedGene decompressedGene
        | compressedGene == 1 = decompressedGene -- note that compression starts with sentinel 1
        | otherwise = decompressGene (shiftR compressedGene 2) (nucleotideFromBits (compressedGene .&. 3): decompressedGene)
    nucleotideFromBits nucleotide
        | nucleotide == 0 = 'A'
        | nucleotide == 1 = 'C'
        | nucleotide == 2 = 'G'
        | nucleotide == 3 = 'T'
        | otherwise = throw (InvalidNucleotide 'X')