{- TrivialCompression.hs
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

module Main where
   
-- The following two imports are needed to calculate the size of the data structures
import Control.DeepSeq
import GHC.DataSize (recursiveSize)

{- The main part of the example is moved into a separate module
   to make it more similar to the Java/Python/Rust examples / reusable -}
import CompressedGene (compress, decompress)

main :: IO ()
main = do
    let original = "TAGGGATTAACCGTTATATATATATAGCCATGGATCGATTATATAGGGATTAACCGTTATATATATATAGCCATGGATCGATTATA"
    let compressedGene = compress original
    -- !! forces evaluation of original before recursiveSize is called, else we'd only get the size of the first element
    originalSize <- recursiveSize $!! original 
    putStrLn ("Original is " ++ (show originalSize) ++ " bytes long")
    -- ! forces evaluation of compressedGene before recursiveSize is called
    compressedSize <- recursiveSize $! compressedGene 
    putStrLn ("Compressed is " ++ (show compressedSize) ++ " bytes long")
    putStrLn ("Original and decompressed are the same: " ++ show (original == decompress compressedGene))   