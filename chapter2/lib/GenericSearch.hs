{- GenericSearch.hs
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

module GenericSearch(
    linearContains
  , binaryContains
) where

linearContains :: Eq a => [a] -> a -> Bool
linearContains [] _ = False
linearContains (x:xs) key
    | x == key   = True
    | otherwise  = linearContains xs key
-- Alternative: use the built-in any function
-- linearContains xs key = any (== key) xs

binaryContains :: Ord a => [a] -> a -> Bool
binaryContains xs key = binaryContains' xs key 0 (length xs - 1) where
    binaryContains' xs key low high
        | low > high = False
        | midKey < key = binaryContains' xs key (mid + 1) high
        | midKey > key = binaryContains' xs key low (mid - 1)
        | otherwise = True
        where mid = (low + high) `div` 2
              midKey = xs !! mid