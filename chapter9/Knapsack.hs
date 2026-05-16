{- Knapsack.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 9
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

module Main where

import Debug.Trace (trace)

data Item = Item {
      name :: String
    , weight :: Int
    , value :: Double
    }
instance Show Item where
    show item = name item
             ++ " (weight: " ++ show (weight item)
             ++ ", value: " ++ show (value item) ++ ")"

knapsack :: [Item] -> Int -> [Item]
knapsack items capacity = fst $ foldl
    (\(solution,cap) (row, nextRow, item) -> if row!!cap /= nextRow!!cap
                then (item:solution,cap - weight item)
                else (solution,cap))
    ([],capacity) -- initial solution is empty and rucksack at full capacity
    (zip3 table (drop 1 table) (reverse items))
    where -- this constructs the table row by row to avoid having to create
          -- the initial table with all 0s and then update it,
          -- starting with the initial row for "no items"
          -- note that the table is built in reverse row order compared to Kopec's book
          table = foldl
                (\rows@(lastRow:_) item ->
                    -- constructs new row
                    (reverse $ foldl (\row cap -> 
                         if cap >= weight item
                         then max (lastRow!!cap)
                                  (value item + lastRow!!(cap - weight item))
                              :row
                         else lastRow!!cap:row)
                         [0.0] [1..capacity])
                    :rows)
                [replicate (capacity + 1) 0.0] -- initial table with one row for "no items"
                items

main :: IO ()
main = do
    let items = [ Item "Television" 50 500,
                  Item "Candlesticks" 2 300,
                  Item "Stereo" 35 400,
                  Item "Laptop" 3 1000,
                  Item "Food" 15 50,
                  Item "Clothing" 20 800,
                  Item "Jewelry" 1 4000,
                  Item "Books" 100 300,
                  Item "Printer" 18 30,
                  Item "Refrigerator" 200 700,
                  Item "Painting" 10 1000]
    mapM_ (putStrLn . show)  (knapsack items 75)