{- Dijkstra.hs (Dijkstra algorthim to find shortest paths)
   Adapted From Classic Computer Science Problems in Python/Java Chapter 4
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

import Graph (
      Graph(..)
    , WeightedGraph(..)
    , add_edge_by_vertices'
    )

main :: IO ()
main = do
    let cityGraph = makeGraph ["Seattle", "San Francisco", "Los Angeles",
            "Riverside", "Phoenix", "Chicago", "Boston", "New York", "Atlanta",
            "Miami", "Dallas", "Houston", "Detroit", "Philadelphia", "Washington"] :: WeightedGraph String
    let cityGraphWithEdges = foldr add_edge_by_vertices' cityGraph [
            ("Seattle", "Chicago", 1737), 
            ("Seattle", "San Francisco", 678),
            ("San Francisco", "Riverside", 386),
            ("San Francisco", "Los Angeles", 348),
            ("Los Angeles", "Riverside", 50),
            ("Los Angeles", "Phoenix", 357),
            ("Riverside", "Phoenix", 307),
            ("Riverside", "Chicago", 1704),
            ("Phoenix", "Dallas", 887),
            ("Phoenix", "Houston", 1015),
            ("Dallas", "Chicago", 805),
            ("Dallas", "Atlanta", 721),
            ("Dallas", "Houston", 225),
            ("Houston", "Atlanta", 702),
            ("Houston", "Miami", 968),
            ("Atlanta", "Chicago", 588),
            ("Atlanta", "Washington", 543),
            ("Atlanta", "Miami", 604),
            ("Miami", "Washington", 923),
            ("Chicago", "Detroit", 238),
            ("Detroit", "Boston", 613),
            ("Detroit", "Washington", 396),
            ("Detroit", "New York", 482),
            ("Boston", "New York", 190),
            ("New York", "Philadelphia", 81),
            ("Philadelphia", "Washington", 123)]
    putStrLn $  "Weighted city graph with edges added:" ++ show cityGraphWithEdges