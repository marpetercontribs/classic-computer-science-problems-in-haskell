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
    , Edge(..)
    , WeightedEdge(..)
    , SimpleWeightedEdge(..)
    , WeightedGraph(..)
    , add_edge_by_vertices'
    , pathToString)
import PriorityQueue (
      PriorityQueue(..)
    , queuePush
    , queuePop
    , queueSize)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Maybe

data DijkstraNode = DijkstraNode { vertex :: Int, distance :: Float } deriving (Eq)
instance Ord DijkstraNode where
    compare this that = compare (distance this) (distance that)

dijkstra :: (Eq v, Show v) => WeightedGraph v -> v ->
    (Seq.Seq (Maybe Float), Map.Map Int SimpleWeightedEdge)
dijkstra graph start =
    let first = index_of_vertex graph start
    in loop
        -- initial list of distances
        (Seq.update first (Just 0) (Seq.replicate (get_vertex_count graph) Nothing))
        -- initial priority queue
        (PriorityQueue [DijkstraNode {vertex = first, distance = 0}])
        -- initial pathMap
        Map.empty
    where
        loop :: Seq.Seq (Maybe Float) -> PriorityQueue DijkstraNode -> Map.Map Int SimpleWeightedEdge -> (Seq.Seq (Maybe Float), Map.Map Int SimpleWeightedEdge)
        loop distances pq path
            | queueSize pq == 0 = (distances, path)
            | otherwise = loop newDistances newPq newPath where
                (node, restPq) = queuePop pq
                u = vertex node
                -- note that Seq.lookup returns a Maybe, and that the elements of the Sequence are also Maybes
                distance_u = fromJust $ fromJust (Seq.lookup u distances) -- this distance should have been set
                updateElements (distances', pq', path') e =
                    (Seq.update (to (edge e)) (Just (weight e + distance_u)) distances'
                    ,queuePush pq' DijkstraNode {vertex = to (edge e), distance = weight e + distance_u}
                    ,Map.insert (to (edge e)) e path')
                (newDistances, newPq, newPath) = foldl
                    (\(distances', pq', path') e -> case fromJust (Seq.lookup (to (edge e)) distances') of
                        Nothing -> updateElements (distances', pq', path') e
                        Just dist_v | dist_v > (weight e) + distance_u -> updateElements (distances', pq', path') e
                        otherwise -> (distances', pq', path'))
                    (distances, restPq, path) 
                    (edges_of_index graph u)

-- Helper function to get easier access to dijkstra results
distance_sequence_to_distance_map :: (Ord v, Show v) =>  WeightedGraph v -> Seq.Seq (Maybe Float) -> Map.Map v (Maybe Float)
distance_sequence_to_distance_map graph distances =
    Seq.foldlWithIndex (\map i distance -> Map.insert (vertex_at_index graph i) distance map) Map.empty distances
-- Takes a map of edges to reach each node and return a list of edges that goes from *start* to *end*
pathMapToPath :: Int -> Int -> Map.Map Int SimpleWeightedEdge -> [SimpleWeightedEdge]
pathMapToPath start end pathMap
    | Map.size pathMap == 0 = []
    | otherwise = let e = fromJust (Map.lookup end pathMap) in
        buildPath e [e] where
        buildPath e path
            | from (edge e) == start = path
            | otherwise = let nextEdge = fromJust (Map.lookup (from (edge e)) pathMap)
                in buildPath nextEdge (nextEdge:path)

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
    let (distances, pathMap) = dijkstra cityGraphWithEdges "Los Angeles"
    let distanceMap = distance_sequence_to_distance_map cityGraphWithEdges distances
    putStrLn $ "Distances from Los Angeles:"
    mapM_ (\(key,value) -> putStrLn $ key ++ ": " ++ (show (fromJust value))) (Map.assocs distanceMap)
    putStrLn ""
    let path = pathMapToPath
            (index_of_vertex cityGraphWithEdges "Los Angeles")
            (index_of_vertex cityGraphWithEdges "Boston")
            pathMap
    putStrLn "Shortest path from Los Angeles to Boston:"
    putStrLn $ pathToString cityGraphWithEdges path
