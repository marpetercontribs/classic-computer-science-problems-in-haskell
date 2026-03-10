{- UnitSpec.hs
   For testing graph data structures and related functions
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

import Test.HUnit
import Graph

cities = ["Seattle", "San Francisco", "Los Angeles",
        "Riverside", "Phoenix", "Chicago", "Boston", "New York", "Atlanta",
        "Miami", "Dallas", "Houston", "Detroit", "Philadelphia", "Washington"]

testUnweightedGraphWithEdges = TestCase( do
    let cityGraph = makeGraph cities :: UnweightedGraph String
    let cityGraphWithEdges = foldr add_edge_by_vertices cityGraph [
            ("Seattle", "Chicago"), 
            ("Seattle", "San Francisco"),
            ("San Francisco", "Riverside"),
            ("San Francisco", "Los Angeles"),
            ("Los Angeles", "Riverside"),
            ("Los Angeles", "Phoenix"),
            ("Riverside", "Phoenix"),
            ("Riverside", "Chicago"),
            ("Phoenix", "Dallas"),
            ("Phoenix", "Houston"),
            ("Dallas", "Chicago"),
            ("Dallas", "Atlanta"),
            ("Dallas", "Houston"),
            ("Houston", "Atlanta"),
            ("Houston", "Miami"),
            ("Atlanta", "Chicago"),
            ("Atlanta", "Washington"),
            ("Atlanta", "Miami"),
            ("Miami", "Washington"),
            ("Chicago", "Detroit"),
            ("Detroit", "Boston"),
            ("Detroit", "Washington"),
            ("Detroit", "New York"),
            ("Boston", "New York"),
            ("New York", "Philadelphia"),
            ("Philadelphia", "Washington")]
    assertEqual
        (unlines [ "--- Unweighted graph with edges added ---"
                 , "Vertices: " ++ show (vertices cityGraphWithEdges)
                 , "Edges: " ++ show (edges cityGraphWithEdges)
                 ])
        ((length (vertices cityGraphWithEdges) == 15) && (sum (map length (edges cityGraphWithEdges)) == 52))
        True
    )

testWeightedGraphWithEdges = TestCase( do
    let cityGraphWeighted = makeGraph cities :: WeightedGraph String
    let cityGraphWeightedWithEdge = foldr add_edge_by_vertices' cityGraphWeighted [
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
    assertEqual
        (unlines [  "--- Weighted graph with edges added ---"
                 ,  "Vertices: " ++ show (vertices cityGraphWeightedWithEdge)
                 , "Edges: " ++ show (edges cityGraphWeightedWithEdge)
                 ])
        ((length (vertices cityGraphWeightedWithEdge) == 15) && (sum (map length (edges cityGraphWeightedWithEdge)) == 52))
        True

    )

tests = TestList [ TestLabel "testUnweightedGrapgWithEdges" testUnweightedGraphWithEdges
                 , TestLabel "testWeightedGraphWithEdges" testWeightedGraphWithEdges
                 ]

main :: IO Counts
main = do 
    runTestTT tests