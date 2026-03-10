module Main (main) where

import Graph (
      Edge(..)
    , Graph(..)
    , WeightedEdge(..)
    , SimpleEdge(..)
    , SimpleWeightedEdge(..)
    , UnweightedGraph(..)
    , WeightedGraph(..)
    , add_edge_by_vertices
    , add_edge_by_vertices'
    )

main :: IO ()
main = do
    let cityGraph = makeGraph ["Seattle", "San Francisco", "Los Angeles",
            "Riverside", "Phoenix", "Chicago", "Boston", "New York", "Atlanta",
            "Miami", "Dallas", "Houston", "Detroit", "Philadelphia", "Washington"] :: UnweightedGraph String
    let cityGraphWithEdge = foldr add_edge_by_vertices cityGraph [
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
    putStrLn "Unweighted city graph with edges added:"
    putStrLn $ "Vertices: " ++ show (vertices cityGraphWithEdge)
    putStrLn $ "Edges: " ++ show (edges cityGraphWithEdge)
    putStrLn "-----"
    let cityGraphWeighted = makeGraph ["Seattle", "San Francisco", "Los Angeles",
            "Riverside", "Phoenix", "Chicago", "Boston", "New York", "Atlanta",
            "Miami", "Dallas", "Houston", "Detroit", "Philadelphia", "Washington"] :: WeightedGraph String
    let cityGraphWeightedWithEdges = foldr add_edge_by_vertices' cityGraphWeighted [
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
    putStrLn "Weighted city graph with edges added:"
    putStrLn $ "Vertices: " ++ show (vertices cityGraphWeightedWithEdges)
    putStrLn $ "Edges: " ++ show (edges cityGraphWeightedWithEdges)
