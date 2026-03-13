{- Graph.hs (Graph Data Structures)
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{- Graph data structures and related type classes -}

module Graph (
      Edge(..)
    , WeightedEdge(..)
    , SimpleEdge(..)
    , SimpleWeightedEdge(..)
    , Graph(..)
    , UnweightedGraph(..)
    , WeightedGraph(..)
    , add_edge_by_vertices
    , add_edge_by_vertices'
) where


import Data.List (findIndex)

class Edge a where
    from :: a -> Int
    to :: a -> Int
    reversed :: a -> a

class (Edge a) => WeightedEdge a where
    weight :: a -> Float

data SimpleEdge = SimpleEdge { u :: Int, v :: Int } deriving (Eq)
instance Edge SimpleEdge where
    from = u
    to = v
    reversed (SimpleEdge { u = u, v = v }) = SimpleEdge { u = v, v = u }
instance Show SimpleEdge where
    show (SimpleEdge { u = u, v = v }) = show u ++ "->" ++ show v

data SimpleWeightedEdge = SimpleWeightedEdge { edge :: SimpleEdge, w :: Float } deriving (Eq)
instance Edge SimpleWeightedEdge where
    from = from . edge
    to = to . edge
    reversed (SimpleWeightedEdge { edge = edge, w = w }) = SimpleWeightedEdge { edge = reversed edge, w = w }
instance WeightedEdge SimpleWeightedEdge where
    weight = w
instance Show SimpleWeightedEdge where
    show (SimpleWeightedEdge { edge = edge, w = w }) = show edge ++ " (weight: " ++ show w ++ ")"

{- e is the type of the edges
   v is the type of the vertices
   g is the type of the graph
-}

class (Edge e, Eq v) => Graph g e v | g -> e v where
    makeGraph :: [v] -> g
    -- "Getter methods"
    vertices :: g -> [v]
    edges :: g -> [[e]]
    -- modifier methods
    add_vertex :: g -> v -> g -- returns the index of the new vertex in the original book
    add_edge :: g -> e -> g
    -- helper methods
    -- get the number of vertices and edges in the graph
    get_vertex_count :: g -> Int
    get_vertex_count g = length (vertices g)
    get_edge_count :: g -> Int
    get_edge_count g = sum (map length (edges g))
    -- find the index of a vertex in the graph (unsafe)
    index_of_vertex :: g -> v -> Int
    index_of_vertex g v 
        | Just i <- findIndex (== v) (vertices g) = i
        | otherwise = error "Vertex not found in graph"
    -- find the vertex at a specific index (unsafe)
    vertex_at_index :: g -> Int -> v
    vertex_at_index g i = vertices g !! i
    -- find the vertices that a vertex at some index is connected to
    neighbors_of_index :: g -> Int -> [v]
    neighbors_of_index g i = map (\edge -> vertex_at_index g (to edge)) (edges g !! i)
    -- return all of the edges associated with a vertex at some index
    edges_of_index :: g -> Int -> [e]
    edges_of_index g i = edges g !! i
    -- look up a vertice's index and find its neighbors (convenience method)
    neighbors_of :: g -> v -> [v]
    neighbors_of g v = neighbors_of_index g (index_of_vertex g v)
    -- look up the index of a vertex and return its edges (convenience method)
    edges_of :: g -> v -> [e]
    edges_of g v = edges_of_index g (index_of_vertex g v)



data (Eq v) => UnweightedGraph v = UnweightedGraph { verticesList :: [v], edgesList :: [[SimpleEdge]] }
instance (Eq v) => Graph (UnweightedGraph v) SimpleEdge v where
    vertices (UnweightedGraph { verticesList = vs }) = vs
    edges (UnweightedGraph { edgesList = es }) = es
    add_vertex (UnweightedGraph { verticesList = vs, edgesList = es }) v = 
        UnweightedGraph { verticesList = vs ++ [v], edgesList = es ++ [[]] }
    add_edge (UnweightedGraph { verticesList = vs, edgesList = es }) e = 
        UnweightedGraph { verticesList = vs, edgesList = map (appendToFromAndToVertexEdges e) (zip [0..length es-1] es)} where
            appendToFromAndToVertexEdges :: SimpleEdge -> (Int, [SimpleEdge]) -> [SimpleEdge]
            appendToFromAndToVertexEdges e (vertexIndex, edgesOfVertex) = 
                if from e == vertexIndex then edgesOfVertex ++ [e]
                else if to e == vertexIndex then edgesOfVertex ++ [reversed e]
                else edgesOfVertex
    makeGraph vs = UnweightedGraph { verticesList = vs, edgesList = map (const []) vs } -- initialize the graph with the vertices and empty edge lists

instance (Eq v, Show v) => Show (UnweightedGraph v) where
    show g = unlines (map (\v -> show v ++ ": " ++ show (neighbors_of g v)) (vertices g))

add_edge_by_vertices :: (Eq v) => (v,v) -> UnweightedGraph v -> UnweightedGraph v
add_edge_by_vertices (v1, v2) g =
    let i1 = index_of_vertex g v1
        i2 = index_of_vertex g v2
        edge = SimpleEdge { u = i1, v = i2 }
    in add_edge g edge

data (Eq v) =>  WeightedGraph v = WeightedGraph { verticesListW :: [v], edgesListW :: [[SimpleWeightedEdge]] }
instance (Eq v) =>  Graph (WeightedGraph v) SimpleWeightedEdge v where
    vertices (WeightedGraph { verticesListW = vs }) = vs
    edges (WeightedGraph { edgesListW = es }) = es
    add_vertex (WeightedGraph { verticesListW = vs, edgesListW = es }) v = 
        WeightedGraph { verticesListW = vs ++ [v], edgesListW = es ++ [[]] }
    add_edge (WeightedGraph { verticesListW = vs, edgesListW = es }) e = 
        WeightedGraph { verticesListW = vs, edgesListW = map (appendToFromAndToVertexEdges e) (zip [0..length es-1] es)} where
            appendToFromAndToVertexEdges :: SimpleWeightedEdge -> (Int, [SimpleWeightedEdge]) -> [SimpleWeightedEdge]
            appendToFromAndToVertexEdges e (vertexIndex, edgesOfVertex) = 
                if from e == vertexIndex then edgesOfVertex ++ [e]
                else if to e == vertexIndex then edgesOfVertex ++ [reversed e]
                else edgesOfVertex
    makeGraph vs = WeightedGraph { verticesListW = vs, edgesListW = map (const []) vs }

neighbors_of_index_with_weight :: (Eq v) => WeightedGraph v -> Int -> [(v, Float)]
neighbors_of_index_with_weight g i = 
    map (\edge -> (vertex_at_index g (to edge), weight edge)) (edges g !! i)

instance (Eq v, Show v) => Show (WeightedGraph v) where
    show g = unlines (map (\i -> show (vertex_at_index g i) ++ ": " ++ show (neighbors_of_index_with_weight g i)) [0..length (vertices g) - 1])

add_edge_by_vertices' :: (Eq v) => (v,v, Float) -> WeightedGraph v -> WeightedGraph v
add_edge_by_vertices' (v1, v2, weight) g =
    let i1 = index_of_vertex g v1
        i2 = index_of_vertex g v2
        edge = SimpleWeightedEdge { edge = SimpleEdge {u = i1, v = i2}, w = weight }
    in add_edge g edge