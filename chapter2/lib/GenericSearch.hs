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
  , Node(..)
  , dfs
  , nodeToPath
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

-- the 'Node' type to remember which "states" we have visited and how we got there
data Node s = Node {
      state :: s
    , parent :: Maybe (Node s)
    , cost :: Double
    , heuristic :: Double }
-- the A* search algorithm needs ordering for the Node type
instance (Eq s) => Eq (Node s) where
    (==) (Node s1 _ c1 h1) (Node s2 _ c2 h2) = s1 == s2 && c1 == c2 && h1 == h2
instance (Ord s) => Ord (Node s) where
    compare (Node _ _ c1 h1) (Node _ _ c2 h2) = compare (c1+h1) (c2+h2)

nodeToPath :: Node a -> [a]
nodeToPath xs = reverse (nodeToPath' xs) where
    nodeToPath' (Node s Nothing       _ _) = [s]
    nodeToPath' (Node s (Just parent) _ _) = s : nodeToPath' parent

-- depth-first search
-- we omit introducting a Stack for the fronter - a simple list will do
dfs :: (Eq a) => a -> ( a -> Bool) -> (a -> [a]) -> Maybe (Node a)
dfs initialNode goalTest successors = dfs' [Node initialNode Nothing 0 0] [] goalTest successors
-- the first argument is "frontier" (the "stack" of nodes to visit),
-- the second argument is "explored" (the list of states we have visited)

dfs' :: (Eq a) => [Node a] -> [a] -> ( a -> Bool) -> (a -> [a]) -> Maybe (Node a)
dfs' [] _ _ _ = Nothing -- if the frontier is empty, we have failed to find the goal
dfs' (currentNode:restOfFrontier) explored goalTest successors =
    if goalTest (state currentNode) then Just currentNode
    else dfs' (restOfFrontier ++ newChildNodes) (explored ++ newChildren) goalTest successors where
        newChildren = filter (\x -> not (elem x explored)) (successors (state currentNode))
        newChildNodes = map (\x -> Node x (Just currentNode) 0 0) newChildren