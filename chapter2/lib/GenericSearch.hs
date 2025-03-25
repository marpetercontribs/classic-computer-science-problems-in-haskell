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
  , bfs
  , astar
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
instance (Eq s) => Ord (Node s) where
    compare (Node _ _ c1 h1) (Node _ _ c2 h2) = compare (c1+h1) (c2+h2)

nodeToPath :: Node a -> [a]
nodeToPath xs = reverse (nodeToPath' xs) where
    nodeToPath' (Node s Nothing       _ _) = [s]
    nodeToPath' (Node s (Just parent) _ _) = s : nodeToPath' parent

-- ******************** depth-first search ********************

{- We model a stack as a simple list, with the top of the stack being the head
   of the list. This is a bit simpler than the Data.Stack implementation (which
   also keeps track of the size of the stack), but it's good enough for our purposes.
   We can push and pop elements from the stack.
   Note that pushing and popping does not modify a Stack, but results in new Stacks.
   This is in line with staying purely functional. -}
data Stack a = Stack [a] deriving (Show)
stackPush :: Stack a -> a -> Stack a
stackPush (Stack xs) x = Stack (x:xs)
stackPop:: Stack a -> (a, Stack a)
stackPop (Stack []) = error "Cannot pop from an empty stack"
stackPop (Stack (x:xs)) = (x, Stack xs)
stackSize :: Stack a -> Int
stackSize (Stack xs) = length xs

dfs :: (Eq a) => a -> ( a -> Bool) -> (a -> [a]) -> Maybe (Node a)
dfs initial goalTest successors = dfs' (Stack [Node initial Nothing 0 0]) [] goalTest successors
-- the first argument is "frontier" (the "stack" of nodes to visit),
-- the second argument is "explored" (the list of states we have visited)

dfs' :: (Eq a) => Stack (Node a) -> [a] -> ( a -> Bool) -> (a -> [a]) -> Maybe (Node a)
dfs' frontier explored goalTest successors
    | stackSize frontier == 0 = Nothing -- if the frontier is empty, we have failed to find the goal
    | otherwise = if goalTest (state currentNode) then Just currentNode
                  else dfs' newFrontier (explored ++ newChildren) goalTest successors where
        (currentNode, restFrontier) = stackPop frontier
        newChildren = filter (\x -> not (elem x explored)) (successors (state currentNode))
        newFrontier = foldl stackPush restFrontier (map (\x -> Node x (Just currentNode) 0 0) newChildren)

{- A slightly simpler version without explicilty introducing a Stack, simply using a list.
   Note that we take elements from the front of the list, and always add elements to 
   the front. So the frontier is really treated last-in-first-out -}
{-
dfs initial goalTest successors = dfs' [Node initial Nothing 0 0] [] goalTest successors
dfs' :: (Eq a) =>  [Node a] -> [a] -> ( a -> Bool) -> (a -> [a]) -> Maybe (Node a)
dfs' [] _ _ _ = Nothing -- if the frontier is empty, we have failed to find the goal
dfs' (currentNode:restOfFrontier) explored goalTest successors =
    if goalTest (state currentNode) then Just currentNode
    else dfs' (newChildNodes ++ restOfFrontier) (explored ++ newChildren) goalTest successors where
        newChildren = filter (\x -> not (elem x explored)) (successors (state currentNode))
        newChildNodes = map (\x -> Node x (Just currentNode) 0 0) newChildren -}

-- ******************** breadth-first search ********************
{- We model a Queue as a simple list, with the beginning of the Queue being the head
   of the list.
   We can push elements to the end and pop elements from the beginning.
   Note that pushing and popping does not modify a Queue, but results in new Queue.
   This is in line with staying purely functional. -}
data Queue a = Queue [a] deriving (Show)
queuePush :: Queue a -> a -> Queue a
queuePush (Queue xs) x = Queue (xs ++ [x])
queuePop:: Queue a -> (a, Queue a)
queuePop (Queue []) = error "Cannot pop from an empty queue"
queuePop (Queue (x:xs)) = (x, Queue xs)
queueSize :: Queue a -> Int
queueSize (Queue xs) = length xs

bfs :: (Eq a) => a -> ( a -> Bool) -> (a -> [a]) -> Maybe (Node a)
bfs initial goalTest successors = bfs' (Queue [Node initial Nothing 0 0]) [] goalTest successors
bfs' :: (Eq a) => Queue (Node a) -> [a] -> ( a -> Bool) -> (a -> [a]) -> Maybe (Node a)
bfs' frontier explored goalTest successors
    | queueSize frontier == 0 = Nothing -- if the frontier is empty, we have failed to find the goal
    | otherwise = if goalTest (state currentNode) then Just currentNode
                  else bfs' newFrontier (explored ++ newChildren) goalTest successors where
        (currentNode, restFrontier) = queuePop frontier
        newChildren = filter (\x -> not (elem x explored)) (successors (state currentNode))
        newFrontier = foldl queuePush restFrontier (map (\x -> Node x (Just currentNode) 0 0) newChildren)

{- A slightly simpler version without explicilty introducing a Queue, simply using a list.
   Note that we take elements from the front of the list, and always add elements to 
   the end. So the frontier is really treated as a FIFO queue -}
{-
bfs initial goalTest successors = bfs' [Node initial Nothing 0 0] [] goalTest successors
bfs' :: (Eq a) => [Node a] -> [a] -> ( a -> Bool) -> (a -> [a]) -> Maybe (Node a)
bfs' [] _ _ _ = Nothing -- if the frontier is empty, we have failed to find the goal
bfs' (currentNode:restOfFrontier) explored goalTest successors =
    if goalTest (state currentNode) then Just currentNode
    else bfs' (restOfFrontier ++ newChildNodes) (explored ++ newChildren) goalTest successors where
        newChildren = filter (\x -> not (elem x explored)) (successors (state currentNode))
        newChildNodes = reverse (map (\x -> Node x (Just currentNode) 0 0) newChildren)
        -- we reverse the list of new child nodes to really keep the frontier FIFO queue -}

-- ******************** A-star search ********************
data PriorityQueue a = PriorityQueue [a] deriving (Show)
queuePush' :: (Eq a, Ord a) => PriorityQueue a -> a -> PriorityQueue a
queuePush' (PriorityQueue xs) x = PriorityQueue ([ys | ys <- xs, ys < x] ++ [x] ++ [ys | ys <- xs, ys >= x])
queuePop':: PriorityQueue a -> (a, PriorityQueue a)
queuePop' (PriorityQueue []) = error "Cannot pop from an empty priority queue"
queuePop' (PriorityQueue xs) = (head xs, PriorityQueue (tail xs))
queueSize' :: PriorityQueue a -> Int
queueSize' (PriorityQueue xs) = length xs

astar :: (Eq a) => a -> ( a -> Bool) -> (a -> [a]) -> (a -> Double) -> Maybe (Node a)
astar initial goalTest successors heuristic = astar' (PriorityQueue [Node initial Nothing 0 (heuristic initial)]) [(initial, 0)] goalTest successors heuristic
astar' :: (Eq a) => PriorityQueue (Node a) -> [(a,Double)] -> ( a -> Bool) -> (a -> [a]) -> (a -> Double) -> Maybe (Node a)
astar' frontier explored goalTest successors heuristic
    | queueSize' frontier == 0 = Nothing -- if the frontier is empty, we have failed to find the goal
    | otherwise = if goalTest (state currentNode) then Just currentNode
                  else astar' newFrontier newExplored goalTest successors heuristic where
        (currentNode, restFrontier) = queuePop' frontier
        newCost = cost currentNode + 1
        (newFrontier, newExplored) = foldl (
            \(f,e) child -> case lookup child e of -- check if this state is in the explored list already
                Just cost -> if newCost < cost     -- it is, but we found a less costly path
                                then (queuePush' f (Node child (Just currentNode) newCost (heuristic child)), (child,newCost):e)
                                else (f,e)
                Nothing -> (queuePush' f (Node child (Just currentNode) newCost (heuristic child)), (child,newCost):e))
            (restFrontier, explored) (successors (state currentNode))