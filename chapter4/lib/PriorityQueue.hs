module PriorityQueue (
      PriorityQueue(..)
    , queuePush
    , queuePop
    , queueSize
 ) where 

-- a simplistic priority queue - copied from chapter 2.
data PriorityQueue a = PriorityQueue [a] deriving (Show)
queuePush :: (Eq a, Ord a) => PriorityQueue a -> a -> PriorityQueue a
queuePush (PriorityQueue xs) x = PriorityQueue ([ys | ys <- xs, ys < x] ++ [x] ++ [ys | ys <- xs, ys >= x])
queuePop:: PriorityQueue a -> (a, PriorityQueue a)
queuePop (PriorityQueue []) = error "Cannot pop from an empty priority queue"
queuePop (PriorityQueue (x:xs)) = (x, PriorityQueue xs)
queueSize :: PriorityQueue a -> Int
queueSize (PriorityQueue xs) = length xs