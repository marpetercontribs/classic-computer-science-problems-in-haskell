{- Maze.hs
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

module Main where

import GenericSearch(dfs, bfs, Node, nodeToPath)
import System.Random (randoms, getStdGen, RandomGen)

data Cell = Empty | Blocked | Start | Goal | Path deriving (Eq)
instance Show Cell where
    show Empty = " "
    show Blocked = "X"
    show Start = "S"
    show Goal = "G"
    show Path = "*"

data MazeLocation = MazeLocation { row :: Int, column :: Int } deriving (Eq)
instance Show MazeLocation where
    show (MazeLocation row col) = "(" ++ show col ++ ", " ++ show row ++ ")"    

data Maze = Maze {
      rows :: Int
    , columns :: Int
    , start :: MazeLocation
    , goal :: MazeLocation
    , grid :: [[Cell]] }
instance Show Maze where
    show (Maze _ _ _ _ grid) = unlines (map (concatMap show) grid)
        -- unlines appends \n to each string and concatenates the list of strings
        -- concatMap applies the function to each element of the list and concatenates the results
        
-- create a maze with the default dimensions, start, goal and "sparseness"
newMaze :: RandomGen g => g -> Maze
newMaze = newMazeWithParams 10 10 0.2 (MazeLocation 0 0) (MazeLocation 9 9)
{- create a maze with the given dimensions, start, goal and "sparseness"
   note that we add a random number generator (rng) to the parameters.
   This allows us to get an rng in the main function without having to provide an explicit seed,
   and pass it to the newMazeWithParams function without having to use a monad in the function.
   This way we get a different maze each time we run the program.
   Alternative would be to use `randoms (mkStdGen <some seed>)` in the list comprehension below,
   leading to exactly the same maze each time we run the program.
-}
newMazeWithParams :: RandomGen g => Int -> Int -> Double -> MazeLocation -> MazeLocation -> g -> Maze
newMazeWithParams rows columns sparseness start goal rng = 
    Maze rows columns start goal (makeGrid start goal) where
        makeGrid (MazeLocation sc sr) (MazeLocation gc gr)
            = chunksOf columns (
                zipWith (\(x,y) s -> 
                    if      x==sc && y==sr then Start 
                    else if x==gc && y==gr then Goal
                    else if s < sparseness then Blocked
                    else Empty)
                -- create a list of coordinates and a random number to determine if the cell is blocked
                [ (x,y) | x<-[0..(columns-1)], y<-[0..(rows-1)]] (randoms rng :: [Double])
            )

markMaze :: Maze -> [MazeLocation] -> Maze
markMaze maze [] = maze
markMaze maze (x:xs) 
    | x == start maze || x == goal maze = markMaze maze xs -- don't mark start and goal
    | otherwise = markMaze (maze { grid = pathAt (grid maze) x }) xs where
        pathAt g (MazeLocation r c) = 
            take r g ++ [take c (g !! r) ++ [Path] ++ drop (c+1) (g !! r)] ++ drop (r+1) g

-- checks if the location is the goal's location in the maze
goalTest :: Maze -> MazeLocation -> Bool
goalTest maze loc = loc == goal maze

-- returns the successors of the given location in the maze
successors :: Maze -> MazeLocation -> [MazeLocation]
successors maze (MazeLocation row col) = filter isLegalLocation
    [MazeLocation (row-1) col, MazeLocation (row+1) col,
     MazeLocation row (col-1), MazeLocation row (col+1)] where
        isLegalLocation (MazeLocation r c) =
            r >= 0 && r < rows maze &&
            c >= 0 && c < columns maze &&
            (grid maze !! r) !! c /= Blocked

-- helper function that splits a list into a (list of) chunks of size n, same as in DnaSearch.hs
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = chunksOf' n (xs,[]) where
    chunksOf' n (xs,chunks)
        | null xs       = reverse chunks
        | length xs < n = reverse (xs:chunks)
        -- reverse because the below produces the chunks in reverse order to
        -- avoid repeated use of append (++), which would lead to O(n^2) run
        -- time with n = length xs
        | otherwise     = chunksOf' n (xs', chunk:chunks)
            where (chunk, xs') = splitAt n xs

main :: IO ()
main = do
    randomGen <- getStdGen
    let maze = newMaze randomGen
    putStrLn $ show maze
    putStrLn "Depth-first search:"
    let solution1 = dfs (start maze) (goalTest maze) (successors maze)
    case solution1 of
        Nothing   -> putStrLn "No solution found"
        Just node -> putStr $ show (markMaze maze (nodeToPath node))
    putStrLn "Breadth-first search:"
    let solution2 = bfs (start maze) (goalTest maze) (successors maze)
    case solution2 of
        Nothing   -> putStrLn "No solution found"
        Just node -> putStr $ show (markMaze maze (nodeToPath node))