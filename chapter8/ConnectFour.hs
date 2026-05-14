{- ConnectFour.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 8
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

{-# LANGUAGE FunctionalDependencies #-}

module ConnectFour(
      Board
    , Piece(..)
    , newBoard) where

import qualified Board
import MiniMax
import Data.List(transpose)

data Piece = R | Y | E deriving (Eq)
instance Board.Piece Piece where
    opposite R = Y
    opposite Y = R
    opposite E = E
instance Show Piece where
    show R = "R"
    show Y = "Y"
    show E = " "

-- Constants for the number of rows and columns
-- Note that in Haskell, all "variables" are in fact constants by default
numRows = 6
numCols = 7

data Board = Board {
    board :: [[Piece]],
    columnLevel :: [Int],
    turn :: Piece }

newBoard :: Board
newBoard = Board {
    board = replicate numCols (replicate numRows E),
    columnLevel = replicate numCols 0,
    turn = Y }

instance Show Board where
    show b = unlines $ reverse (map
        (\row -> foldl (\acc col -> acc ++ show col ++ "|") "|" row)
        (transpose (board b)))

instance Board.Board Board Piece Int where
    turn = turn
    legalMoves b = [col | (col,lvl) <- zip [0..] (columnLevel b), lvl<numRows]
    move b m = b { 
          board = zipWith
            (\col pcs -> if col == m
                then take (columnLevel b!!m) pcs
                     ++ [turn b]
                     ++ drop ((columnLevel b!!m)+1) pcs
                else pcs)
            [0..] (board b)
        , columnLevel = zipWith 
            (\col lvl -> if col == m then lvl+1 else lvl)
            [0..] (columnLevel b)
        , turn = (Board.opposite (turn b))}
    isWin b = any (\(yc,rc) -> yc == 4 || rc == 4)
        (map (countSegment b) segments)
    evaluate b p = sum (map (\s -> evaluateSegment b s p) segments)

type Segment = [(Int,Int)]

generateSegments :: Int -> Int -> Int -> [Segment]
generateSegments numCols numRows length =
    verticals ++ horizontals ++ diagonalsTl2Br ++ diagonalsBl2Tr
    where
        verticals = [ [(col,row+i) | i <- [0..length-1]] | 
            row <- [0..numRows-length], col <- [0..numCols-1] ]
        horizontals = [ [(col+i,row) | i <- [0..length-1]] |
            row <- [0..numRows-1], col <- [0..numCols-length] ]
        diagonalsBl2Tr = [ [(col+i,row+i) | i <- [0..length-1]] |
            row <- [0..numRows-length], col <- [0..numCols-length] ]
        diagonalsTl2Br = [ [(col+i,row-i) | i <- [0..length-1]] |
            row <- [length-1..numRows-1], col <- [0..numCols-length] ]

segments = generateSegments numCols numRows 4

countSegment :: Board -> Segment -> (Int,Int)
countSegment b segment = foldl (\(yc,rc) (col,row) ->
    if board b !! col !! row == Y then (yc+1,rc)
    else if board b !! col !! row == R then (yc,rc+1)
    else (yc,rc)
    ) (0,0) segment

evaluateSegment :: Board -> Segment -> Piece -> Double
evaluateSegment b s p
    | yc>0 && rc>0 = 0 -- mixed segments are neutral
    | winner == p = score -- only player's pieces
    | otherwise = -score -- only opponent's pieces
    where (yc,rc) = countSegment b s
          score = (case max yc rc of
                2 -> 1
                3 -> 100
                4 -> 1000000
                _ -> 0)
          winner = if rc > yc then R else Y




