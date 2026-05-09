{- MiniMax.hs
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

module MiniMax(
     minimax
   , findBestMove
) where


import Board (Board(..), Piece(..))
import Data.List(maximumBy)

minimax :: (Show b, Board b p m) => b -> Bool -> p -> Int -> Double
minimax board isMaximizing player maxDepth
    -- termination condition: the board is in a terminal state or we've reached the maximum depth
    | isWin board || isDraw board || maxDepth == 0 = evaluate board player
    -- recursively evaluate the next boards
    | isMaximizing = maximum (map (\b -> minimax b False player (maxDepth - 1))
                        (map (move board) (legalMoves board)))
    | otherwise    = minimum (map (\b -> minimax b True player (maxDepth - 1))
                        (map (move board) (legalMoves board)))

findBestMove :: (Show b, Board b p m) => b -> Int -> m
findBestMove board maxDepth = fst $ maximumBy
      (\(_, score1) (_, score2) -> compare score1 score2) -- compare the scores of the moves
      -- for each legal move, get the tuple of the move and its minimax score
      (map (\m -> (m, minimax (move board m) False (turn board) maxDepth))
         (legalMoves board))