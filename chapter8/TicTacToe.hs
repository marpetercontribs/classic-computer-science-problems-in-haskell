{- TicTacToe.hs
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

module TicTacToe (
      TTTBoard
    , TTTPiece(..)
    , newBoard) where

import qualified Board
import MiniMax

data TTTPiece = X | O | E deriving (Eq)
instance Board.Piece TTTPiece where
    opposite X = O
    opposite O = X
    opposite E = E
instance Show TTTPiece where
    show X = "X"
    show O = "O"
    show E = " "

data TTTBoard = TTTBoard { board :: [TTTPiece], turn :: TTTPiece }
newBoard :: TTTBoard
newBoard = TTTBoard { board = replicate 9 E, turn = X }

instance Board.Board TTTBoard TTTPiece Int where
    turn = turn
    legalMoves b = [ i | (i, p) <- zip [0..] (board b), p == E ]
    move b m = b {
        board = take m (board b) ++ [turn b] ++ drop (m + 1) (board b),
        turn = Board.opposite (turn b) }
    isWin b = any (\(i, j, k) -> board b !! i == board b !! j
            && board b !! j == board b !! k && board b !! i /= E) 
        [(0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7),
         (2,5,8), (0,4,8), (2,4,6)]
    evaluate b p = if Board.isWin b
        then if turn b == p then -1 else 1
        else 0

instance Show TTTBoard where
    show b = unlines [
        (show (board b!!0) ++ " | " ++ show (board b!!1) ++ " | " ++ show (board b!!2)),
        "--+---+--",
        (show (board b!!3) ++ " | " ++ show (board b!!4) ++ " | " ++ show (board b!!5)),
        "--+---+--",
        (show (board b!!6) ++ " | " ++ show (board b!!7) ++ " | " ++ show (board b!!8)) ]