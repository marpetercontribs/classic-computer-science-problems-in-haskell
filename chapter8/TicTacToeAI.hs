{- TicTacToeAI.hs
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

module Main where

import TicTacToe (TTTBoard(..), TTTPiece(..), newBoard)
import Board (Board(..), Piece(..))
import MiniMax (minimax, findBestMove)

getMove :: TTTBoard -> IO Int
getMove board = do
    if Board.turn board == X then do
        putStrLn "Enter your move (0-8): "
        move <- getLine
        let moveInt = read move :: Int
        if moveInt `elem` legalMoves board
            then return moveInt
            else do
               putStrLn "Invalid move, try again."
               getMove board
    else do 
        let aiMove = findBestMove board 8
        putStrLn $ "AI plays: " ++ show aiMove
        return aiMove

gameLoop :: TTTBoard -> IO ()
gameLoop board = do
    move <- getMove board
    let newBoard = Board.move board move
    putStrLn $ show newBoard
    putStrLn (show (Board.turn newBoard )++ "'s turn")
    if isWin newBoard then
        if Board.turn newBoard == O then putStrLn "You win!" else putStrLn "AI wins!"
    else if isDraw newBoard then putStrLn "It's a draw!"
    else gameLoop newBoard

main :: IO ()
main = do
    let board = newBoard
    putStrLn "Welcome to Tic Tac Toe!"
    gameLoop board