{- ConnectFourAI.hs
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

import ConnectFour(Board, Piece(..), newBoard)
import Board
import MiniMax (findBestMove)
import System.IO (hFlush, stdout)

gameLoop :: ConnectFour.Board -> IO ()
gameLoop board = do
    move <- getMove board
    let newBoard = Board.move board move
    putStrLn $ show newBoard
    if isWin newBoard then
        if Board.turn newBoard == R then putStrLn "You win!" else putStrLn "AI wins!"
    else if isDraw newBoard then putStrLn "It's a draw!"
    else gameLoop newBoard

getMove :: ConnectFour.Board -> IO Int
getMove board = do
      if Board.turn board == Y then do -- human's turn
         putStr "Enter your move (0-6): "
         hFlush stdout -- Ensure the prompt is printed before waiting for input
         move <- getLine
         let moveInt = read move :: Int
         if moveInt `elem` legalMoves board
               then return moveInt
               else do
                putStrLn "Invalid move, try again."
                getMove board
      else do
         let move = findBestMove board 7
         putStrLn $ "AI plays column " ++ show move
         return move

main :: IO ()
main = do
    putStrLn "Welcome to Connect Four! You are Yellow (Y) and the AI is Red (R)."
    let board = newBoard
    gameLoop board
