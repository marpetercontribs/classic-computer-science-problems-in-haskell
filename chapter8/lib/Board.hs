{- Board.hs
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

module Board(
      Board(..)
    , Piece(..)
) where

class Piece p where
   opposite :: p -> p

class (Piece p) => Board b p m | b -> p, b -> m where
    turn :: b -> p
    legalMoves :: b -> [m]
    move :: b -> m -> b
    isWin :: b -> Bool
    isDraw :: b -> Bool
    isDraw b = not (isWin b) && null (legalMoves b)
    evaluate :: b -> p -> Double
