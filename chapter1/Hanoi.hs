{- Hanoi.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 1
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

{- We model the Hanoi Towers as three Stacks of Integers. The goal is to move all
   elements from the first stack to the last stack, using the second stack as a
   temporary storage. -}

type Tower = Stack Int

solveHanoi :: (Tower, Tower, Tower) -> (Tower, Tower, Tower)
solveHanoi (start, goal, temp) = solveHanoi' (stackSize start) (start, temp, goal) where
   solveHanoi' n (start, goal, temp)
      | n == 1 = let (top, rest) = stackPop start in (rest, stackPush goal top, temp)
      | otherwise = 
            let (start1, temp1, goal1) = solveHanoi' (n-1) (start, temp, goal)
                (start2, goal2, temp2) = solveHanoi' 1 (start1, goal1, temp1)
                (temp3, goal3, start3) = solveHanoi' (n-1) (temp2, goal2, start2)
            in (start3, goal3, temp3)

main :: IO ()
main = do
   let towerA = Stack [1..6]
   let towerB = Stack [] :: Tower
   let towerC = Stack [] :: Tower
   putStrLn ("Hanoi Towers at the beginning: " ++ (show towerA) ++ " - " ++ (show towerB) ++ " - " ++ (show towerC))
   let (towerA', towerB', towerC') = solveHanoi (towerA, towerB, towerC)
   putStrLn ("Hanoi Towers at the end: " ++ (show towerA') ++ " - " ++ (show towerB') ++ " - " ++ (show towerC'))
