{- Fib2.hs
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

fib2 :: Integer -> Integer
-- Note that this implementation is not tail-recursive, so better don't use too high values for n.
fib2 n
    | n < 2 = n
    | otherwise = fib2 (n-2) + fib2 (n-1)

main :: IO ()
main = do
    putStrLn ("The 5th Fibonacci number is: " ++ show (fib2 5))
    putStrLn ("The 10th Fibonacci number is: " ++ show (fib2 10))
