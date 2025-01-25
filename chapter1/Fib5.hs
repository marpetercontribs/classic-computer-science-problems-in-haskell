-- Fib5.hs
-- Adapted From Classic Computer Science Problems in Python/Java Chapter 1
-- Copyright 2025 Markus Peter
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
-- You wouldn't use a classical "for" loop in Haskell. But something getting
-- close is using a recursion with a counter, which also allows making the
-- recursive algorithm (e.g. of Fib2) tail-recursive, thus faster and much 
-- less memory consuming

module Main where

fib5 :: Integer -> Integer
fib5 n = firstElement (fibonacciLoop (0, 1, n))
    where
        firstElement (x,_,_) = x
        fibonacciLoop (last,next,0) = (last, next, 0)
        fibonacciLoop (last,next,counter) = fibonacciLoop(next, last+next, counter-1 )

main :: IO ()
main = do
    putStrLn ("The 5th Fibonacci number is: " ++ show (fib5 5))
    putStrLn ("The 1000th Fibonacci number is: " ++ show (fib5 1000))