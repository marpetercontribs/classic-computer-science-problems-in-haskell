{- Fib4.hs
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

import Data.Function.Memoize

fib4 :: Integer -> Integer
fib4 = memoize plainFib where
    plainFib n
        | n < 2 = n
        | otherwise = fib4 (n-2) + fib4 (n-1)

main :: IO ()
main = do
    putStrLn ("The    5th Fibonacci number is: " ++ show (fib4 5))
    putStrLn ("The 1000th Fibonacci number is: " ++ show (fib4 1000))