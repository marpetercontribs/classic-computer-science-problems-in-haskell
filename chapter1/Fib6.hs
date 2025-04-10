{- Fib6.hs
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

{-
   An infinite list in Haskell is close to a "generator", although it "remembers"
   all its elements. Nevertheless, you could use it in a list comprehension similar
   to how you would use a stream in Java or a generator in Python.
-}

module Main where
   
fib6 = fibGenerator 1 1 where
    fibGenerator last next = last:(fibGenerator next (last+next))

main :: IO ()
main = do
    putStrLn ("The first 50 Fibonacci numbers are: " ++ show (take 50 fib6)) 