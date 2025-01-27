-- Fib3.hs
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


{-
  The technique used to memoize in usual programming languages, a mutable
  dictionary/hashmap, cannot be used in a purely functional way (Haskell)
  directly because it would require a mutable data structure, which we
  don't want to introduce just for the sake of memoization.

  But we can make use of how Haskell lazily evaluates (for example lists)
  and "caches" list elements once they have been evaluated: to memoize results
  of a function taking Integer arguments, we can map from the Integers to
  the corresponding results. Note that lazy evaluation means that Haskell
  does not automatically generate the infinite list [0..], but only those elements
  that are really requested.
  
  See e.g. https://kseo.github.io/posts/2017-01-14-memoization-in-hasekll.html
-}

import Data.List -- to be able to use genericIndex instead of the "standard"
                 -- list index operator !! below, because !! requires type Int

fib3 :: Integer -> Integer
fib3 = genericIndex (map plainFib [0..]) 
    where
        plainFib n
            | n < 2     = n
            | otherwise = fib3 (n-2) + fib3 (n-1)

main :: IO ()
main = do
    putStrLn ("The    5th Fibonacci number is: " ++ show (fib3 5))
    putStrLn ("The 1000th Fibonacci number is: " ++ show (fib3 1000))