{- GeneticAlgorithm.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 5
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

module Main (main) where

import GeneticAlgorithm
import System.Random(RandomGen, random, randomR, getStdGen)

data SimpleEquation = SimpleEquation { x :: Int, y :: Int }
instance Chromosome SimpleEquation where
    fitness se = fromIntegral(6 * xx - xx * xx + 4* yy - yy * yy) where
        xx = x se
        yy = y se         
    crossover rg this that = ( SimpleEquation { x = x this, y = y that },
                               SimpleEquation { x = x that, y = y this}, rg )
    randomInstance rg = (SimpleEquation { x = x, y = y }, rg'') where
            (x, rg') = randomR (0,100) rg
            (y, rg'') = randomR (0,100) rg'
    mutate se rg =  if switch1 then
                        if switch2 then (SimpleEquation { x = (x se) + 1, y = y se }, rg''')
                        else (SimpleEquation { x = (x se) - 1, y = y se }, rg''')
                    else
                        if switch3 then (SimpleEquation { x = x se, y = (y se) + 1 }, rg''')
                        else (SimpleEquation { x = x se, y = (y se) - 1 }, rg''')
                    where 
                        (switch1, rg') = random rg
                        (switch2, rg'') = random rg'
                        (switch3, rg''') = random rg''
instance Show SimpleEquation where
    show se = "X: " ++ (show (x se)) ++ ", Y: " ++ (show (y se)) ++ "  Fitness: " ++ (show (fitness se))

main :: IO ()
main = do
    rg <- getStdGen
    let (randomSEs, rg') = foldl 
            (\(rses, rg') _ -> let (rse,rg'') = randomInstance rg' in (rse:rses,rg'')) 
            ([],rg) [1..20]
    putStrLn $ show (randomSEs :: [SimpleEquation])
    let ga = newGeneticAlgorithm randomSEs 0.1 0.7 Tournament
    putStrLn $ show (runGeneticAlgorithm ga rg' 100 13.0)
