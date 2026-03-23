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

module GeneticAlgorithm(
      Chromosome(..)
    , GeneticAlgorithm(..)
    , SelectionType(..)
    , newGeneticAlgorithm
    , runGeneticAlgorithm
    , shuffle
) where

import System.Random(RandomGen, getStdGen, random, randomR)
import Data.List(sortBy)
import Debug.Trace(trace)

class Chromosome t where
    fitness :: t -> Float
    crossover :: t -> t -> (t,t)
    -- randomInstance and mutate return a random generator in addition that can be used for follow-on calls
    randomInstance :: RandomGen g => g -> (t,g)
    mutate :: RandomGen g => t -> g -> (t,g)

data SelectionType = Roulette | Tournament

data (Chromosome c) => GeneticAlgorithm c = GeneticAlgorithm {
      population :: [c]
    , mutationChance :: Float
    , crossOverChance :: Float
    , selectionType :: SelectionType 
}

newGeneticAlgorithm :: Chromosome c => [c] -> Float -> Float -> SelectionType -> GeneticAlgorithm c
newGeneticAlgorithm initialPopulation mutationChance crossOverChance selectionType =
    GeneticAlgorithm {
          population = initialPopulation
        , mutationChance = mutationChance
        , crossOverChance = crossOverChance
        , selectionType = selectionType  }

runGeneticAlgorithm :: (Chromosome c, RandomGen g) => GeneticAlgorithm c -> g -> Int -> Float -> c
runGeneticAlgorithm ga rg maxGenerations threshold = fst $
    foldl nextGeneration (bestOfPopulation ga, (ga, rg)) [1..maxGenerations] where
        bestOfPopulation gg = head (sortBy (\a b -> compare (fitness b) (fitness a)) (population gg)) 
        nextGeneration (best, (ga, rg)) gen =
            if (fitness best) >= threshold
            then (best, (ga, rg)) -- early exit if we beat threshold
            else 
                trace ("Generation " ++ (show gen) ++
                       " Best " ++ (show (fitness best)) ++
                       " Avg " ++ (show ((totalFitness ga) / (fromIntegral (length (population ga)))))) $
                if (fitness best') > (fitness best) then (best', (ga', rg')) else (best, (ga', rg')) where
                (ga', rg') = mutateGA (reproduceAndReplace (ga, rg))
                best' = bestOfPopulation ga' 

mutateGA :: (Chromosome c, RandomGen g) => (GeneticAlgorithm c, g) -> (GeneticAlgorithm c, g)
mutateGA (ga, rg) = ( GeneticAlgorithm {
    population = mutatedPopulation,
    mutationChance = (mutationChance ga),
    crossOverChance = (crossOverChance ga),
    selectionType = (selectionType ga)}, rg') where
        (mutatedPopulation, rg') = foldl
            (\(pop,rg') c ->
                let (rndm, rg'') = random rg'
                    (c',rg''') = mutate c rg' in
                if rndm < (mutationChance ga) then (c':pop,rg''') else ((c:pop, rg''')))
            ([], rg)
            (population ga)

reproduceAndReplace :: (Chromosome c, RandomGen g) => (GeneticAlgorithm c, g) -> (GeneticAlgorithm c, g)
reproduceAndReplace (ga, rg) = (
    GeneticAlgorithm {
        population = nextPopulation,
        mutationChance = (mutationChance ga),
        crossOverChance = (crossOverChance ga),
        selectionType = (selectionType ga)}
    , rg') where
        (nextPopulation, rg') = foldl
            (\(pop,rg') _ ->
                let (parent1, parent2, rg'') = pickParents (ga,rg')
                    (rndm, rg''') = random rg'' in
                if rndm < (crossOverChance ga) 
                then let (parent1',parent2') = crossover parent1 parent2 in ((parent1':parent2':pop), rg''')
                else ((parent1:parent2:pop), rg'''))
            ([], rg)
            [1..length (population ga) `div` 2]

totalFitness :: Chromosome c => GeneticAlgorithm c -> Float
totalFitness ga = sum (map fitness (population ga))

pickParents :: (Chromosome c, RandomGen g) => (GeneticAlgorithm c, g) -> (c,c,g)
pickParents (ga, rg)= case (selectionType ga) of
    Roulette -> undefined
    Tournament -> pickTournament (population ga) (length (population ga) `div` 2) rg

pickTournament :: (Chromosome c, RandomGen g) => [c] -> Int -> g -> (c,c,g)
pickTournament cs numPlayers rg =
    let (shuffledCs, rg') = shuffle (cs, rg)
        (pick1:pick2:_) = sortBy (\a b -> compare (fitness b) (fitness a)) (take numPlayers shuffledCs)
    in (pick1, pick2, rg')
    

-- adapted from https://www.literateprograms.org/fisher-yates_shuffle__haskell_.html
-- probably not very efficient
shuffle :: RandomGen g => ([a],g) -> ([a], g)
shuffle (l, rg) = shuffle' (l,rg) []
  where
    shuffle' ([],rg') acc =  (acc,rg')
    shuffle' (l',rg') acc =
        let (k,rg'') = randomR (0, length l' - 1) rg'
            (lead, x:xs) = splitAt k l'           
        in  shuffle' (lead ++ xs, rg'') (x:acc)

