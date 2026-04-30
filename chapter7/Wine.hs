{- Wine.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 7
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

import qualified NeuralNetwork as Network
import qualified Layer as Layer
import qualified Neuron as Neuron
import Utils(csvStringTo, shuffle, normalizeByFeatureScaling, sigmoid, sigmoidDerivative)
import System.Random(RandomGen, getStdGen)

interpretation :: [Double] -> Int
interpretation output = if maxOutput == output !! 0 then 1
                    else if maxOutput == output !! 1 then 2
                    else 3
  where maxOutput = maximum output  


main = do
    content <- readFile "data/wine.csv"
    rg <- getStdGen
    let (dataset, rg')  = shuffle (csvStringTo content, rg)
    let parameters = normalizeByFeatureScaling $
    -- the '0':s ensures that each decimal starts with a 0, so that read can parse it as a Double
                     map ( map (\s -> read ('0':s) :: Double) . drop 1) dataset
    let species = map (\(h:t) -> read h :: Int) dataset
    let classifications = map (\sp -> case sp of
                                1 -> [1.0, 0.0, 0.0]
                                2 -> [0.0, 1.0, 0.0]
                                3 -> [0.0, 0.0, 1.0]
                                _ -> error "Unknown class in dataset") species
    let (wineNN, rg'') = Network.new rg' [13, 7, 3] 0.9 sigmoid sigmoidDerivative
    let wineNNTrained = foldl (\nn run -> 
         Network.train nn (take 150 parameters) (take 150 classifications))
         wineNN [1..10]
    let (correct, trials, percentage) = Network.validate wineNNTrained
          (drop 150 parameters) (drop 150 species) interpretation
    putStrLn $ show correct ++ " correct of " ++ show trials ++ 
      " = " ++ show (100*percentage) ++ "%"
