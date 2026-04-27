{- Iris.hs
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

interpretation :: [Double] -> String
interpretation output = if maxOutput == output !! 0 then "Iris-setosa"
                    else if maxOutput == output !! 1 then "Iris-versicolor"
                    else "Iris-virginica"
  where maxOutput = maximum output

main :: IO ()
main = do
    content <- readFile "data/iris.csv"
    rg <- getStdGen
    let (dataset, rg')  = shuffle (csvStringTo content, rg)
    let parameters = normalizeByFeatureScaling $
                     map ( map (\s -> read s :: Double) . take 4) dataset
    let species = map last dataset
    let classifications = map (\sp -> case sp of
                                "Iris-setosa" -> [1.0, 0.0, 0.0]
                                "Iris-versicolor" -> [0.0, 1.0, 0.0]
                                "Iris-virginica" -> [0.0, 0.0, 1.0]
                                _ -> error "Unknown class in dataset") species
    let (irisNN, rg'') = Network.new rg' [4, 6, 3] 0.3 sigmoid sigmoidDerivative
    let irisNNTrained = foldl (\nn run -> 
         Network.train nn (take 140 parameters) (take 140 classifications))
         irisNN [1..50]
    let (correct, trials, percentage) = Network.validate irisNNTrained
          (drop 140 parameters) (drop 140 species) interpretation
    putStrLn $ show correct ++ " correct of " ++ show trials ++ 
      " = " ++ show (100*percentage) ++ "%"
    