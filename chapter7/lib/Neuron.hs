{- Neuron.hs
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

module Neuron(
       Neuron(..)
     , new
     , output
) where

import Utils(dotProduct)

data Neuron = Neuron {
     weights :: [Double]
   , learningRate :: Double
   , outputCache :: Double
   , activationFunction :: Double -> Double 
}

new :: [Double] -> Double -> (Double -> Double) -> Neuron
new weights learningRate activationFunction =
    Neuron { weights = weights
           , learningRate = learningRate
           , outputCache = 0.0
           , activationFunction = activationFunction
           }

output :: [Double] -> Neuron -> (Neuron, Double)
output inputs neuron = (neuron { outputCache = cache }, activationFunction neuron cache)
    where cache = dotProduct (weights neuron) inputs