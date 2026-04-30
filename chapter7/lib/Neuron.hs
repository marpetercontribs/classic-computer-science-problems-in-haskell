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
     , updateCache
     , output
) where

import Utils(dotProduct)

{- Note: this implementation
         - does not use a "delta" field in the Neurons - the deltas are
           passed from layer to layer during backpropagation and returned there as list of lists,
           then passed, within the Layer's train function, to the updateWeights function
         - does not store the activationFunctionDerivative in the Neuron, but in the Layer instead
-}
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

-- calculate the 'outputCache' of a neuron for a given input
-- and return the new neuron with updated outputCache, to remain pure
updateCache :: [Double] -> Neuron -> Neuron
updateCache inputs neuron =
    neuron { outputCache = dotProduct (weights neuron) inputs }

-- return the true output of the neuron for a given input,
-- assuming the neuron's outputCache is already updated for the given input)
output :: [Double] -> Neuron -> Double
output inputs neuron = activationFunction neuron (outputCache neuron)