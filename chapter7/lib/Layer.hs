{- Layer.hs
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

module Layer(
       Layer(..)
     , new
     , outputs
     , deltasForOutputLayer
     , deltasForHiddenLayer
) where

import qualified Neuron as Neuron
import Utils(dotProduct)
import System.Random(RandomGen, random)

data Layer = Layer {
       previousLayer :: Maybe Layer
     , neurons :: [Neuron.Neuron]
     , activationFunctionDerivative :: Double -> Double
     , outputCache :: [Double]
}

new :: RandomGen rg => (Maybe Layer, rg) -> Int -> Double ->
    (Double -> Double) -> (Double -> Double) -> (Layer, rg)
new (Nothing, rg) numOfNeurons learningRate activationFn activationFn' =
    (Layer { previousLayer = Nothing
            , neurons = [Neuron.new [] learningRate activationFn {- activationFn' -} | _ <- [1..numOfNeurons]]
            , activationFunctionDerivative = activationFn'
            , outputCache = replicate numOfNeurons 0.0
            }, rg)
new (Just prevLayer, rg) numOfNeurons learningRate activationFn activationFn' =
    (Layer { previousLayer = (Just prevLayer)
            , neurons = neurns
            , activationFunctionDerivative = activationFn'
            , outputCache = replicate numOfNeurons 0.0
            }, rg')
    where (neurns, rg') = foldl
            (\(ns, rg'') _ -> let (weights,rg''') = randomWeights rg'' (length (neurons prevLayer))
                in ((Neuron.new weights learningRate activationFn {- activationFn' -}):ns,rg'''))
            ([],rg)
            [1..numOfNeurons]

randomWeights :: RandomGen rg => rg -> Int -> ([Double], rg)
randomWeights rg n = foldl (\(ws, rg') _ -> let (w, rg'') = random rg' in (w:ws, rg'')) ([], rg) [1..n]

outputs :: Layer -> [Double] -> (Layer, [Double])
outputs layer inputs = (layer { neurons = updatedNeurons, outputCache = cache }, cache)
    where (updatedNeurons, cache) = case previousLayer layer of
            Nothing -> (neurons layer, inputs)
            Just prevLayer -> unzip $ map (Neuron.output inputs) (neurons layer)
 

-- in contrast to the Python/Java/Rust version, does not update a delta field of the neurons
deltasForOutputLayer :: Layer -> [Double] -> [Double]
deltasForOutputLayer layer expected = 
    zipWith (*)
    (zipWith (-) expected (outputCache layer))
    (map (\n -> (Neuron.activationFunction n) (Neuron.outputCache n)) (neurons layer))

-- should not be called on the output layer
-- in contrast to the Python/Java/Rust version, does not update a delta field of the neurons
deltasForHiddenLayer :: Layer -> Layer -> [Double]-> [Double]
deltasForHiddenLayer layer nextLayer nextLayerDeltas = zipWith
    (\n i -> (activationFunctionDerivative layer) (Neuron.outputCache n)
           * (dotProduct nextLayerDeltas (map (\n' -> Neuron.weights n' !! i) (neurons nextLayer))))
    (neurons layer) [0..]
