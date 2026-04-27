{- NeuralNetwork.hs
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

module NeuralNetwork (
     Network(..)
   , new
   , train
   , validate
) where

import qualified Layer as Layer
import qualified Neuron as Neuron
import System.Random(RandomGen)

data Network = Network { layers :: [Layer.Layer] }

new :: RandomGen rg => rg -> [Int] -> Double -> 
   (Double -> Double) -> (Double -> Double) -> (Network, rg)
new rg layerStructure learningRate activationFn activationFn'
      | length layerStructure < 3 = error "Network must have at least 3 layers (input, hidden and output)"
      | (inputSize:sizes') <- layerStructure = 
         let (inputLayer, rg') = Layer.new (Nothing, rg) inputSize learningRate
                                  activationFn activationFn'
             (ls, rg'') = foldl (\(ls'@(prev:_), gen) size -> 
                  let (layer, gen') = Layer.new (Just prev, gen) size learningRate
                                      activationFn activationFn'
                   in (layer:ls', gen'))
               ([inputLayer], rg') sizes'
          in (Network { layers = reverse ls }, rg'')
          
-- Returns the output of the network for a given input
-- and the new network with updated caches, to remain pure
outputs :: Network -> [Double] -> (Network, [Double])
outputs network inputs = ( Network { layers = reverse updatedLayers }, outputs )
  where
    (updatedLayers, outputs) = foldl (\(ls, outs) layer -> 
      let (layer', outs') = Layer.outputs layer outs
       in (layer':ls, outs')) ([], inputs) (layers network)

-- helper function to get the first element of a triple, used in backpropagate to get the list of deltas
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

 -- returns the list of deltas for each layer only, does not "update" the network / its layers / neurons!
backpropagate :: Network -> [Double] -> [[Double]]
backpropagate network expected = fst3 $ foldl
   (\(ds,nl,nlds) layer -> let lds = Layer.deltasForHiddenLayer layer nl nlds
                            in (lds:ds,layer,lds))
   ([outputLayerDeltas],outputLayer,outputLayerDeltas) restLayers
   where (outputLayer:restLayers) = reverse (drop 1 (layers network)) -- ignore input layer, start with output layer and work backwards
         outputLayerDeltas = Layer.deltasForOutputLayer outputLayer expected

updateWeights :: Network -> [[Double]] -> Network
updateWeights network deltas = Network { layers = fst $
   foldl (\(ls',prev) (l, d) -> (ls' ++ [updateLayerWeights l prev d],l))
   ([inputLayer],inputLayer) (zip ls deltas) }
   where (inputLayer:ls) = layers network
         updateLayerWeights l prev ds = l { Layer.neurons = ns' }
            where ns' = zipWith (updateNeuronWeights prev) (Layer.neurons l) ds
         updateNeuronWeights prev n d = n { Neuron.weights = 
            map (\(w, inp) -> w + Neuron.learningRate n * d * inp)
            (zip (Neuron.weights n) prevOutputs) }
            where prevOutputs = Layer.outputCache prev

train :: Network -> [[Double]] ->  [[Double]] -> Network
train network inputs expected = foldl
   (\net (inp, exp) -> let (net', _) = outputs net inp
                           deltas = backpropagate net' exp
                        in updateWeights net' deltas)
   network (zip inputs expected)

validate :: Eq t => Network -> [[Double]] -> [t] -> ([Double] -> t) -> (Int, Int, Double)
validate network inputs expecteds interpretOutput = (correct, length inputs,
   fromIntegral correct / fromIntegral (length inputs))
   where correct = foldl (\correct' (inp, exp) ->
               if interpretOutput (snd (outputs network inp)) == exp
                  then correct' + 1
                  else correct')
               0 (zip inputs expecteds)

