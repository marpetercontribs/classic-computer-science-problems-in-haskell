{- KMeans.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 6
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

{-# LANGUAGE FlexibleInstances #-}

module KMeans(
      DataPoint(..)
    , Clusterable(..)
    , Cluster(..)
    , KMeans(..)
    , newKMeans
    , runKMeans
    , zscores
) where

import Data.List(transpose)
import System.Random(RandomGen, randomR)
import Debug.Trace(trace)

-- corresponds to the "Into<DataPoint>" trait in my Rust implementation
class (Eq a) => Clusterable a where
    fromClusterable :: a -> DataPoint a

-- DataPoint can be a data structure like in my Rust implementation, so that it can hold data
data (Clusterable p) => DataPoint p = DataPoint {
      original :: p
    , coordinates :: [Double]
    , numDimensions :: Int
}

-- Double must be "Clusterable" so that it can be used as the type for the cluster centroids
instance Clusterable [Double] where
    fromClusterable x = DataPoint {
          original = x
        , coordinates = x
        , numDimensions = length x
    }

data (Clusterable p) => Cluster p = Cluster {
      dataPoints :: [DataPoint p]
    , centroid :: DataPoint [Double]
}

data (Clusterable p) => KMeans p = KMeans {
      points :: [DataPoint p]
    , clusters :: [Cluster p]
    }

distance :: (Clusterable p, Clusterable q) => DataPoint p -> DataPoint q -> Double
distance a b = sqrt . sum $
    zipWith (\x y -> (x - y) ^ 2) (coordinates a) (coordinates b)

newKMeans :: (Clusterable p, RandomGen g) => Int -> [p] -> g -> KMeans p
newKMeans k ps rg = let dps = zScoreNormalize ps in
     KMeans {
          points = dps
        , clusters = fst $ foldl (\(cs,rg') _ ->
            let (c,rg'') = randomPoint dps rg'
            in (Cluster { dataPoints = [], centroid = c }:cs, rg''))
                ([],rg) [1..k]
     }

zScoreNormalize :: (Clusterable p) => [p] -> [DataPoint p]
zScoreNormalize ps = zipWith (\dp zsp -> dp { coordinates = zsp }) dps zsps
    where dps = map fromClusterable ps
          dimensions = case dps of
                            [] -> 0 -- handle empty list case
                            (dp:_) -> numDimensions dp
          zsps = transpose $
            map (\dim -> zscores (dimensionSlice dim dps)) [0..dimensions - 1]

randomPoint :: (Clusterable p, RandomGen g) => [DataPoint p] -> g -> (DataPoint [Double], g)
randomPoint dps rg = case dps of
    [] -> error "Cannot select a random point from an empty list"
    (dp:_) ->
        let (cs, rg''') = foldl (\(coords,rg') dim ->
                let values = dimensionSlice dim dps
                    (c,rg'') = randomR (minimum values, maximum values) rg'
                in (coords ++ [c], rg''))
                ([],rg) [0..numDimensions dp - 1]
        in (fromClusterable cs, rg''')

runKMeans :: (Clusterable p) => KMeans p -> Int -> [Cluster p]
runKMeans kmeans maxIterations = loop 0 kmeans
    where loop iteration kmeans
            | iteration >= maxIterations = clusters kmeans
            | otherwise =
                let nextClusters = (generateCentroids . assignClusters) kmeans
                in  if coordinatesAreEqual (map centroid nextClusters)
                        (map centroid (clusters kmeans))
                    then trace ("Converged after " ++ (show iteration) ++ " iterations")
                        $ clusters kmeans
                    else loop (iteration + 1) kmeans { clusters = nextClusters }

assignClusters :: (Clusterable p) => KMeans p -> [Cluster p]
assignClusters kmeans = foldl (\cs dp -> addToClosestCluster dp cs)
    -- the clusters must be cleared before points are assigned
    (map (\c-> c { dataPoints = [] }) (clusters kmeans))
    (points kmeans)

-- helper "function" to get the maximum double value as a starting point for finding the closest cluster
-- because Haskell has no built-in constant for the maximum double value
maxDouble :: Double
maxDouble = x
  where n = floatDigits x
        b = floatRadix x
        (_, u) = floatRange x
        x = encodeFloat (b^n - 1) (u - n)

closestCluster :: (Clusterable p) => DataPoint p -> [Cluster p] -> Int
closestCluster dp clusters = case clusters of
    [] -> error "No clusters available to assign the data point"
    (c1:_) -> snd $ foldl (\(lowestDistance,closestClusterIdx) (clstr, idx) ->
            let centroidDistance = distance dp (centroid clstr)
            in  if centroidDistance < lowestDistance
                then (centroidDistance, idx)
                else (lowestDistance, closestClusterIdx))
        (maxDouble,0) (zip clusters [0..])

addToClosestCluster :: (Clusterable p) => DataPoint p -> [Cluster p] -> [Cluster p]
addToClosestCluster dp clusters = let closestIdx = closestCluster dp clusters
    in map (\(c,idx) ->
        if idx == closestIdx then c { dataPoints = dp : dataPoints c } else c)
        (zip clusters [0..])
-- alternative implementation using take, ++ and drop:    
        -- take closestIdx clusters ++
        --  [ (clusters !! closestIdx) { dataPoints = dp : dataPoints (clusters !! closestIdx) } ] ++
        --  drop (closestIdx + 1) clusters

generateCentroids :: (Clusterable p) => [Cluster p] -> [Cluster p]
generateCentroids clusters = map (\cluster -> case dataPoints cluster of
        [] -> cluster -- handle empty cluster case
        dps@(dp:_) ->
            let dimensions = numDimensions dp
                newCentroid = fromClusterable $ 
                    map (\dim -> mean (dimensionSlice dim dps)) [0..dimensions - 1]
            in cluster { centroid = newCentroid })
    clusters

coordinatesAreEqual :: [DataPoint [Double]] -> [DataPoint [Double]] -> Bool
coordinatesAreEqual dps1 dps2 = all
    (\(dp1, dp2) -> coordinates dp1 == coordinates dp2)
    (zip dps1 dps2)

dimensionSlice :: (Clusterable p) => Int -> [DataPoint p] -> [Double]
dimensionSlice dim dps = map (\dp -> coordinates dp !! dim) dps

zscores :: [Double] -> [Double]
zscores xs = let m = mean xs
                 s = stdev xs
             in if s == 0
                then replicate (length xs) 0
                else map (\x -> (x - m) / s) xs

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

stdev :: [Double] -> Double
stdev xs = sqrt (sum (map (\x -> (x - m) ^ 2) xs) / fromIntegral (length xs))
    where m = mean xs
