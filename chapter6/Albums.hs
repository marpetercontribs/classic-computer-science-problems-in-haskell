{- Albums.hs
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

module Main (main) where

import KMeans
import System.Random(getStdGen)

data Album = Album {
      name :: String 
    , year :: Int
    , duration :: Double
    , tracks :: Int
    } deriving (Eq)

instance Clusterable Album where
    fromClusterable a = DataPoint {
          original = a
        , coordinates = [duration a, fromIntegral (tracks a)]
        , numDimensions = 2
    }

instance Show Album where
    show a = "(" ++ show (name a) ++ "," ++ show (year a) ++ ")"

main :: IO ()
main = do
    rg <- getStdGen
    let albums = [
         Album "Got to Be There" 1972 35.45 10,
         Album "Ben" 1972 31.31 10,
         Album "Music & Me" 1973 32.09 10,
         Album "Forever, Michael" 1975 33.36 10,
         Album "Off the Wall" 1979 42.28 10,
         Album "Thriller" 1982 42.19 9,
         Album "Bad" 1987 48.16 10,
         Album "Dangerous" 1991 77.03 14,
         Album "HIStory: Past, Present and Future, Book I" 1995 148.58 30,
         Album "Invincible" 2001 77.05 16]
    let km = newKMeans 2 albums rg
    let clusters = runKMeans km 100
    mapM_ (\(cluster, idx) -> do
        putStrLn $ "Cluster " ++ show idx ++ " (Average Duration: " ++ 
            show ( coordinates (centroid cluster) !! 0) ++ ", Average Tracks: " ++
            show (coordinates (centroid cluster) !! 1) ++ "):"
        mapM_ (putStrLn . ("  " ++) . show . original) (dataPoints cluster))
            (zip clusters [0..])