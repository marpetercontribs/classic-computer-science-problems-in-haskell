{- Governors.hs
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

data Governor = Governor {
      longitude :: Double
    , age :: Int
    , state :: String
    } deriving (Eq)

instance Clusterable Governor where
    fromClusterable g = DataPoint {
          original = g
        , coordinates = [longitude g, fromIntegral (age g)]
        , numDimensions = 2
    }

instance Show Governor where
    show g = state g ++ " (longitude: " ++ show (longitude g) ++ ", age: " ++ show (age g) ++ ")"

main :: IO ()
main = do
    rg <- getStdGen
    let governors = [
         Governor (-86.79113::Double)   (72::Int)  "Alabama",
         Governor (-152.404419::Double) (66::Int)  "Alaska",
         Governor (-111.431221::Double) (53::Int)  "Arizona",
         Governor (-92.373123::Double)  (66::Int)  "Arkansas",
         Governor (-119.681564::Double) (79::Int)  "California",
         Governor (-105.311104::Double) (65::Int)  "Colorado",
         Governor (-72.755371::Double)  (61::Int)  "Connecticut",
         Governor (-75.507141::Double)  (61::Int)  "Delaware",
         Governor (-81.686783::Double)  (64::Int)  "Florida",
         Governor (-83.643074::Double)  (74::Int)  "Georgia",
         Governor (-157.498337::Double) (60::Int)  "Hawaii",
         Governor (-114.478828::Double) (75::Int)  "Idaho",
         Governor (-88.986137::Double)  (60::Int)  "Illinois",
         Governor (-86.258278::Double)  (49::Int)  "Indiana",
         Governor (-93.210526::Double)  (57::Int)  "Iowa",
         Governor (-96.726486::Double)  (60::Int)  "Kansas",
         Governor (-84.670067::Double)  (50::Int)  "Kentucky",
         Governor (-91.867805::Double)  (50::Int)  "Louisiana",
         Governor (-69.381927::Double)  (68::Int)  "Maine",
         Governor (-76.802101::Double)  (61::Int)  "Maryland",
         Governor (-71.530106::Double)  (60::Int)  "Massachusetts",
         Governor (-84.536095::Double)  (58::Int)  "Michigan",
         Governor (-93.900192::Double)  (70::Int)  "Minnesota",
         Governor (-89.678696::Double)  (62::Int)  "Mississippi",
         Governor (-92.288368::Double)  (43::Int)  "Missouri",
         Governor (-110.454353::Double) (51::Int)  "Montana",
         Governor (-98.268082::Double)  (52::Int)  "Nebraska",
         Governor (-117.055374::Double) (53::Int)  "Nevada",
         Governor (-71.563896::Double)  (42::Int)  "New Hampshire",
         Governor (-74.521011::Double)  (54::Int)  "New Jersey",
         Governor (-106.248482::Double) (57::Int)  "New Mexico",
         Governor (-74.948051::Double)  (59::Int)  "New York",
         Governor (-79.806419::Double)  (60::Int)  "North Carolina",
         Governor (-99.784012::Double)  (60::Int)  "North Dakota",
         Governor (-82.764915::Double)  (65::Int)  "Ohio",
         Governor (-96.928917::Double)  (62::Int)  "Oklahoma",
         Governor (-122.070938::Double) (56::Int)  "Oregon",
         Governor (-77.209755::Double)  (68::Int)  "Pennsylvania",
         Governor (-71.51178::Double)   (46::Int)  "Rhode Island",
         Governor (-80.945007::Double)  (70::Int)  "South Carolina",
         Governor (-99.438828::Double)  (64::Int)  "South Dakota",
         Governor (-86.692345::Double)  (58::Int)  "Tennessee",
         Governor (-97.563461::Double)  (59::Int)  "Texas",
         Governor (-111.862434::Double) (70::Int)  "Utah",
         Governor (-72.710686::Double)  (58::Int)  "Vermont",
         Governor (-78.169968::Double)  (60::Int)  "Virginia",
         Governor (-121.490494::Double) (66::Int)  "Washington",
         Governor (-80.954453::Double)  (66::Int)  "West Virginia",
         Governor (-89.616508::Double)  (49::Int)  "Wisconsin",
         Governor (-107.30249::Double)  (55::Int)  "Wyoming"]
    let km = newKMeans 2 governors rg
    let clusters = runKMeans km 100
    mapM_ (\(cluster, idx) -> do
        putStrLn $ "Cluster " ++ show idx ++ ":"
        mapM_ (putStrLn . ("  " ++) . show . original) (dataPoints cluster)) (zip clusters [0..])