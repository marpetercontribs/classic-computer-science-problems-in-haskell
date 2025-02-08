{- CalculatePi.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 1
   Copyright 2025 Markus Peter

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

module Main where

{- uncomment the following if you want to measure the time it takes to calculate pi -}
-- import Control.DeepSeq
-- import Data.Time ( diffUTCTime, getCurrentTime )

-- "loop-like" (tail-recursive) version close to the Python/Java version
calculatePi :: Int -> Double
calculatePi n = 4.0 * loop 0.0 1 1 n where
   loop sum denominator sign 0 = sum
   loop sum denominator sign n = loop (sum + sign * (1.0 / fromIntegral denominator)) (denominator + 2) (-sign) (n - 1) 

-- generator version using an infinite list
calculatePi2 :: Int -> Double
calculatePi2 n = 4.0 * sum (take n (denominatorGenerator 1.0 1.0)) where
    denominatorGenerator :: Double -> Double -> [Double]
    denominatorGenerator denominator sign = (sign/denominator):(denominatorGenerator (denominator + 2.0) (-sign))


main :: IO ()
main = do
{- comment the following lines if you want to measure the time it takes to calculate pi -
   else the second call to calculatePi2 will have "memoized" most of its work -}
    let pi = calculatePi 10000000
    putStrLn ("pi ~= " ++ show pi)
    let pi = calculatePi2 10000000
    putStrLn ("pi ~= " ++ show pi)
{- uncomment the following if you want to measure the time it takes to calculate pi -}
   --  startTime <- getCurrentTime
   --  let pi = calculatePi 10000000
   --  endTime <- deepseq pi getCurrentTime -- force evaluation of pi before measuring the end time, else pi might only be calculated in the next line
   --  putStrLn ("pi ~= " ++ show pi ++ " (calculated in " ++ show (diffUTCTime endTime startTime) ++ " seconds)")
   --  startTime <- getCurrentTime
   --  let pi = calculatePi2 10000000
   --  endTime <- deepseq pi getCurrentTime -- force evaluation of pi before measuring the end time, else pi might only be calculated in the next line
   --  putStrLn ("pi ~= " ++ show pi ++ " (calculated in " ++ show (diffUTCTime endTime startTime) ++ " seconds)")