{- Missionaries.hs
   Adapted From Classic Computer Science Problems in Python/Java Chapter 2
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

import GenericSearch(bfs, nodeToPath)

maxNum = 3 :: Int

data MCState = MCState {
    westBankMissionaries :: Int,
    westBankCannibals    :: Int,
    eastBankMissionaries :: Int,
    eastBankCannibals    :: Int,
    boatOnWestBank       :: Bool } deriving (Eq)
newMCState missionaries cannibals boat = 
    MCState missionaries cannibals (maxNum-missionaries) (maxNum-cannibals) boat
instance Show MCState where
    show (MCState wm wc em ec boat) = 
        "On the west bank there are " ++ show wm ++ " missionaries and " ++ show wc ++ " cannibals.\n" ++
        "On the east bank there are " ++ show em ++ " missionaries and " ++ show ec ++ " cannibals.\n" ++
        "The boat is on the " ++ (if boat then "west" else "east") ++ " bank.\n"

goalTest:: MCState -> Bool
goalTest (MCState wm wc em ec boat) = 
    (isLegal (MCState wm wc em ec boat)) &&  (wm == 0 && wc == 0) && (em == maxNum && ec == maxNum)
isLegal:: MCState -> Bool
isLegal (MCState wm wc em ec boat) = not ( wm > 0 && wm < wc) && not ( em > 0 && em < ec )

successors:: MCState -> [MCState]
successors (MCState wm wc em ec True) = filter isLegal (
    [ newMCState (wm-2) wc False | wm > 1 ] ++
    [ newMCState (wm-1) wc False | wm > 0 ] ++
    [ newMCState wm (wc-2) False | wc > 1 ] ++
    [ newMCState wm (wc-1) False | wc > 0 ] ++
    [ newMCState (wm-1) (wc-1) False | wm > 0 && wc > 0 ] )
successors (MCState wm wc em ec False) = filter isLegal (
    [ newMCState (wm+1) wc True | em > 0 ] ++
    [ newMCState wm (wc+1) True | ec > 0 ] ++
    [ newMCState (wm+2) wc True | em > 1 ] ++
    [ newMCState wm (wc+2) True | ec > 1 ] ++
    [ newMCState (wm+1) (wc+1) True | em > 0 && ec > 0 ] )

showPath:: [MCState] -> String
showPath [] = ""
showPath [x] = show x
showPath (o:x:xs) = show o ++ "\n" ++ (showMove o x) ++ showPath (x:xs) where
    showMove o x =
        if (boatOnWestBank x) then
            show (eastBankMissionaries o - eastBankMissionaries x) ++ " missionaries and "
            ++ show (eastBankCannibals o - eastBankCannibals x)
            ++ " cannibals moved from the east bank to the west bank.\n"
        else show (westBankMissionaries o - westBankMissionaries x) ++ " missionaries and "
            ++ show (westBankCannibals o - westBankCannibals x)
            ++ " cannibals moved from the west bank to the east bank.\n"


main :: IO ()
main = do
    let start = newMCState maxNum maxNum True
    let solution = bfs start goalTest successors
    case solution of
        Nothing   -> putStrLn "No solution found."
        Just node -> putStrLn $ showPath (nodeToPath node)