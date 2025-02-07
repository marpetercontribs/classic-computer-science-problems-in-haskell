{- UnbreakableEncryption.hs
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

import Data.Bits (xor)
import Data.Char (ord, chr)
import Data.Word (Word8)
import System.Random (randoms, mkStdGen)

randomKey :: Int -> [Word8]
randomKey length = take length (randoms (mkStdGen 12345)) -- 12345 is the seed

encrypt :: String -> ([Word8], [Word8])
encrypt string = (encrypted, key) where
   key = randomKey (length string)
   encrypted = zipWith xor key (map (fromIntegral . ord) string)
-- or a bit more explicit:
{- encrypt string = (encrypted, key) where
    key = randomKey (length string)
    -- convert the string into a list of bytes
    originalBytes = map (fromIntegral . ord) string
    -- create a list by xoring each byte of the key with the corresponding byte of the original message
    encrypted = zipWith xor key originalBytes -}

decrypt :: [Word8] -> [Word8] -> String
decrypt key1 key2 = map (chr . fromIntegral) (zipWith xor key1 key2)
-- or a bit more explicit:
{- decrypt key1 key2 = toString decrypted
    where
        -- create a list by xoring each byte of the key with the corresponding byte of the encrypted message
        decrypted = zipWith xor key1 key2
        -- convert the list be converting each byte to a character
        toString = map (chr . fromIntegral)  -}

main :: IO ()
main = do
    let (key1, key2) = encrypt "One Time Pad!"
    let result = decrypt key1 key2
    putStrLn ("Decrypted: " ++ result)