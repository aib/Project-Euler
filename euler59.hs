import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Ord
import qualified Data.Set as Set

import Common

import Data.List.Split

keys = sequence ((take 3 . repeat) ['a'..'z'])

decrypt key ciphertext = zipWith xor ((concat . repeat) key) ciphertext
decodeNums nums key = map chr (decrypt (map ord key) nums)

score wordList text = length . filter (`Set.member` wordList) $ (words text)

main = do
    wordList <- (liftM Set.fromList) . (liftM lines) . readFile $ "/usr/share/dict/american-english"
    file <- readFile "p059_cipher.txt"
    cipher <- return $ map (read :: String -> Int) . (splitOn ",") $ file
--    mapM_ print $ map (\k -> (k, score wordList (decodeNums cipher k))) keys
--    mapM_ print $ map (\k -> (k, take 100 $ decodeNums cipher k)) keys
    keyScores <- return $ map (\k -> (k, score wordList (decodeNums cipher k))) keys
    bestKey   <- return $ last . sortBy (comparing snd) $ keyScores
--    print $ (decodeNums cipher (fst bestKey))
    print $ sum . map ord $ (decodeNums cipher (fst bestKey))
