import Data.Function
import Data.List
import Data.List.Split
import Common

import Debug.Trace

main = do
    numLines <- readFile "euler99-base_exp.txt"
    nums <- return . (map readLine) . lines $ numLines 
    print $ getLargest nums
  where
    readLine = (\[b,e] -> ((read::String->Integer) b, (read::String->Int) e)) . wordsBy (== ',')

getLargest :: [(Integer,Int)] -> (Integer,Int)
getLargest nums = head $ until ((==1) . length) reduce nums  

augmentWithGcds :: [(Integer,Int)] -> [(Integer,(Integer,Int),(Integer,Int))]
augmentWithGcds = map (\[(e1,b1),(e2,b2)] -> (gcd e1 e2,(e1,b1),(e2,b2))) . combinations 2

reduce :: [(Integer,Int)] -> [(Integer,Int)]
reduce ns = trace (show largestGcd ++ " " ++ show (length ns)) $ delete (smallerInPair largestGcd) ns
  where
    largestGcd = maximumBy (compare `on` (\(x,_,_) -> x)) (augmentWithGcds ns)
    smallerInPair (gcd,(b1,e1),(b2,e2)) = if ((b1 `div` gcd) ^ e1) < ((b2 `div` gcd) ^ e2) then (b1,e1) else (b2,e2)
  