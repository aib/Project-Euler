import Data.Function
import Data.List
import Data.List.Split

main = do
    numLines <- readFile "euler99-base_exp.txt"
    nums <- return . zipWith (,) [1..] . map readLine . lines $ numLines
    print $ {-fst $-} maximumBy (compare' `on` snd) nums
  where
    readLine = (\[b,e] -> ((read::String->Integer) b, (read::String->Integer) e)) . wordsBy (== ',')

compare' :: (Integer,Integer) -> (Integer,Integer) -> Ordering
compare' (b1,e1) (b2,e2) = compare (e1' * log b1') (e2' * log b2')
  where
    b1' = fromInteger b1
    b2' = fromInteger b2
    e1' = fromInteger e1
    e2' = fromInteger e2
