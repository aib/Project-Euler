module Common
    (combinations, multicombinations, fib, fibI, palindrome, consecElems)
  where

import Data.List

-- k-Combinations
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k xxs@(x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- k-Multicombinations (aka multisets or combinations with repetitions)
multicombinations :: Int -> [a] -> [[a]]
multicombinations 0 _ = [[]]
multicombinations _ [] = []
multicombinations k xxs@(x:xs) = map (x:) (multicombinations (k-1) xxs) ++ multicombinations k xs

-- Infinite recursive list of the Fibonacci sequence
fib = unfoldr (\(x,y) -> Just (x, (y,x+y))) (0, 1)

-- Fibonacci list, paired with indices
fibI = unfoldr (\(n,x,y) -> Just ((n,x), (n+1,y,x+y))) (0, 0, 1)

-- Palindrome predicate for lists
palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome (x : []) = True
palindrome (x : xs) = (x == last xs) && palindrome (take ((length xs) - 1) xs)

-- The list of n consecutive elements in a list
consecElems :: Int -> [a] -> [[a]]
consecElems 1 list = map return list
consecElems n list = zipWith (++) (consecElems (n-1) list) (map return (drop (n-1) list))
