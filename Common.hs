{-# LANGUAGE TupleSections #-}

module Common
    (ordPermutations, combinations, multicombinations, fib, fibI, palindrome, consecElems, rotations, countElems, pandigital, fixedPointList, fixedPoint)
  where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Ordered permutations
ordPermutations :: [a] -> [[a]]
ordPermutations [] = [[]]
ordPermutations xs = concatMap (doNthPos xs) [0..length xs - 1]
  where
    doNthPos xs n = map ((nthHead xs n) :) (ordPermutations (nthTail xs n))
    nthHead xs n = xs !! n
    nthTail xs n = take n xs ++ drop (n + 1) xs

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
consecElems n list = zipWith (++) (consecElems (n-1) list) (consecElems 1 (drop (n-1) list))

-- Rotations of a list (abc -> abc, bca, cab)
rotations xs = take (length xs) $ iterate rot1 xs
  where
    rot1 (x:xs) = xs ++ [x]

-- Count of all elements in a list
countElems :: Ord a => [a] -> [(a, Int)]
countElems = Map.toList . Map.fromListWith (+) . map (,1)

-- Pandigital numbers
pandigital :: Int -> [Integer] -> Bool
pandigital d n = (length n == d) && (Set.fromList n == Set.fromList [1..toInteger d])

-- Iterate a function until a fixed point is found (f(x) = x)
fixedPointList :: Eq a => (a -> a) -> a -> [a]
fixedPointList f x = x : unfoldr (iterateIfDifferent f) (x, f x)
  where
    iterateIfDifferent f (x, x') = if (x == x') then Nothing else Just (x', (x', f x'))

-- Return the fixed point of a function
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint = ((last .) .) fixedPointList
