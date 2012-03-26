module Common
    (fib, fibI, palindrome)
  where

import Data.List

-- Infinite recursive list of the Fibonacci sequence
fib = unfoldr (\(x,y) -> Just (x, (y,x+y))) (0, 1)

-- Fibonacci list, paired with indices
fibI = unfoldr (\(n,x,y) -> Just ((n,x), (n+1,y,x+y))) (0, 0, 1)

-- Palindrome predicate for lists
palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome (x : []) = True
palindrome (x : xs) = (x == last xs) && palindrome (take ((length xs) - 1) xs)
