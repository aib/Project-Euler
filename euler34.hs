import Data.List
import Data.Digits

-- Factorions have an upper limit at 7 digits (and another, smaller one which we ignore) 
main = print $ filter (\x -> (sum . (map fac) . digits 10) x == x) [1..9999999]
  where
    fac = foldl' (*) 1 . enumFromTo 1
