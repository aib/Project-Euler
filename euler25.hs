import Data.List
import Data.Digits

fib = unfoldr (\(n,x,y) -> Just ((n,x), (n+1,y,x+y))) (0, 0, 1)

main = print $ head $ dropWhile (\(n,x) -> (length $ digits 10 x) < 1000) fib
