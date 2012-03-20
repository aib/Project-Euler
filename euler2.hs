import Data.List

fib = unfoldr (\(x,y) -> Just (x, (y,x+y))) (0, 1)

main = print $ sum $ filter even $ takeWhile (<= 4000000) fib
