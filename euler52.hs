import Data.Digits
import Data.List

main = do
    print $ head $ filter (\x -> all (sameDigits x) (multiples x)) [1..]
  where
    multiples x = map (x*) [2..6]
    sameDigits x y = sortBy compare (digits 10 x) == sortBy compare (digits 10 y)
