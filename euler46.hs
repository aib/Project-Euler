import Data.List.Ordered
import Data.Ord
import Math.NumberTheory.Primes

p_sq = mergeAllBy (comparing (\(p, sq) -> p + sq)) [[(p, sq) | p <- primes] | sq <- (map (\x -> 2 * x^2) [0..])]

main = print $
    head $ [3,5..] `minus` (map (\(p, sq) -> p + sq) p_sq)
