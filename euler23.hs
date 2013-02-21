import Math.NumberTheory.Primes.Factorisation
import qualified Data.Set as S

abundant n = properDivSum n > n
  where
    properDivSum x = sum . S.toList . (S.delete x) . divisors $ x

sumOfTwo xs n = or [(n - x) `elem` xs | x <- xs]

main = print $
    sum $ filter (sumOfTwo abundants) [1..28123]
  where
    abundants = filter abundant [1..28123]
