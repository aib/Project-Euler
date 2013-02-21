import Math.NumberTheory.Primes.Factorisation
import qualified Data.Set as S

abundant n = properDivSum n > n
  where
    properDivSum x = sum . S.toList . (S.delete x) . divisors $ x

abundants = filter abundant [1..28123]

removePairSumsLessThan maxValue terms ns = pipe (S.fromList ns) (map S.delete sums')
  where
    sums = [n1 + n2 | n1 <- terms, n2 <- terms]
    sums' = S.toList . S.fromList $ filter (<= maxValue) sums
    pipe = foldl (flip ($))

main = print $
    sum $ S.toList $ removePairSumsLessThan 28123 abundants [1..28123]
