import Math.NumberTheory.Primes.Factorisation
import qualified Data.Set as S

abundant n = properDivSum n > n
  where
    properDivSum x = sum . S.toList . (S.delete x) . divisors $ x

abundants = filter abundant [1..28123]

removePairSums terms ns = pipe (S.fromList ns) (map S.delete sums')
  where
    sums = [n1 + n2 | n1 <- terms, n2 <- terms]
    sums' = S.toList . S.fromList $ sums
    pipe = foldl (flip ($))

main = print $
    sum $ S.toList $ removePairSums abundants [1..28123]
