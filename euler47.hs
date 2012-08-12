import Data.List
import Math.NumberTheory.Primes.Factorisation
import Common

consecFactors n f = find (all ((== f) . factorCount)) $ consecElems n [1..]
  where
    factorCount = length . factorise

main = print $
    consecFactors 4 4
