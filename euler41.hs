import Data.List

import Data.Digits
import Math.NumberTheory.Primes.Testing

pandigitals = concatMap (\n -> map (unDigits 10) (permutations [1..n])) [1..9]

main = print $
  last . sort . (filter isPrime) $ pandigitals
