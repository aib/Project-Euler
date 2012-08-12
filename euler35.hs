import Data.Digits
import Math.NumberTheory.Primes
import Common

circularPrime = and . (map isPrime) . rots
  where
    rots = map (unDigits 10) . rotations . (digits 10)

main = print $
    length $ filter circularPrime [1..999999]
    
