import Data.Digits
import Data.List
import Math.NumberTheory.Primes

lTrunc = map (unDigits 10) . tail . init . tails . digits 10
rTrunc = map (unDigits 10) . init . tail . inits . digits 10

main = print $ sum . take 11 . (filter interesting) . drop 4 $ primes
  where
    interesting p = all isPrime (lTrunc p) && all isPrime (rTrunc p)
