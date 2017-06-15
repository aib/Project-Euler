import Math.NumberTheory.Primes

main = print $
    sum $ takeWhile (< 2000000) primes
