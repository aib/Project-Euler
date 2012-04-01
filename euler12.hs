import Math.NumberTheory.Primes.Factorisation

triangle n = sum [1..n]

main = print $
    head . filter (\x -> divisorCount x > 500) . map triangle $ [1..]
