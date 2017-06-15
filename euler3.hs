import Math.NumberTheory.Primes.Factorisation

main = print $
    (maximum . map fst . factorise) 600851475143
