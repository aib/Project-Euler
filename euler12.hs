import Math.NumberTheory.ArithmeticFunctions

triangle n = sum [1..n]

main = print $
    head . filter (\x -> (length . divisors) (x :: Integer) > 500) . map triangle $ [1..]
