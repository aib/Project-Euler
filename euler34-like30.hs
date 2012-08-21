import Data.Digits

fact n = product [1..n]

-- Find the upper limit as in problem #30
upperLimit = (fact 9 *) $ last $ takeWhile (\n -> fact 9 * n >= 10^(n-1)) [1..]

curious n = (sum . map fact . digits 10) n == n

main = print $ (sum . filter curious) [3..upperLimit]
