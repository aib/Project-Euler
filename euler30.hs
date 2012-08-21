import Data.Digits

{-
  Find the largest number which is small enough to be the sum of the fifth
  powers its digits. For this, we compare n * 9^5 and 10^(n-1), which are,
  respectively, the upper limit for a sum and the lower limit for a number,
  of n digits.
-}
upperLimit = (9^5 *) $ last $ takeWhile (\n -> 9^5*n >= 10^(n-1)) [1..]

sumOfDigitPowers p n = (sum $ map (^p) (digits 10 n)) == n

main = print $ sum $ filter (sumOfDigitPowers 5) [2..upperLimit]
