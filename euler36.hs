import Data.Digits

import Common

main = print $
       sum $ filter (\x -> palindrome (digits 10 x) && palindrome (digits 2 x)) [1..999999]
