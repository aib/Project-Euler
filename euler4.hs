import Data.Digits

import Common

main = print $
       maximum . filter (palindrome . digits 10) $ [a*b | a <- [100..999], b <- [100..999]]
