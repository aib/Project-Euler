import Data.Digits

main = print $
    (sum . digits 10) $ product [1..100]
