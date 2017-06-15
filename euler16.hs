import Data.Digits

main = print $
    (sum . digits 10) $ 2^1000
