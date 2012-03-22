import Data.Digits

import Common

main = print $ head $ dropWhile (\(n,x) -> (length $ digits 10 x) < 1000) fibI
