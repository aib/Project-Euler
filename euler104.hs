import Data.Digits

import Common

pandigitalFirst = (pandigital 9) . (take 9) . (digits 10)
pandigitalLast = (pandigital 9) . (take 9) . reverse . (digits 10)

main =
    print $ head $ Prelude.filter (\(x,y) -> pandigitalFirst y && pandigitalLast y) fibI
