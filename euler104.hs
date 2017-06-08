import Data.Digits

import Common

pandigitalFirst = pandigital . (take 9) . (digits 10)
pandigitalLast = pandigital . (take 9) . reverse . (digits 10)

main =
    print $ head $ Prelude.filter (\(x,y) -> pandigitalFirst y && pandigitalLast y) fibI
