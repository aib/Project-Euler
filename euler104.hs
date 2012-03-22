import Data.Digits
import Data.Set as Set

import Common

pandigital = (== 9) . length . toList . (Set.filter (/= 0)) . fromList 

pandigitalFirst = pandigital . (take 9) . (digits 10)
pandigitalLast = pandigital . (take 9) . reverse . (digits 10)

main =
    print $ head $ Prelude.filter (\(x,y) -> pandigitalFirst y && pandigitalLast y) fibI
