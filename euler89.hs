import Control.Monad
import Data.List
import Data.Ord

import Roman

optirom = minimumBy (comparing length) . romans True

optidiff rstr = length rstr - length (optirom (fromRoman rstr))

main = do
    rnums <- (liftM lines . readFile) "euler89-roman.txt"
    print $ (sum . map optidiff) rnums
