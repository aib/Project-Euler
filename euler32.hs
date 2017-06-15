import Control.Arrow
import Data.Digits
import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Primes.Factorisation
import qualified Data.Set as Set

import Common

mulTuples num = Set.map (id &&& div num) $ divisors (num :: Integer)

unusual = any (pandigital 9) . mulDigits
  where
    mulDigits num = map (\(a, b) -> digits 10 a ++ digits 10 b ++ digits 10 num) . Set.toList . mulTuples $ num

main = print $
    sum . filter unusual $ [1..9999]
