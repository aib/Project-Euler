import Data.Digits
import Data.List
import Data.Maybe
import Math.NumberTheory.Primes

import Common

sequenceTriplets ns =
    catMaybes $ 
    do yi <- enumFromTo 1 (length ns - 1)
       xi <- enumFromTo 0 (yi - 1)
       let x = ns !! xi
           y = ns !! yi
           z = y + (y - x) in
         return $ elemIndex z ns >>= (\zi -> Just (x, y, ns !! zi))
        
fourDigitPrimes = dropWhile (<1000) . takeWhile(<10000) $ primes

permutedTriplet (x,y,z) = y `elem` xp && z `elem` xp
  where
    xp = map (unDigits 10) (permutations (digits 10 x))

main = do
    print $ filter permutedTriplet (sequenceTriplets fourDigitPrimes)
    
