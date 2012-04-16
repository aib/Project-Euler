import Math.NumberTheory.Primes.Testing
import Control.Arrow
import Data.List
import Data.Ord

quads = [((a,b),(\n -> n*n + a*n + b)) | a <- [-999..999], b <- [-999..999]]

consecPrimes f = length . (takeWhile isPrime) $ (map f [0..])

main = print $
    (\((a,b),f) -> (a*b)) $
    maximumBy (comparing snd) quadMap
  where
    quadMap = map (fst &&& (consecPrimes . snd)) quads
