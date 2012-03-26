import Control.Arrow
import Data.List
import Data.Ord

-- More accurately called a hailstone sequence, but meh...
collatzSeq :: Integer -> [Integer]
collatzSeq 1 = [1]
collatzSeq n = n : collatzSeq nextN
  where
    nextN = if (odd n) then 3*n+1 else n `quot` 2

main = print $
       maximumBy (comparing snd) $ map (id &&& length . collatzSeq) [1..999999]
