import Control.Arrow
import Data.List
import Data.Ord

-- a < b < c
rightTriangles p = [(a,b,c) | a <- [1 .. p `div` 3], b <- [a+1 .. (p-a) `div` 2], c <- [(p-(a+b))], a*a + b*b == c*c]

main = print $
    maximumBy (comparing snd) $ map (id &&& length . rightTriangles) [1..1000]