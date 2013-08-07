import Common
import Control.Applicative
import qualified Data.Map as Map

peteDice = replicate 9 [1,2,3,4]
colinDice = replicate 6 [1,2,3,4,5,6]

diceOutcomes :: (Num a, Ord a) => [[a]] -> [(a, Int)]
diceOutcomes ds = countElems sums
  where
    sums = map sum (sequence ds)

compareOutcomes :: (Ord a, Fractional b) => [(a, Int)] -> [(a, Int)] -> [(Ordering, b)]
compareOutcomes xs ys = map (\(o,cnt) -> (o,(fromIntegral cnt) / (fromIntegral totalThrows))) allOutcomes
  where
    cmp (x,xc) (y,yc) = (compare x y, xc*yc)
    allOutcomes = Map.toList . Map.fromListWith (+) $ liftA2 cmp xs ys
    numThrows = sum . (map snd)
    totalThrows = (numThrows xs) * (numThrows ys)

main = do
          print (compareOutcomes (diceOutcomes peteDice) (diceOutcomes colinDice))
