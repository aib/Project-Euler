import Data.Function
import Data.List

targets :: [a -> a -> a] -> [a] -> [a]
targets ops nums = do
--  opOrder <- concatMap permutations $ multicombinations (length nums - 1) ops
    opOrder <- sequence . replicate (length nums - 1) $ ops
    numOrder <- permutations nums
    return $ target opOrder numOrder
  where
    target ops nums = foldr ($) (last nums) $ zipWith ($) ops (init nums)

ops = [(+),(*),(-),flip (-),safeDiv,flip safeDiv]
  where
    safeDiv x y = if y == 0
                  then 0
                  else x / y

main = do
    print $ maximumBy (compare `on` snd) $ map targetLength allForms 
  where
    allForms = do d <- [3..9]
                  c <- [2..d - 1]
                  b <- [1..c - 1]
                  a <- [0..b - 1]
                  return ((a,b,c,d), sort . nub . filter (>0) . filter isWhole $ targets ops [a,b,c,d])
    isWhole x = (fromInteger . truncate) x == x
    targetLength (nums, targets) = (nums, length . takeWhile id . zipWith (==) [1..] $ targets)