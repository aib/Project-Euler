import Data.Function
import Data.List

import Common

type Coin = Int

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p)
coins = [1, 2, 5, 10, 20, 50, 100, 200]

addCoins :: [Coin] -> [Coin] -> [[Coin]]
addCoins newCoins base = base : ((:) <$> newCoins <*> [base])

addCoinsAndCheck :: Int -> [Coin] -> [[Coin]] -> [[Coin]]
addCoinsAndCheck coinSum newCoins bases = nub $ map sort $ filter (\cs -> (sum cs) <= coinSum) $ concatMap (addCoins newCoins) bases

main = print $
    length $ filter (\cs -> sum cs == 200) allSums
  where
    allSums = (fixedPoint (addCoinsAndCheck 200 coins)) [[]]
