import qualified Data.Set as Set
import qualified Data.Map as Map

import Common

import Control.Arrow

type Coin = Int
type Coins = Map.Map Coin Int

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p)
coins = [1, 2, 5, 10, 20, 50, 100, 200] :: [Coin]

addCoin :: Coin -> Coins -> Coins
addCoin coin cs = Map.insertWith (+) coin 1 cs

coinSum :: Coins -> Int
coinSum = Map.foldlWithKey (\s k v -> s + (k * v)) 0

addCoinsWithCheck :: Int -> [Coin] -> Set.Set Coins -> Set.Set Coins
addCoinsWithCheck amount newCoins base = Set.foldl (\a b -> foldl (\bcs c -> Set.insert c bcs) a (addAll b)) Set.empty base
  where
    addWithCheck n cs c = let cs' = addCoin c cs in if coinSum cs' <= n then cs' else Map.empty
    addAll cs = map (addWithCheck amount cs) newCoins

findCombinations amount =
    length $ Set.filter (\cs -> coinSum cs == amount) allSums
  where
    allSums = (fixedPoint (addCoinsWithCheck amount coins)) (Set.singleton Map.empty)

main = print $
    map (id &&& findCombinations) [1..]
