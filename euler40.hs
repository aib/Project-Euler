import Data.Char

main = print $ product $ map (digitToInt . (digitList !!) . subtract 1) [1,10,100,1000,10000,100000,1000000]
  where
    digitList = foldr (++) "" $ map show [1..]
