import Data.Digits

import Common

lychrel = null . dropWhile (not . palindrome . digits 10) . take 50 . drop 1 . (iterate lychrelIt)
  where
    lychrelIt x = x + ((unDigits 10) . reverse . (digits 10) $ x)

main = do
    print $ length . filter (lychrel) $ [1..10000]
