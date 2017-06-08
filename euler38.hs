import Data.Digits
import Data.List

import Common

panmul x ys = concatMap (digits 10) . map (x*) $ ys

muls = map (\x -> [1..x]) [2..9]

panmuls x = map (panmul x) muls

main = print $ concatMap show $
  (last . sort) $ (filter pandigital) (concat feasibles)
    where
  feasibles = takeWhile (any (\x -> length x < 10)) $ map panmuls [1..]
