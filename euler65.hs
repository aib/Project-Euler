import Data.Digits
import Data.Ratio

evalfrac [x] = toRational x
evalfrac (x:xs) = toRational x + recip (evalfrac xs)

econv = 2 : concatMap (\n -> [1, 2*n, 1]) [1..]

main = print $ sum . (digits 10) . numerator . evalfrac $ (take 100 econv)
