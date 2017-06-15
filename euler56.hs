import Data.Digits

main = print $
    foldr max 0 $ map (sum . (digits 10)) [a^b | a <- [1..100], b <- [1..100]]
