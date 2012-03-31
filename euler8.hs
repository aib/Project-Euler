import Control.Monad

import Common

-- Greatest multiple of n consecutive digits in a string of numbers. Contact me if you find a better name :/
greatestConsecDigits n digits = maximum $ map (\x -> product $ map (\d -> (read [d] :: Int)) x) (consecElems n digits)

main = do
    digits <- readFile "euler8.txt" >>= (\x -> return $ filter (/= '\n') x)
    print $ greatestConsecDigits 5 digits
