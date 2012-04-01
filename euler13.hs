import Data.Digits

main = do
    nums <- readFile "euler13.txt" >>= return . (map (read :: String -> Integer)) . lines
    print $ (concat . map show . take 10 . digits 10 . sum) nums
