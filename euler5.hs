import Control.Applicative

head $ dropWhile (\x -> not $ all (==0) $ map (flip mod) [1..20] <*> [x]) [1..]