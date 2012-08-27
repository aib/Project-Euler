import Data.List

main = do
  print $ length . filter (>1000000) . map (uncurry combs) $ [(n,r) | n <- [1..100], r <- [1..n]]
  where
    combs n r = fac n `div` (fac r * fac (n - r))
    fac = foldl' (*) 1 . enumFromTo 1
