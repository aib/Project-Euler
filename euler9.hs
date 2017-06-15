main = print $
    (\(a,b,c) -> a*b*c) $ head $ filter (\(a,b,c) -> a^2 + b^2 == c^2) [(a, b, 1000-(a+b)) | a <- [1..1000], b <- [1..1000], a+b < 1000]
