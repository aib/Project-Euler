main = print $
    (reverse . take 10 . reverse) $ show $ sum [x^x | x <- [1..1000]]
