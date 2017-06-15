module Roman
    (romans)
  where

romans :: Bool -> Int -> [String]
romans subtractive num = [th ++ hu ++ te ++ on |
    th <- [replicate thousands 'M'],
    hu <- [replicate hundreds 'C']
            ++ (if hundreds >= 5 then ['D' : replicate (hundreds-5) 'C'] else [])
            ++ (if subtractive then if hundreds == 4 then ["CD"] else if hundreds == 9 then ["CM"] else [] else []),
    te <- [replicate tens 'X']
            ++ (if tens >= 5 then ['L' : replicate (tens-5) 'X'] else [])
            ++ (if subtractive then if tens == 4 then ["XL"] else if tens == 9 then ["XC"] else [] else []),
    on <- [replicate ones 'I']
            ++ (if ones >= 5 then ['V' : replicate (ones-5) 'I'] else [])
            ++ (if subtractive then if ones == 4 then ["IV"] else if ones == 9 then ["IX"] else [] else [])
    ]
  where
    (thousands, th_rem) = num `divMod` 1000
    (hundreds, hu_rem) = th_rem `divMod` 100
    (tens, te_rem) = hu_rem `divMod` 10
    ones = te_rem
