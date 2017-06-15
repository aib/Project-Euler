module Roman
    (romans, fromRoman)
  where

import Control.Arrow
import Data.List.Split

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

fromRoman :: String -> Int
fromRoman str = snd $ foldl addval (str, 0) tvs
  where
    addval (str, val) (token, tokenVal) = (concat &&& (val +). (tokenVal *) . (subtract 1) . length) $ splitOn token str
    tvs = [("CM", 900), ("M", 1000), ("CD", 400), ("D", 500), ("XC", 90), ("C", 100), ("XL", 40), ("L", 50), ("IX", 9), ("X", 10), ("IV", 4), ("V", 5), ("I", 1)]
