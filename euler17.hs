import Data.Char

intToEnglish :: Integer -> String
intToEnglish n
    | n < 0 = "negative " ++ intToEnglish (0 - n)

    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five" 
    | n == 6 = "six" 
    | n == 7 = "seven" 
    | n == 8 = "eight" 
    | n == 9 = "nine" 

    | n == 10 = "ten"
    | n == 11 = "eleven"
    | n == 12 = "twelve"
    | n == 13 = "thirteen"
    | n == 14 = "fourteen"
    | n == 15 = "fifteen"
    | n == 16 = "sixteen"
    | n == 17 = "seventeen"
    | n == 18 = "eighteen"
    | n == 19 = "nineteen"

    | n == 20 = "twenty"
    | n == 30 = "thirty"
    | n == 40 = "forty"
    | n == 50 = "fifty"
    | n == 60 = "sixty"
    | n == 70 = "seventy"
    | n == 80 = "eighty"
    | n == 90 = "ninety"

    | n >= 20 && n <= 99 = intToEnglish (n - n `mod` 10) ++ "-" ++ intToEnglish (n `mod` 10)

    | n >= 100 && n <= 999 = intToEnglish (n `div` 100) ++ " hundred" ++ let r = n `mod` 100 in if r > 0 then " and " ++ intToEnglish r else ""

    | n >= 1000 && n <= 999999 = intToEnglish (n `div` 1000) ++ " thousand" ++ let r = n `mod` 1000 in if r > 0 then " " ++ intToEnglish r else ""

main = print $
    length . filter isAlpha . concat . map intToEnglish $ [1..1000]
