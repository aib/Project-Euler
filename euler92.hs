import Data.Digits

chain = iterate (sum . map (^2) . (digits 10))
classify = head . dropWhile ((flip notElem) [1, 89]) . chain

main = print $ length . (filter (== 89)) . map classify $ [1..9999999]
