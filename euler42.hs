import Data.Char
import Data.List.Split

ntri n = (n * (n+1)) `div` 2

wordVal = sum . map charVal
  where
    charVal = ((+1) . (subtract $ ord 'A') . ord)

triNum n = (elem n . takeWhile (<= n)) $ map ntri [1..]

main = do
    file <- readFile "euler42-words.txt"
    wordList <- return $ map (filter (/= '"')) $ wordsBy (== ',') file
    print $ (length . filter id) $ map (triNum . wordVal) wordList
