import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.List.Split

wordScore = sum . map letterScore
  where
    letterScore l = 1 + flip ((-) `on` ord) 'A' l


main = do
    names <- liftM (map (filter (/= '"')) . wordsBy (== ',')) $ readFile "euler22-names.txt"
    print $ sum $ zipWith (*) [1..] (map wordScore (sort names))
