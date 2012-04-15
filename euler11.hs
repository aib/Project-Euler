import Control.Monad
import Data.Ix

width = 20
height = 20

main = do
    matrix <- liftM (map (map (read :: String -> Integer) . words) . lines) $ readFile "euler11.txt"
    print $ maximum $ map (getProduct matrix) (lines2D width height 4)
  where
    getProduct matrix points = product $ map (\(x,y) -> matrix !! y !! x) points

lines2D :: Int -> Int -> Int -> [[(Int, Int)]]
lines2D width height len =
    let unfilteredLines = concatMap (\p -> map (($ p) . (\f -> take len . iterate f)) allDirs) startPoints in
        filter (all (\(x,y) -> inRange(0, width-1) x && inRange(0, height-1) y)) unfilteredLines
  where
    left  (x,y) = (x+1,y)
    down  (x,y) = (x,y+1)
    diag  (x,y) = (x+1,y+1)
    rdiag (x,y) = (x+1,y-1)
    allDirs = [left, down, diag, rdiag]
    startPoints = [(x,y) | x <- [0..width-1], y <- [0..height-1]]
