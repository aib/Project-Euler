import Control.Monad
import Data.Function
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.List
import Maybe

type NodeLabel = Int
type EdgeLabel = ()

main = do
    nums <- liftM (map (map (read :: String -> Int) . words) . lines) $ readFile "euler67-triangle.txt"
    let graph = graphFromRows nums in
        print . show $ maximumBy (compare `on` snd) $ map (bestPath graph) $ nodes graph

labelRows :: [[a]] -> Int -> [[(Int, a)]]
labelRows [] _ = []
labelRows (r:rs) n = row : labelRows rs newn
  where
    (row, newn) = (zipWith (,) [n..] r, n + length r)

graphFromRows :: [[Int]] -> Gr NodeLabel EdgeLabel
graphFromRows rs = fst $ graphFromRows' (labelRows rs 1)

graphFromRows' :: [[(Int, Int)]] -> (Gr NodeLabel EdgeLabel, [Node])
graphFromRows' [r] = (mkGraph r [], map fst r)
graphFromRows' (r:rs) = ((insEdges ledges . insEdges redges . insNodes r) graph, curnodes)
  where
    curnodes = map fst r
    (graph, subnodes) = graphFromRows' rs
    ledges = zipWith3 (,,) curnodes (init subnodes) (repeat ())
    redges = zipWith3 (,,) curnodes (tail subnodes) (repeat ())

bestPath :: Gr NodeLabel EdgeLabel -> Node -> ([Node], Int)
bestPath g n
    | preds == [] = ([n], getLabel n)
    | otherwise   = let (path, cost) = maximumBy (compare `on` snd) predPaths in (path ++ [n], cost + getLabel n)
  where
    preds = pre g n
    predPaths = map (bestPath g) preds
    getLabel = fromJust . lab g
