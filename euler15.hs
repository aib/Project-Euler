import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

width = 5
height = 5

type NodeLabel = (Int,Int,Int)
type EdgeLabel = String

makeNode :: Int -> Int -> LNode NodeLabel
makeNode x y = (nodeId, (nodeId,x,y))
  where
    nodeId = y*height + x

graph :: Gr NodeLabel EdgeLabel
graph = mkGraph [makeNode x y | x <- [0..width-1], y <- [0..height-1]] graphEdges

graphEdges :: [(Int,Int,EdgeLabel)]
graphEdges = [edge (x,y) (x+1,y) | x <- [0..width-2], y <- [0..height-1]] ++
             [edge (x,y) (x,y+1) | x <- [0..width-1], y <- [0..height-2]]
  where
    edge (x,y) (x',y') = (fst $ makeNode x y, fst $ makeNode x' y', show (fst $ makeNode x y) ++ "->" ++ show (fst $ makeNode x' y'))

paths :: Graph g => Decomp g NodeLabel EdgeLabel -> [EdgeLabel] -> Node -> Node -> [[EdgeLabel]]
paths d p s e | s == e = [p]
paths (Just ct, g) p s e = (concatMap (\(ne,nn) -> paths (match nn g) (p ++ [ne]) nn e) (toAdjs ct))
  where
    toAdjs = (\(_,_,_,a) -> a)

--main = print $ length $ paths (Just (context graph 0), graph) [] 0 (width*height - 1)

--path is a string of 40 movements, 20 of which go right and 20 down: (R|D){40} where count(R) = count(D) = 20
--40! total permutations, divide 20! because Rs are indistinct, 20! because Ds are indistinct
main = print $ product [1..40] `div` product [1..20] `div` product [1..20]
