module Graph where

import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as S
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M

type Graph = (Set Edge, Map Vertex (Set Vertex))

type Vertex = Int

type Edge = (Vertex, Vertex)

graph :: Int -> Graph
graph n = (uncoloredEdges, makerColored)
 where
  uncoloredEdges = S.fromList [ (u, v) | u <- [1 .. n], v <- [u + 1 .. n] ]
  makerColored   = M.fromList [ (u, S.empty) | u <- [1 .. n] ]

colorMaker :: Edge -> Graph -> Graph
colorMaker (u, v) (uncoloredEdges, makerColored) =
  (uncoloredEdges', makerColored')
 where
  uncoloredEdges' = S.delete (u, v) uncoloredEdges
  makerColored' =
    M.adjust (S.insert v) u $ M.adjust (S.insert u) v makerColored

colorBreaker :: Edge -> Graph -> Graph
colorBreaker (u, v) (uncoloredEdges, makerColored) =
  (uncoloredEdges', makerColored)
  where uncoloredEdges' = S.delete (u, v) uncoloredEdges

uncoloredEdges :: Graph -> [Edge]
uncoloredEdges = S.toList . fst

findHam :: Graph -> Bool
findHam (_, makerColored) = helper 1 S.empty 1
 where
  helper :: Int -> Set Int -> Int -> Bool
  helper depth path v = if depth == 6
    then 1 `elem` neighbors
    else any (helper (depth + 1) (S.insert v path)) (neighbors \\ path)
    where neighbors = M.findWithDefault S.empty v makerColored


