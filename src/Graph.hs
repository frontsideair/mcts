module Graph
  ( Graph
  , Edge
  , complete
  , uncoloredEdges
  , colorMaker
  , colorBreaker
  , hasHamiltonianCycle
  )
where

import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as S
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M

-- TODO: use vector for O(1) access
type Graph = (Set Edge, Map Vertex (Set Vertex))

type Vertex = Int

type Edge = (Vertex, Vertex)

complete :: Int -> Graph
complete n = (uncoloredEdges, makerColored)
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

-- TODO: Optimize this
hasHamiltonianCycle :: Graph -> Bool
hasHamiltonianCycle (_, makerColored) = helper S.empty 1
 where
  n = M.size makerColored
  helper :: Set Int -> Int -> Bool
  helper path v = if S.size path < n
    then 1 `elem` neighbors
    else any (helper (S.insert v path)) (neighbors \\ path)
    where neighbors = M.findWithDefault S.empty v makerColored


