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
import           Data.Map.Strict                ( Map
                                                , (!)
                                                )
import qualified Data.Map.Strict               as M

-- (uncoloredEdges, makerColored, winningPaths)
type Graph = (Set Edge, Set Edge, [Set Edge])

type Vertex = Int

type Edge = (Vertex, Vertex)

edge :: Vertex -> Vertex -> Edge
edge u v = if u < v then (u, v) else (v, u)

complete :: Int -> Graph
complete n = (uncoloredEdges, makerColored, winningPaths)
 where
  uncoloredEdges = S.fromList [ edge u v | u <- [1 .. n], v <- [u + 1 .. n] ]
  makerColored   = S.empty
  winningPaths   = cycles n

cycles :: Int -> [Set Edge]
cycles n = z <$> cs
 where
  z xs = S.fromList $ zipWith edge xs (tail (cycle xs))
  cs = iterate g [[1]] !! (n - 1)
  g paths =
    [ vertex : path | path <- paths, vertex <- [1 .. n], vertex `notElem` path ]

colorMaker :: Edge -> Graph -> Graph
colorMaker edge@(u, v) (uncoloredEdges, makerColored, winningPaths) =
  (uncoloredEdges', makerColored', winningPaths)
 where
  uncoloredEdges' = S.delete edge uncoloredEdges
  makerColored'   = S.insert edge makerColored

colorBreaker :: Edge -> Graph -> Graph
colorBreaker edge@(u, v) (uncoloredEdges, makerColored, winningPaths) =
  (uncoloredEdges', makerColored, winningPaths')
 where
  uncoloredEdges' = S.delete edge uncoloredEdges
  winningPaths'   = filter (S.notMember edge) winningPaths

uncoloredEdges :: Graph -> [Edge]
uncoloredEdges (uncolored, _, _) = S.toList uncolored

hasHamiltonianCycle :: Graph -> Bool
hasHamiltonianCycle (_, makerColored, winningPaths) =
  any (`S.isSubsetOf` makerColored) winningPaths
