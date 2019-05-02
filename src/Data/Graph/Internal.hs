module Data.Graph.Internal where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S

-- uncoloredEdges is the set of edges unclaimed by maker or breaker
-- makerColoredEdges is the set of edges claimed by maker
-- winningHamCycles is the set that contains the cycles made of uncolored and maker colored edges
-- (uncoloredEdges, makerColoredEdges, winningHamCycles)
type Graph = (Set Edge, Set Edge, Set (Set Edge), Int)

type Vertex = Int

type Edge = (Vertex, Vertex)

edge :: Vertex -> Vertex -> Edge
edge u v = if u < v then (u, v) else (v, u)

complete :: Int -> Graph
complete n = (uncoloredEdges, makerColored, winningPaths, graphSize)
 where
  uncoloredEdges = S.fromList [ edge u v | u <- [1 .. n], v <- [u + 1 .. n] ]
  makerColored   = S.empty
  winningPaths   = cycles n
  graphSize = n

rotate :: [a] -> [a]
rotate xs = take (length xs) $ (tail . cycle) xs

cycles :: Int -> Set (Set Edge)
cycles n = S.fromList $ S.fromList . pathToCycle <$> cyclicPaths
 where
  pathToCycle xs = zipWith edge xs (rotate xs)
  cyclicPaths = iterate grow [[1]] !! (n - 1)
  grow paths =
    [ vertex : path | path <- paths, vertex <- [1 .. n], vertex `notElem` path ]

uncoloredEdges :: Graph -> [Edge]
uncoloredEdges (uncolored, _, _, graphSize) = S.toList uncolored

winningPaths :: Graph -> Set (Set Edge)
winningPaths (_, _, winningPaths, graphSize) = winningPaths

colorMaker :: Edge -> Graph -> Graph
colorMaker edge@(u, v) (uncoloredEdges, makerColored, winningPaths, graphSize) =
  (uncoloredEdges', makerColored', winningPaths, graphSize)
 where
  uncoloredEdges' = S.delete edge uncoloredEdges
  makerColored'   = S.insert edge makerColored

colorBreaker :: Edge -> Graph -> Graph
colorBreaker edge@(u, v) (uncoloredEdges, makerColored, winningPaths, graphSize) =
  (uncoloredEdges', makerColored, winningPaths', graphSize)
 where
  uncoloredEdges' = S.delete edge uncoloredEdges
  winningPaths'   = S.filter (S.notMember edge) winningPaths

edgesToVertices :: Set Edge -> Set Vertex
edgesToVertices edges = S.fromList $ (S.toList edges) >>= (\(u, v) -> [u, v])

hasHamiltonianCycle :: Graph -> Bool
hasHamiltonianCycle (_, makerColored, winningPaths, graphSize) =
  S.size makerColored >= graphSize && S.size (edgesToVertices makerColored) >= graphSize && any (`S.isSubsetOf` makerColored) winningPaths
