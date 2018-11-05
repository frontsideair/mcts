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

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Foldable                  ( any )

-- uncoloredEdges is the set of edges unclaimed by maker or breaker
-- makerColoredEdges is the set of edges claimed by maker
-- winningHamCycles is the set that contains the cycles made of uncolored and maker colored edges
-- (uncoloredEdges, makerColoredEdges, winningHamCycles)
type Graph = (Set Edge, Set Edge, Set (Set Edge))

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

rotate :: [a] -> [a]
rotate = tail . cycle

cycles :: Int -> Set (Set Edge)
cycles n = S.fromList $ S.fromList . pathToCycle <$> cyclicPaths
 where
  pathToCycle xs = zipWith edge xs (rotate xs)
  cyclicPaths = iterate grow [[1]] !! (n - 1)
  grow paths =
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
  winningPaths'   = S.filter (S.notMember edge) winningPaths

uncoloredEdges :: Graph -> [Edge]
uncoloredEdges (uncolored, _, _) = S.toList uncolored

hasHamiltonianCycle :: Graph -> Bool
hasHamiltonianCycle (_, makerColored, winningPaths) =
  any (`S.isSubsetOf` makerColored) winningPaths
