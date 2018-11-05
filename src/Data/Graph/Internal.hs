module Data.Graph.Internal where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S

type Vertex = Int

type Edge = (Vertex, Vertex)

edge :: Vertex -> Vertex -> Edge
edge u v = if u < v then (u, v) else (v, u)

rotate :: [a] -> [a]
rotate xs = take (length xs) $ (tail . cycle) xs

cycles :: Int -> Set (Set Edge)
cycles n = S.fromList $ S.fromList . pathToCycle <$> cyclicPaths
 where
  pathToCycle xs = zipWith edge xs (rotate xs)
  cyclicPaths = iterate grow [[1]] !! (n - 1)
  grow paths =
    [ vertex : path | path <- paths, vertex <- [1 .. n], vertex `notElem` path ]
