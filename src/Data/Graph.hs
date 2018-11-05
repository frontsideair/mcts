module Data.Graph
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

import           Data.Graph.Internal
