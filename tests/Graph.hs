module Graph where

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import qualified Data.Set                      as S

import           Data.Graph.Internal

prop_rotate_length :: Property
prop_rotate_length = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  length (rotate xs) === length xs

prop_cycles_length :: Property
prop_cycles_length = property $ do
  n <- forAll $ Gen.integral (Range.linear 3 10)
  (length (cycles n) * 2) === product [1 .. n - 1]

prop_cycles_path_length :: Property
prop_cycles_path_length = property $ do
  n <- forAll $ Gen.integral (Range.linear 3 10)
  all (== n) (S.map length (cycles n)) === True

prop_color_breaker :: Property
prop_color_breaker = property $ do
  n <- forAll $ Gen.integral (Range.linear 3 10)
  let g = complete n
  edge <- forAll $ Gen.element $ uncoloredEdges g
  let g' = colorBreaker edge g
  all (S.notMember edge) (winningPaths g') === True

prop_color_breaker_removed :: Property
prop_color_breaker_removed = property $ do
  n <- forAll $ Gen.integral (Range.linear 3 10)
  let g = complete n
  edge <- forAll $ Gen.element $ uncoloredEdges g
  let g' = colorBreaker edge g
  (length $ S.difference (winningPaths g) (winningPaths g')) === product [1 .. n - 2]
