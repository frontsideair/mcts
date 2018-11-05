import           Test.Tasty
import           Test.Tasty.Hedgehog

import           Graph

main = defaultMain $ testGroup "Graph" [prop_rotate, prop_cycles, prop_breaker]

prop_rotate = testGroup "rotate" [testProperty "rotate" prop_rotate_length]

prop_cycles = testGroup
  "cycles"
  [ testProperty "prop_cycles_length"      prop_cycles_length
  , testProperty "prop_cycles_path_length" prop_cycles_path_length
  ]

prop_breaker =
  testGroup "breaker" [testProperty "color_breaker" prop_color_breaker]
