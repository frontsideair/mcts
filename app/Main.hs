import           MCTS
import           Hamiltonicity

main :: IO ()
main = do
  tree <- runMCTSTimeout 10 initialGameTree :: IO (GameTree Hamiltonicity)
  print $ robustChild tree
