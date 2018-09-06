import           MCTS

main :: IO ()
main = do
  tree <- runMCTSTimeout 10 initialGameTree
  print $ robustChild tree
