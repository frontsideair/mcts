import           MCTS
import           Hamiltonicity
import           TicTacToe

main :: IO ()
main = do
  tree <- runMCTSTimeout 10 initialGameTree :: IO (GameTree Hamiltonicity)
  print $ rootStats tree
  print $ stats tree
  print $ robustChild tree
