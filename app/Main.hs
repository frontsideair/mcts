import           MCTS
import           Hamiltonicity
import           GamePlay

main :: IO ()
main = do
  result <- gamePlay 10 (mctsPlay :: AIPlayer Hamiltonicity) mctsPlay
  print result
