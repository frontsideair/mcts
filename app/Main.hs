import           MCTS
import           Hamiltonicity
import           TicTacToe
import           GamePlay

main :: IO ()
main = do
  result <- gamePlay 10 (mctsPlay :: AIPlayer Hamiltonicity) mctsPlay
  print result
