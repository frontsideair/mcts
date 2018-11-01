import           MCTS
import           RandomOpponent
import           Hamiltonicity
import           GamePlay

main :: IO ()
main = do
  result <- gamePlay 10 (mctsPlay :: AIPlayer Hamiltonicity) randomPlay
  print result
