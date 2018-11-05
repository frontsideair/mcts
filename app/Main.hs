import           Opponent.MCTS
import           Opponent.Random
import           Game.Hamiltonicity
import           Game

main :: IO ()
main = do
  result <- playToEnd 10 (mctsPlay :: AIPlayer Hamiltonicity) randomPlay
  print result
