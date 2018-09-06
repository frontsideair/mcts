-- import Game
import           MCTS

main :: IO ()
main = do
  bestMove <- runMCTS
  print bestMove
