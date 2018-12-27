import           System.Environment             ( getArgs )
import           Safe                           ( headMay
                                                , readMay
                                                )

import           Opponent.MCTS
import           Opponent.Random
import           Game.Hamiltonicity
import           Game

main :: IO ()
main = do
  args <- getArgs
  case headMay args >>= readMay of
    Nothing      -> putStrLn "Usage: hamiltonicity [iterations]"
    Just iterations -> do
      result <- playToEnd iterations
                          (mctsPlayChanReuse :: AIPlayer Hamiltonicity)
                          mctsPlayChan
      print result
