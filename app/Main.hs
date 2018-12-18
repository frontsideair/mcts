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
    Nothing      -> putStrLn "Usage: hamiltonicity [seconds]"
    Just seconds -> do
      result <- playToEnd seconds
                          (mctsPlayChanReuse :: AIPlayer Hamiltonicity)
                          mctsPlayChan
      print result
