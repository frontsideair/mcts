import           System.Environment             ( getArgs )
import           Safe                           ( headMay
                                                , readMay
                                                )

import           Opponent.MCTS
import           Game.Hamiltonicity
import           Game

main :: IO ()
main = do
  args <- getArgs
  case headMay args >>= readMay of
    Nothing         -> putStrLn "Usage: hamiltonicity [iterations]"
    Just iterations -> do
      result <- playToEnd (hamiltonicity 6)
                          iterations
                          mctsPlayChanReuse
                          mctsPlayChan
      print result
