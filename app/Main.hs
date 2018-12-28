{-# LANGUAGE NamedFieldPuns #-}

import           Options.Applicative

import           Opponent.MCTS
import           Game.Hamiltonicity
import           Game

data Options = Options {
  iterations :: Int,
  size :: Int
}

main :: IO ()
main = do
  Options { iterations, size } <- execParser opts
  result                       <- playToEnd (hamiltonicity size)
                                            iterations
                                            mctsPlayChanReuse
                                            mctsPlayChan
  print result
 where
  parser =
    Options
      <$> option
            auto
            (  long "iterations"
            <> short 'i'
            <> help "Number of iterations"
            <> showDefault
            <> value 10000
            <> metavar "INT"
            )
      <*> option
            auto
            (  long "size"
            <> short 's'
            <> help "Size of complete graph"
            <> showDefault
            <> value 6
            <> metavar "INT"
            )
  opts = info (parser <**> helper) mempty
