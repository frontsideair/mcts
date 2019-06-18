{-# LANGUAGE NamedFieldPuns #-}

import           Options.Applicative

import           Opponent.MCTS
import           Opponent.Minimax
import           Game.Hamiltonicity
import           Game.TicTacToe
import           Game

data Options = Options {
  size :: Int,
  makerReuse :: Bool,
  breakerReuse :: Bool,
  makerIters :: Int,
  breakerIters :: Int,
  removeCycle :: Bool
} deriving Show

defaultParams :: Params
defaultParams =
  Params { reuse = False, iters = 1000, constant = 0.7, robust = True }

main :: IO ()
main = do
  Options { size, makerReuse, breakerReuse, makerIters, breakerIters, removeCycle } <-
    execParser opts
  result <- playToEnd
    True
    (hamiltonicity size 1 removeCycle) -- ticTacToe
    (mcts (defaultParams { reuse = makerReuse, iters = makerIters })) -- minimaxChan
    (mcts (defaultParams { reuse = breakerReuse, iters = breakerIters })) -- minimaxChan
  print result

opts :: ParserInfo Options
opts = info (parser <**> helper) mempty
 where
  parser =
    Options
      <$> option
            auto
            (  long "size"
            <> short 's'
            <> help "Size of complete graph"
            <> showDefault
            <> value 6
            <> metavar "INT"
            )
      <*> switch (long "maker-reuse" <> help "Tree reuse for maker")
      <*> switch (long "breaker-reuse" <> help "Tree reuse for breaker")
      <*> option
            auto
            (  long "maker-iters"
            <> help "Number of iterations for maker"
            <> showDefault
            <> value 1000
            <> metavar "INT"
            )
      <*> option
            auto
            (  long "breaker-iters"
            <> help "Number of iterations for breaker"
            <> showDefault
            <> value 1000
            <> metavar "INT"
            )
      <*> switch (long "remove-cycle" <> help "One cycle removed")
