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
  breakerMoves :: Int
} deriving Show

main :: IO ()
main = do
  Options { size, makerReuse, breakerReuse, makerIters, breakerIters, breakerMoves } <-
    execParser opts
  result <- playToEnd True
                      (hamiltonicity size breakerMoves) -- ticTacToe
                      (mcts makerReuse makerIters) -- minimaxChan
                      (mcts breakerReuse breakerIters) -- minimaxChan
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
      <*> option
            auto
            (  long "breaker-moves"
            <> help "Number of moves breaker can make in a turn"
            <> showDefault
            <> value 1
            <> metavar "INT"
            )
