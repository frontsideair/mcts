{-# LANGUAGE NamedFieldPuns #-}

import           Options.Applicative
import           Data.Foldable
import           Control.Monad
import qualified Data.Map                      as M

import           Opponent.MCTS
import           Game.Hamiltonicity
import           Game

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = M.toList (M.fromListWith (+) [ (x, 1) | x <- xs ])

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
  let maker   = mcts makerReuse
  let breaker = mcts breakerReuse
  result <- playToEnd True
                      (hamiltonicity size breakerMoves)
                      (makerIters  , maker)
                      (breakerIters, breaker)
  print result
  where mcts reuse = if reuse then mctsPlayChanReuse else mctsPlayChan

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
          <> short 'b'
          <> help "Number of moves breaker can make in a turn"
          <> showDefault
          <> value 1
          <> metavar "INT"
          )
opts = info (parser <**> helper) mempty
