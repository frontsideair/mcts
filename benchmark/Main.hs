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
  varyMaker :: Bool,
  size :: Int,
  breakerMoves :: Int
} deriving Show

main :: IO ()
-- main = execParser opts >>= app
main = do
  Options { varyMaker, size, breakerMoves } <- execParser opts
  traverse_ go $ do
    variedIters <- [1000, 2000, 4000, 8000, 16000, 32000, 64000]
    let fixedIters = 16000
    variedReuse <- [False, True]
    let fixedReuse = False
    -- size <- [6, 7, 8]
    return $ if varyMaker
      then
        (variedIters, fixedIters, variedReuse, fixedReuse, size, breakerMoves)
      else
        (fixedIters, variedIters, fixedReuse, variedReuse, size, breakerMoves)

go :: (Int, Int, Bool, Bool, Int, Int) -> IO ()
go (makerIters, breakerIters, makerReuse, breakerReuse, size, breakerMoves) =
  do
    putStrLn ("Graph: " ++ show size)
    putStrLn
      ("Maker iters: " ++ show makerIters ++ ", reuse: " ++ show makerReuse)
    putStrLn
      (  "Breaker iters: "
      ++ show breakerIters
      ++ ", reuse: "
      ++ show breakerReuse
      )
    results <- forM [1 .. 200] (const app)
    print $ frequency results
    putStrLn ""
 where
  mcts reuse = if reuse then mctsPlayChanReuse else mctsPlayChan
  app = playToEnd False
                  (hamiltonicity size breakerMoves)
                  (mcts makerReuse makerIters)
                  (mcts breakerReuse breakerIters)

parser =
  Options
    <$> switch (long "vary-maker" <> help "Should vary maker")
    <*> option
          auto
          (  long "size"
          <> short 's'
          <> help "Size of complete graph"
          <> showDefault
          <> value 7
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
