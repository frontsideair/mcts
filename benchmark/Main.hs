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
  vary :: Vary
} deriving Show

data Vary = Reuse | Iters | Constant | Robust deriving Show

defaultParams :: Params
defaultParams =
  Params { reuse = False, iters = 1000, constant = 0.7, robust = True }

variedParams :: Vary -> [Params]
variedParams Reuse = [ defaultParams { reuse } | reuse <- [False, True] ]
variedParams Iters =
  [ defaultParams { iters } | iters <- [1000, 2000, 4000, 8000, 16000] ]
variedParams Constant =
  [ defaultParams { constant } | constant <- [0.1, 0.3, 0.5, 0.7, 0.9] ]
variedParams Robust = [ defaultParams { robust } | robust <- [True, False] ]

main :: IO ()
main = do
  Options { size, vary } <- execParser opts
  putStrLn ("Graph: " ++ show size)
  traverse_ (go size) $ variedParams vary

go :: Int -> Params -> IO ()
go size variedParams = do
  putStrLn ("Varied: " ++ show variedParams)
  putStrLn ("Default: " ++ show defaultParams)
  results1 <- forM [1 .. 50] (const (app varied def))
  putStr "Varied as Maker, Default as Breaker: "
  print $ frequency results1
  results2 <- forM [1 .. 50] (const (app def varied))
  putStr "Default as Maker, Varied as Breaker: "
  print $ frequency results2
  putStrLn ""
 where
  app    = playToEnd False (hamiltonicity size 1)
  varied = mcts variedParams
  def    = mcts defaultParams


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
            <> value 7
            <> metavar "INT"
            )
      <*> (   flag' Reuse    (long "reuse")
          <|> flag' Iters    (long "iters")
          <|> flag' Constant (long "constant")
          <|> flag' Robust   (long "robust")
          )
