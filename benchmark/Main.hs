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
  vary :: Vary,
  iterations :: Int,
  removeCycle :: Bool
} deriving Show

data Vary = Reuse | Iters | Constant | Robust deriving Show

defaultParams :: Int -> Params
defaultParams iters =
  Params { reuse = False, iters, constant = 0.7, robust = True }

variedParams :: Params -> Vary -> [Params]
variedParams fixedParams Reuse =
  [ fixedParams { reuse } | reuse <- [False, True] ]
variedParams fixedParams Iters =
  [ fixedParams { iters }
  | iters <- [1000, 2000, 4000, 8000, 16000, 32000, 64000]
  ]
variedParams fixedParams Constant =
  [ fixedParams { constant } | constant <- [0.1, 0.3, 0.5, 0.7, 0.9] ]
variedParams fixedParams Robust =
  [ fixedParams { robust } | robust <- [True, False] ]

main :: IO ()
main = do
  Options { size, vary, iterations, removeCycle } <- execParser opts
  putStrLn ("Graph: " ++ show size)
  let fixedParams = defaultParams iterations
  traverse_ (go size removeCycle fixedParams) $ variedParams fixedParams vary

go :: Int -> Bool -> Params -> Params -> IO ()
go size removeCycle fixedParams variedParams = do
  putStrLn ("Varied: " ++ show variedParams)
  putStrLn ("Default: " ++ show fixedParams)
  results1 <- forM [1 .. 50] (const (app varied fixed))
  putStr "Varied as Maker, Default as Breaker: "
  print $ frequency results1
  results2 <- forM [1 .. 50] (const (app fixed varied))
  putStr "Default as Maker, Varied as Breaker: "
  print $ frequency results2
  putStrLn ""
 where
  app    = playToEnd False (hamiltonicity size 1 removeCycle)
  varied = mcts variedParams
  fixed  = mcts fixedParams


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
      <*> option
            auto
            (  long "iterations"
            <> short 'i'
            <> help "Number of iterations for fixed player"
            <> showDefault
            <> value 8000
            <> metavar "INT"
            )
      <*> switch (long "remove-cycle" <> short 'r' <> help "Remove one cycle")
