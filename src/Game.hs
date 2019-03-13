{-# LANGUAGE NamedFieldPuns #-}

module Game
  ( Game(..)
  , Result(Win, Draw)
  , AIPlayer
  , playToEnd
  , Message(Start, Move)
  )
where

import           Control.Concurrent.Process     ( Process
                                                , forkProcess
                                                , readProcess
                                                , writeProcess
                                                , killProcess
                                                )

data Result player = Win player | Draw deriving (Show, Eq, Ord)

data Game g m p = Game {
  initialGame :: g,
  legalMoves :: g -> [m],
  turn :: g -> p,
  play :: m -> g -> g,
  result :: g -> Maybe (Result p)
}

type AIPlayer g m p = Game g m p -> Int -> Process (Message m) (Message m) -> IO ()

data Message m = Start | Move m deriving Show

playToEnd
  :: (Show m, Eq p, Ord p)
  => Game g m p
  -> (Int, AIPlayer g m p)
  -> (Int, AIPlayer g m p)
  -> IO (Result p)
playToEnd g@Game { initialGame, result, play } (makerIters, maker) (breakerIters, breaker)
  = do
    makerProcess   <- forkProcess $ maker g makerIters
    breakerProcess <- forkProcess $ breaker g breakerIters
    writeProcess makerProcess Start
    result <- helper initialGame (makerProcess, breakerProcess)
    killProcess makerProcess
    killProcess breakerProcess
    return result
 where
  helper game (process1, process2) = case result game of
    Just result -> return result
    Nothing     -> do
      Move move <- readProcess process1
      -- print move
      writeProcess process2 (Move move)
      helper (play move game) (process2, process1)
