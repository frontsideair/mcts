{-# LANGUAGE NamedFieldPuns #-}

module Game
  ( Game(..)
  , Result(Win, Draw)
  , AIPlayer
  , playToEnd
  , Message(Start, Move)
  )
where

import           Control.Monad                  ( when )
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

type AIPlayer g m p = Game g m p -> Process (Message m) (Message m) -> IO ()

data Message m = Start | Move m deriving Show

playToEnd
  :: (Show m, Eq p, Ord p)
  => Bool
  -> Game g m p
  -> AIPlayer g m p
  -> AIPlayer g m p
  -> IO (Result p)
playToEnd logMove g@Game { initialGame, result, play } p1 p2 = do
  p1Process <- forkProcess $ p1 g
  p2Process <- forkProcess $ p2 g
  writeProcess p1Process Start
  result <- helper initialGame (p1Process, p2Process)
  killProcess p1Process
  killProcess p2Process
  return result
 where
  helper game (process1, process2) = case result game of
    Just result -> return result
    Nothing     -> do
      Move move <- readProcess process1
      when logMove $ print move
      writeProcess process2 (Move move)
      helper (play move game) (process2, process1)
