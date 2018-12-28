{-# LANGUAGE NamedFieldPuns #-}

module Game
  ( Game(..)
  , Result(Win, Draw)
  , AIPlayer
  , playToEnd
  , Message(Start, Move)
  )
where

import           Data.Tuple                     ( swap )
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Control.Concurrent.Chan        ( Chan
                                                , newChan
                                                , readChan
                                                , writeChan
                                                )

data Result player = Win player | Draw deriving Show

data Game g m p = Game {
  initialGame :: g,
  legalMoves :: g -> [m],
  turn :: g -> p,
  play :: m -> g -> g,
  result :: g -> Maybe (Result p)
}

type AIPlayer g m p = Game g m p -> Integer -> Chan (Message m) -> Chan (Message m) -> IO ()

data Message m = Start | Move m deriving Show

playToEnd
  :: Show m
  => Game g m p
  -> Integer
  -> AIPlayer g m p
  -> AIPlayer g m p
  -> IO (Result p)
playToEnd g@Game { initialGame, result, play } iterations maker breaker = do
  makerIn    <- newChan
  makerOut   <- newChan
  breakerIn  <- newChan
  breakerOut <- newChan
  thread1    <- forkIO $ maker g iterations makerIn makerOut
  thread2    <- forkIO $ breaker g iterations breakerIn breakerOut
  writeChan makerIn Start
  result <- helper initialGame ((makerOut, breakerIn), (breakerOut, makerIn))
  killThread thread1
  killThread thread2
  return result
 where
  helper game chans@((input, output), _) = case result game of
    Just result -> return result
    Nothing     -> do
      Move move <- readChan input
      print move
      writeChan output (Move move)
      helper (play move game) (swap chans)
