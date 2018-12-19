{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Game
  ( Game
  , Move
  , Player
  , Result(Win, Draw)
  , initialGame
  , legalMoves
  , turn
  , play
  , result
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

class (Ord (Move g), Eq (Player g), Show (Move g)) => Game g where
  type Move g = m | m -> g
  type Player g = p | p -> g
  initialGame :: g
  legalMoves :: g -> [Move g]
  turn :: g -> Player g
  play :: Move g -> g -> g
  result :: g -> Maybe (Result (Player g))

type AIPlayer g = Integer -> Chan (Message (Move g)) -> Chan (Message (Move g)) -> IO ()

data Message m = Start | Move m deriving Show

playToEnd
  :: Game g => Integer -> AIPlayer g -> AIPlayer g -> IO (Result (Player g))
playToEnd seconds maker breaker = do
  makerIn    <- newChan
  makerOut   <- newChan
  breakerIn  <- newChan
  breakerOut <- newChan
  a          <- forkIO $ maker seconds makerIn makerOut
  b          <- forkIO $ breaker seconds breakerIn breakerOut
  writeChan makerIn Start
  result <- helper initialGame ((makerOut, breakerIn), (breakerOut, makerIn))
  killThread a
  killThread b
  return result
 where
  helper game chans@((input, output), _) = case result game of
    Just result -> return result
    Nothing     -> do
      Move move <- readChan input
      print move
      writeChan output (Move move)
      helper (play move game) (swap chans)
