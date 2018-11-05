{-# LANGUAGE TypeFamilies #-}
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
  )
where

import           Data.Tuple
import           Debug.Trace                    ( traceIO )

data Result player = Win player | Draw deriving Show

class (Ord (Move g), Eq (Player g), Show (Move g)) => Game g where
  type Move g
  type Player g
  initialGame :: g
  legalMoves :: g -> [Move g]
  turn :: g -> Player g
  play :: Move g -> g -> g
  result :: g -> Maybe (Result (Player g))

type AIPlayer g = Integer -> g -> IO (Move g)

playToEnd
  :: Game g => Integer -> AIPlayer g -> AIPlayer g -> IO (Result (Player g))
playToEnd seconds =
  let helper game players@(currentPlayer, _) = do
        move <- currentPlayer seconds game
        traceIO (show move)
        let nextGame = play move game
        case result nextGame of
          Nothing  -> helper nextGame $ swap players
          Just res -> return res
  in  curry $ helper initialGame
