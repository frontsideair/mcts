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
  )
where

data Result player = Win player | Draw deriving Show

class (Show g, Eq (Player g)) => Game g where
  type Move g
  type Player g
  initialGame :: g
  legalMoves :: g -> [Move g]
  turn :: g -> Player g
  play :: Move g -> g -> g
  result :: g -> Maybe (Result (Player g))
