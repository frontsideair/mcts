{-# LANGUAGE TypeFamilies #-}

module Game.Hamiltonicity
  ( Hamiltonicity
  )
where

import           Data.Maybe                     ( isJust )
import           Data.Graph                     ( Graph
                                                , Edge
                                                , complete
                                                , uncoloredEdges
                                                , colorMaker
                                                , colorBreaker
                                                , hasHamiltonianCycle
                                                )
import           Game

data HPlayer = Maker | Breaker deriving (Show, Eq, Ord)

type Position = Edge

-- TODO: Make use of symmetry
type Board = Graph

data Hamiltonicity = Hamiltonicity { _board :: Board, _turn :: HPlayer } deriving (Show, Eq)

type HMove = Position

initialBoard :: Board
initialBoard = complete 6

toggle :: HPlayer -> HPlayer
toggle Maker   = Breaker
toggle Breaker = Maker

isTerminal :: Game g => g -> Bool
isTerminal = isJust . result

emptyPositions :: Board -> [Position]
emptyPositions = uncoloredEdges

instance Game Hamiltonicity where
  type Move Hamiltonicity = HMove
  type Player Hamiltonicity = HPlayer
  initialGame = Hamiltonicity initialBoard Maker
  legalMoves game@(Hamiltonicity board _) = if isTerminal game then [] else emptyPositions board
  turn = _turn
  play position (Hamiltonicity board player) = Hamiltonicity
      (play' player position board)
      (toggle player)
    where
      play' Maker   = colorMaker
      play' Breaker = colorBreaker
  result (Hamiltonicity board _)
    | null (emptyPositions board) = Just (Win Breaker)
    | hasHamiltonianCycle board               = Just (Win Maker)
    | otherwise                   = Nothing
