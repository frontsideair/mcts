module Hamiltonicity
  ( Game
  , Move
  , Player
  , Result(Win, Draw)
  , initialGame
  , legalMoves
  , turn
  , play
  , gameResult
  )
where

import           Data.Maybe
import           Safe
import           Graph

data Player = Maker | Breaker deriving (Show, Eq, Ord)

type Position = Edge

type Board = Graph

data Game = Game { _board :: Board, _turn :: Player } deriving (Show, Eq)

type Move = Position

data Result = Win Player | Draw deriving Show

turn :: Game -> Player
turn = _turn

initialBoard :: Board
initialBoard = graph 6

initialGame :: Game
initialGame = Game initialBoard Maker

toggle :: Player -> Player
toggle Maker   = Breaker
toggle Breaker = Maker

isTerminal :: Game -> Bool
isTerminal = isJust . gameResult

legalMoves :: Game -> [Move]
legalMoves game@(Game board _) =
  if isTerminal game then [] else emptyPositions board

emptyPositions :: Board -> [Position]
emptyPositions = uncoloredEdges

play :: Move -> Game -> Game
play position (Game board player) = Game (play' player position board)
                                         (toggle player)
 where
  play' Maker   = colorMaker
  play' Breaker = colorBreaker

gameResult :: Game -> Maybe Result
gameResult (Game board _) | null (emptyPositions board) = Just (Win Breaker)
                          | findHam board               = Just (Win Maker)
                          | otherwise                   = Nothing
