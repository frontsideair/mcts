module TicTacToe
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
import qualified Data.Matrix                   as M
import qualified Data.Vector                   as V
import           Safe

data Player = X | O deriving (Show, Eq)

type Position = (Int, Int)

type Board = M.Matrix (Maybe Player)

data Game = Game { _board :: Board, _turn :: Player } deriving (Show, Eq)

type Move = Position

data Result = Win Player | Draw deriving Show

turn :: Game -> Player
turn = _turn

initialBoard :: Board
initialBoard = M.fromList 3 3 (repeat Nothing)

initialGame :: Game
initialGame = Game initialBoard X

toggle :: Player -> Player
toggle X = O
toggle O = X

isTerminal :: Game -> Bool
isTerminal = isJust . gameResult

legalMoves :: Game -> [Move]
legalMoves game@(Game board _) =
  if isTerminal game then [] else emptyPositions board

emptyPositions :: Board -> [Position]
emptyPositions board = catMaybes $ M.toList $ M.mapPos f board
 where
  f pos Nothing = Just pos
  f pos _       = Nothing

play :: Move -> Game -> Game
play position (Game board player) =
  Game (M.setElem (Just player) position board) (toggle player)

gameResult :: Game -> Maybe Result -- TODO: not ideal
gameResult (Game board _) =
  maybe (if isDraw then Just Draw else Nothing) (Just <$> Win)
    $   headMay
    $   catMaybes
    $   isWinner
    <$> winnings board
 where
  isWinner [Just x, Just y, Just z] | x == y && y == z = Just x
  isWinner _ = Nothing

  isDraw = null $ emptyPositions board

  winnings board = V.toList (M.getDiag board) : M.toLists board ++ M.toLists
    (M.transpose board)
