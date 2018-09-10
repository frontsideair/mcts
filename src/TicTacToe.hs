{-# LANGUAGE TypeFamilies #-}

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
import           Game

data TPlayer = X | O deriving (Show, Eq)

type Position = (Int, Int)

type Board = M.Matrix (Maybe TPlayer)

data TicTacToe = TicTacToe { _board :: Board, _turn :: TPlayer } deriving (Show, Eq)

type TMove = Position

initialBoard :: Board
initialBoard = M.fromList 3 3 (repeat Nothing)

toggle :: TPlayer -> TPlayer
toggle X = O
toggle O = X

isTerminal :: TicTacToe -> Bool
isTerminal = isJust . gameResult

emptyPositions :: Board -> [Position]
emptyPositions board = catMaybes $ M.toList $ M.mapPos f board
 where
  f pos Nothing = Just pos
  f pos _       = Nothing

instance Game TicTacToe where
  type Move TicTacToe = TMove
  type Player TicTacToe = TPlayer
  initialGame = TicTacToe initialBoard X
  legalMoves game@(TicTacToe board _) = if isTerminal game then [] else emptyPositions board
  turn = _turn
  play position (TicTacToe board player) = TicTacToe (M.setElem (Just player) position board) (toggle player)
  gameResult (TicTacToe board _) = maybe (if isDraw then Just Draw else Nothing) (Just <$> Win)
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
