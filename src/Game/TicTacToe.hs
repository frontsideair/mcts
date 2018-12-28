{-# LANGUAGE TypeFamilies #-}

module Game.TicTacToe
  ( ticTacToe
  )
where

import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )
import           Data.Matrix                    ( Matrix )
import qualified Data.Matrix                   as M
import qualified Data.Vector                   as V
import           Safe                           ( headMay )
import           Game

data TPlayer = X | O deriving (Show, Eq)

type Position = (Int, Int)

type Board = Matrix (Maybe TPlayer)

data TicTacToe = TicTacToe { _board :: Board, _turn :: TPlayer } deriving (Show, Eq)

type TMove = Position

initialBoard :: Board
initialBoard = M.fromList 3 3 (repeat Nothing)

toggle :: TPlayer -> TPlayer
toggle X = O
toggle O = X

isTerminal :: TicTacToe -> Bool
isTerminal = isJust . result'

emptyPositions :: Board -> [Position]
emptyPositions board = catMaybes $ M.toList $ M.mapPos f board
 where
  f pos Nothing = Just pos
  f pos _       = Nothing

legalMoves' game@(TicTacToe board _) =
  if isTerminal game then [] else emptyPositions board

play' position (TicTacToe board player) =
  TicTacToe (M.setElem (Just player) position board) (toggle player)

result' (TicTacToe board _) =
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

ticTacToe :: Game TicTacToe TMove TPlayer
ticTacToe = Game
  { initialGame = TicTacToe initialBoard X
  , legalMoves  = legalMoves'
  , turn        = _turn
  , play        = play'
  , result      = result'
  }
