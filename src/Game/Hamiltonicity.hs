module Game.Hamiltonicity
  ( hamiltonicity
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

toggle :: HPlayer -> HPlayer
toggle Maker   = Breaker
toggle Breaker = Maker

isTerminal :: Hamiltonicity -> Bool
isTerminal = isJust . result'

emptyPositions :: Board -> [Position]
emptyPositions = uncoloredEdges

play' position (Hamiltonicity board player) = Hamiltonicity
  (play'' player position board)
  (toggle player)
 where
  play'' Maker   = colorMaker
  play'' Breaker = colorBreaker

result' (Hamiltonicity board _)
  | null (emptyPositions board) = Just (Win Breaker)
  | hasHamiltonianCycle board   = Just (Win Maker)
  | otherwise                   = Nothing

legalMoves' game@(Hamiltonicity board _) =
  if isTerminal game then [] else emptyPositions board

hamiltonicity :: Int -> Game Hamiltonicity HMove HPlayer
hamiltonicity n = Game
  { initialGame = Hamiltonicity (complete n) Maker
  , legalMoves  = legalMoves'
  , turn        = _turn
  , play        = play'
  , result      = result'
  }
