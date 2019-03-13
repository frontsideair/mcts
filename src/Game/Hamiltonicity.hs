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

data Turn = MakerTurn | BreakerTurn Int deriving (Show, Eq)

data HPlayer = Maker | Breaker deriving (Show, Eq, Ord)

turnToPlayer :: Turn -> HPlayer
turnToPlayer MakerTurn       = Maker
turnToPlayer (BreakerTurn _) = Breaker

type Position = Edge

-- TODO: Make use of symmetry
type Board = Graph

data Hamiltonicity = Hamiltonicity { _board :: Board, _turn :: Turn } deriving (Show, Eq)

type HMove = Position

toggle :: Int -> Turn -> Turn
toggle _ MakerTurn = BreakerTurn 1
toggle breakerMoves (BreakerTurn n) | n == breakerMoves = MakerTurn
                                    | otherwise         = BreakerTurn (n + 1)

isTerminal :: Hamiltonicity -> Bool
isTerminal = isJust . result'

emptyPositions :: Board -> [Position]
emptyPositions = uncoloredEdges

play' breakerMoves position (Hamiltonicity board player) = Hamiltonicity
  (play'' player position board)
  (toggle breakerMoves player)
 where
  play'' MakerTurn       = colorMaker
  play'' (BreakerTurn n) = colorBreaker

result' (Hamiltonicity board _)
  | null (emptyPositions board) = Just (Win Breaker)
  | hasHamiltonianCycle board   = Just (Win Maker)
  | otherwise                   = Nothing

legalMoves' game@(Hamiltonicity board _) =
  if isTerminal game then [] else emptyPositions board

hamiltonicity :: Int -> Int -> Game Hamiltonicity HMove HPlayer
hamiltonicity n breakerMoves = Game
  { initialGame = Hamiltonicity (complete n) MakerTurn
  , legalMoves  = legalMoves'
  , turn        = turnToPlayer . _turn
  , play        = play' breakerMoves
  , result      = result'
  }
