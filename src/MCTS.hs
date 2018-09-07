module MCTS
  ( runMCTSTimeout
  , runMCTSTimes
  , robustChild
  , maxChild
  , initialGameTree
  )
where

import           Data.Tree
import           Data.Ord
import           Data.List
import           Data.Random
import           Debug.Trace
import           System.CPUTime
import           Control.Monad
import           Hamiltonicity                  ( Game
                                                , Move
                                                , Player
                                                , Result(Win)
                                                , initialGame
                                                , legalMoves
                                                , turn
                                                , play
                                                , gameResult
                                                )

data Stats = Stats { _wins :: Float, _visits :: Float } deriving (Show, Eq)

data State = State { _game :: Game, _unplayedMoves :: [Move], _stats :: Stats } deriving (Show, Eq)

type GameTree = Tree State

initialStats = Stats 0 0

-- Moves aren't shuffled on initial state, but it's not a problem since direct leaves of root are all expanded
initialState = State initialGame (legalMoves initialGame) initialStats

initialGameTree = Node initialState []

ucb1 :: Float -> Stats -> Float
ucb1 rootVisits stats =
  exploit stats + sqrt (2 * log rootVisits / _visits stats)

exploit :: Stats -> Float
exploit (Stats wins visits) = wins / visits

shouldExpand :: GameTree -> Bool
shouldExpand (Node (State _ unplayedMoves _) leaves) = not $ null unplayedMoves

expand :: GameTree -> IO (GameTree, Result)
expand (Node (State game unplayedMoves stats) leaves) = do
  -- traceIO $ "Expanding " ++ show move' ++ show game'
  result <- simulate game'
  -- traceIO $ "Result " ++ show result
  leaf   <- makeLeaf game' result
  return (Node (State game rest stats) (leaf : leaves), result)
 where
  move' : rest = unplayedMoves -- These are shuffled at initialization
  game'        = play move' game

-- Smart constructor, shuffles moves on initialization
makeLeaf :: Game -> Result -> IO GameTree
makeLeaf game result = do
  unplayedMoves <- sample $ shuffle $ legalMoves game
  return $ Node (State game unplayedMoves stats) []
  where stats = backprop (turn game) result initialStats

randomPlay :: Game -> IO Game
randomPlay game = do
  move <- sample $ randomElement $ legalMoves game
  return $ play move game
  -- let g' = play move game
  -- traceIO $ show g'
  -- return g'

simulate :: Game -> IO Result
simulate game = case gameResult game of
  Nothing     -> randomPlay game >>= simulate
  Just result -> return result

backprop :: Player -> Result -> Stats -> Stats
backprop player result (Stats win visits) = Stats win' (visits + 1)
 where
  win' = case result of
    Win winner | winner /= player -> win + 1
    _                             -> win

data Selection = Terminal Result | Expand | Select

selection :: GameTree -> Selection
selection tree@(Node (State game _ _) _) = case gameResult game of
  Just result -> Terminal result
  Nothing     -> if shouldExpand tree then Expand else Select

select :: Float -> GameTree -> IO (GameTree, Result)
select rv tree@(Node state leaves) = backprop' <$> case selection tree of
  Terminal result -> trace "Terminal" $ return (tree, result)
  Expand          -> trace "Expanding" $ expand tree
  Select          -> trace "Selecting" $ do
    -- leaves <- sample $ shuffle leaves -- INFO: select randomly from equally scored leaves, slows algorithm down
    let selection : rest = sortOn (Down . ucb1 rv . _stats . rootLabel) leaves
    (selection', result) <- select rv selection
    return (Node state (selection' : rest), result)
 where
  backprop' (Node (State game moves stats) leaves, result) =
    (Node (State game moves (backprop (turn game) result stats)) leaves, result)

step :: GameTree -> IO GameTree
step tree = do
  (tree', _) <- select (_visits $ _stats $ rootLabel tree) tree
  return tree'

selectChild :: (Stats -> Float) -> GameTree -> State
selectChild strategy tree = leaf
 where
  leaf   = maximumBy (comparing (strategy . _stats)) leaves
  leaves = rootLabel <$> subForest tree

maxChild :: GameTree -> State
maxChild = selectChild exploit

robustChild :: GameTree -> State
robustChild = selectChild _visits

runMCTSTimeout :: Integer -> GameTree -> IO GameTree
runMCTSTimeout seconds = helper (seconds * 10 ^ 12)
 where
  helper timeout tree = if timeout > 0
    then do
      start <- getCPUTime
      tree' <- step tree
      end   <- getCPUTime
      helper (timeout - end + start) tree'
    else return tree

runMCTSTimes :: Int -> GameTree -> IO GameTree
runMCTSTimes n tree = foldM ((flip . const) step) tree (replicate n ())
