module MCTS
  ( GameTree
  , runMCTSTimeout
  , runMCTSTimes
  , robustChild
  , maxChild
  , initialGameTree
  )
where

import           Data.Tree                      ( Tree(Node)
                                                , rootLabel
                                                , subForest
                                                )
import           Data.Ord                       ( Down(Down)
                                                , comparing
                                                )
import           Data.List                      ( sortOn
                                                , maximumBy
                                                )
import           Data.Random                    ( sample
                                                , shuffle
                                                , randomElement
                                                )
import           Debug.Trace                    ( trace )
import           System.CPUTime                 ( getCPUTime )
import           Control.Monad                  ( foldM )
import           Game                           ( Game
                                                , Move
                                                , Result(Win)
                                                , Player
                                                , legalMoves
                                                , initialGame
                                                , play
                                                , turn
                                                , result
                                                )

data Stats = Stats { _wins :: Float, _visits :: Float } deriving Show

data State g = State { _game :: g, _unplayedMoves :: [Move g], _stats :: Stats }

-- TODO: Turn tree into a keyed one, using Map for leaves
type GameTree g = Tree (State g)

instance Game g => Show (State g) where
  show (State game moves stats) = unlines [show game, show stats]

initialStats = Stats 0 0

-- Moves aren't shuffled on initial state, but it's not a problem since direct leaves of root are all expanded
state game = State game (legalMoves game) initialStats

initialState :: Game g => State g
initialState = state initialGame

initialGameTree :: Game g => GameTree g
initialGameTree = Node initialState []

ucb1 :: Float -> Stats -> Float
ucb1 rootVisits stats =
  exploit stats + sqrt (2 * log rootVisits / _visits stats)

exploit :: Stats -> Float
exploit (Stats wins visits) = wins / visits

shouldExpand :: GameTree g -> Bool
shouldExpand (Node (State _ unplayedMoves _) leaves) = not $ null unplayedMoves

expand :: Game g => GameTree g -> IO (GameTree g, Result (Player g))
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
makeLeaf :: Game g => g -> Result (Player g) -> IO (GameTree g)
makeLeaf game result = do
  unplayedMoves <- sample $ shuffle $ legalMoves game
  return $ Node (State game unplayedMoves stats) []
  where stats = backprop (turn game) result initialStats

randomPlay :: Game g => g -> IO g
randomPlay game = do
  move <- sample $ randomElement $ legalMoves game
  return $ play move game
  -- let g' = play move game
  -- traceIO $ show g'
  -- return g'

simulate :: Game g => g -> IO (Result (Player g))
simulate game = case result game of
  Nothing     -> randomPlay game >>= simulate
  Just result -> return result

backprop :: Eq p => p -> Result p -> Stats -> Stats
backprop player result (Stats win visits) = Stats win' (visits + 1)
 where
  win' = case result of
    Win winner | winner /= player -> win + 1
    _                             -> win

data Selection g = Terminal (Result (Player g)) | Expand | Select

selection :: Game g => GameTree g -> Selection g
selection tree@(Node (State game _ _) _) = case result game of
  Just result -> Terminal result
  Nothing     -> if shouldExpand tree then Expand else Select

select :: Game g => Float -> GameTree g -> IO (GameTree g, Result (Player g))
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

step :: Game g => GameTree g -> IO (GameTree g)
step tree = do
  (tree', _) <- select (_visits $ _stats $ rootLabel tree) tree
  return tree'

selectChild :: (Stats -> Float) -> GameTree g -> State g
selectChild strategy tree = leaf
 where
  leaf   = maximumBy (comparing (strategy . _stats)) leaves
  leaves = rootLabel <$> subForest tree

maxChild :: GameTree g -> State g
maxChild = selectChild exploit

robustChild :: GameTree g -> State g
robustChild = selectChild _visits

runMCTSTimeout :: Game g => Integer -> GameTree g -> IO (GameTree g)
runMCTSTimeout seconds = helper (seconds * 10 ^ 12)
 where
  helper timeout tree = if timeout > 0
    then do
      start <- getCPUTime
      tree' <- step tree
      end   <- getCPUTime
      helper (timeout - end + start) tree'
    else return tree

runMCTSTimes :: Game g => Int -> GameTree g -> IO (GameTree g)
runMCTSTimes n tree = foldM ((flip . const) step) tree (replicate n ())
