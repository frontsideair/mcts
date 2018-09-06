module MCTS where

import           Data.Tree
import           Data.Foldable
import           Data.Ord
import           Data.List
import           Safe.Foldable
import           Data.Random
import           Debug.Trace
import           System.CPUTime
import           Control.Monad
import           TicTacToe                      ( Game(_turn)
                                                , Move
                                                , Player
                                                , Result(Win, Draw)
                                                , initialGame
                                                , moves
                                                , play
                                                , result
                                                , isTerminal
                                                )

data Stats = Stats { _wins :: Float, _visits :: Float } deriving (Show, Eq)

data State = State { _game :: Game, _unplayedMoves :: [Move], _stats :: Stats } deriving (Show, Eq)

type GameTree = Tree State

initialStats = Stats 0 0

initialState = State initialGame (moves initialGame) initialStats

initialGameTree = Node initialState []

ucb1 :: Float -> Stats -> Float
ucb1 rootVisits (Stats wins visits) =
  (wins / visits) + sqrt (2 * log rootVisits / visits)

shouldExpand :: GameTree -> Bool
shouldExpand (Node (State _ unplayedMoves _) leaves) = not $ null unplayedMoves

expand :: GameTree -> IO (GameTree, Result)
expand tree@(Node (State game unplayedMoves stats) leaves) = do
  let move' : rest = unplayedMoves
  let game'        = play move' game
  -- traceIO $ "Expanding " ++ show move' ++ show game'
  result            <- simulate game'
  -- traceIO $ "Result " ++ show res
  leaf <- makeLeaf game' result
  -- unplayedMoves' <- sample $ shuffle $ moves game'
  -- let stats' = backprop (_turn game') result initialStats
  -- let leaf = Node (State game' unplayedMoves' stats') []
  return
    ( Node (State game rest (backprop (_turn game) result stats)) (leaf : leaves)
    , result
    )

makeLeaf :: Game -> Result -> IO GameTree
makeLeaf game result = do
  unplayedMoves <- sample $ shuffle $ moves game
  return $ Node (State game unplayedMoves stats) []
  where stats = backprop (_turn game) result initialStats

randomPlay :: Game -> IO Game
randomPlay game = do
  move <- sample $ randomElement $ moves game
  return $ play move game
  -- let g' = play move game
  -- traceIO $ show g'
  -- return g'

simulate :: Game -> IO Result
simulate game = case result game of
  Nothing     -> randomPlay game >>= simulate
  Just result -> return result

backprop :: Player -> Result -> Stats -> Stats
backprop player result (Stats win visits) = Stats win' (visits + 1)
 where
  win' = case result of
    Win winner | winner /= player -> win + 1
    _                             -> win

-- TODO: get rid of Draw
select :: Float -> GameTree -> IO (GameTree, Result)
select rootVisits tree@(Node (State g m s) leaves)
  | isTerminal g = trace "Terminal" $ return (tree, Draw)
  | shouldExpand tree = trace "Expanding" $ expand tree
  | otherwise = trace ("Selecting " ++ show rootVisits) $ do
    leaves <- sample $ shuffle leaves -- INFO: select randomly from equally scored leaves, slows algorithm down
    let selection : rest =
          sortOn (Down . ucb1 rootVisits . _stats . rootLabel) leaves
    (selection', res) <- select rootVisits selection
    let tree' = Node (State g m (backprop (_turn g) res s)) (selection' : rest)
    return (tree', res)

step :: GameTree -> IO GameTree
step tree = do
  (tree', _) <- select (_visits $ _stats $ rootLabel tree) tree
  return tree'

stepTimes :: Int -> IO GameTree
stepTimes n = foldM ((flip . const) step) initialGameTree (replicate n ())

findBest :: GameTree -> State
findBest tree = leaf
 where
  leaf   = maximumBy (comparing (_visits . _stats)) leaves
  leaves = rootLabel <$> subForest tree

runMCTS' :: Integer -> GameTree -> IO GameTree
runMCTS' timeout tree = if timeout > 0
  then do
    curr  <- getCPUTime
    tree' <- step tree
    curr' <- getCPUTime
    runMCTS' (timeout - curr' + curr) tree'
  else return tree

runMCTS = findBest <$> runMCTS' 10000000000000 initialGameTree
