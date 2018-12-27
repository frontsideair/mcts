module Opponent.MCTS
  ( mctsPlayChan
  , mctsPlayChanReuse
  )
where

import           Data.Tree                      ( Tree(Node)
                                                , node
                                                , rootLabel
                                                , subForest
                                                , findMax
                                                , insert
                                                , getLeaf
                                                )
import           Data.Ord                       ( comparing )
import           Data.List                      ( maximumBy )
import           Data.Random                    ( sample
                                                , shuffle
                                                , randomElement
                                                )
import           System.CPUTime.Extra           ( foldIOTimeout )
import           Control.Arrow                  ( second )
import           Game                           ( Game
                                                , Move
                                                , Result(Win)
                                                , Player
                                                , legalMoves
                                                , initialGame
                                                , play
                                                , turn
                                                , result
                                                , Message(Start, Move)
                                                )
import           Control.Concurrent.Chan        ( Chan
                                                , readChan
                                                , writeChan
                                                )

data Stats = Stats { _wins :: Float, _visits :: Float }

instance Show Stats where
  show (Stats wins visits) = show wins ++ "/" ++ show visits ++ "=" ++ show (wins / visits)

data State g = State { _game :: g, _unplayedMoves :: [Move g], _stats :: Stats }

type GameTree g = Tree (Move g) (State g)

initialStats :: Stats
initialStats = Stats 0 0

-- Moves aren't shuffled on initial state, but it's not a problem since direct leaves of root are all expanded
state :: Game g => g -> State g
state game = State game (legalMoves game) initialStats

gameTree :: Game g => g -> GameTree g
gameTree game = node $ state game

ucb1 :: Float -> Stats -> Float
ucb1 rootVisits stats =
  exploit stats + sqrt (2 * log rootVisits / _visits stats)

exploit :: Stats -> Float
exploit (Stats wins visits) = wins / visits

shouldExpand :: GameTree g -> Bool
shouldExpand (Node (State _ unplayedMoves _) leaves) = not $ null unplayedMoves

expand :: Game g => GameTree g -> IO (GameTree g, Result (Player g))
expand (Node (State game unplayedMoves stats) leaves) = do
  result <- simulate game'
  leaf   <- makeLeaf game' result
  return
    (Node (State game rest stats) (insert move' (node leaf) leaves), result)
 where
  move' : rest = unplayedMoves -- These are shuffled at initialization
  game'        = play move' game

-- Smart constructor, shuffles moves on initialization
makeLeaf :: Game g => g -> Result (Player g) -> IO (State g)
makeLeaf game result = do
  unplayedMoves <- sample $ shuffle $ legalMoves game
  return $ State game unplayedMoves stats
  where stats = backprop (turn game) result initialStats

randomPlay :: Game g => g -> IO g
randomPlay game = do
  move <- sample $ randomElement $ legalMoves game
  return $ play move game

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
  Terminal result -> return (tree, result)
  Expand          -> expand tree
  Select          -> do
    let Just (move, leaf) = findMax (ucb1 rv . _stats . rootLabel) leaves
    (leaf', result) <- select rv leaf
    return (Node state (insert move leaf' leaves), result)
 where
  backprop' (Node (State game moves stats) leaves, result) =
    (Node (State game moves (backprop (turn game) result stats)) leaves, result)

step :: Game g => GameTree g -> IO (GameTree g)
step tree = do
  (tree', _) <- select (_visits $ _stats $ rootLabel tree) tree
  return tree'

rootStats :: GameTree g -> Stats
rootStats = _stats . rootLabel

stats :: GameTree g -> [(Move g, Stats)]
stats tree = second (_stats . rootLabel) <$> subForest tree

selectChild :: (Stats -> Float) -> GameTree g -> Move g
selectChild strategy tree = move
  where (move, _) = maximumBy (comparing (strategy . snd)) (stats tree)

maxChild :: GameTree g -> Move g
maxChild = selectChild exploit

robustChild :: GameTree g -> Move g
robustChild = selectChild _visits

mctsPlay :: Game g => Integer -> g -> IO (Move g)
mctsPlay iterations game =
  robustChild <$> iterateM iterations (gameTree game) step

mctsPlay' :: Game g => Integer -> GameTree g -> IO (Move g, GameTree g)
mctsPlay' iterations gameTree = do
  newTree <- iterateM iterations gameTree step
  return (robustChild newTree, newTree)

iterateM :: Monad m => Integer -> a -> (a -> m a) -> m a
iterateM 0 a f = return a
iterateM n a f = do
  a' <- f a
  iterateM (n - 1) a' f

mctsPlayChan
  :: Game g
  => Integer
  -> Chan (Message (Move g))
  -> Chan (Message (Move g))
  -> IO ()
mctsPlayChan iterations input output = helper initialGame
 where
  helper game = do
    message <- readChan input
    let game' = case message of
          Start  -> initialGame
          Move m -> play m game
    move <- mctsPlay iterations game'
    writeChan output (Move move)
    helper (play move game')

mctsPlayChanReuse
  :: Game g
  => Integer
  -> Chan (Message (Move g))
  -> Chan (Message (Move g))
  -> IO ()
mctsPlayChanReuse iterations input output = helper (gameTree initialGame)
 where
  helper tree = do
    message <- readChan input
    let tree' = case message of
          Start  -> gameTree initialGame
          Move m -> getLeaf tree m
    (move, tree'') <- mctsPlay' iterations tree'
    writeChan output (Move move)
    helper (getLeaf tree'' move)
