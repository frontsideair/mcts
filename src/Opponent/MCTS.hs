{-# LANGUAGE NamedFieldPuns #-}

module Opponent.MCTS
  ( mcts
  , Params(..)
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
import           Data.Maybe                     ( fromMaybe )
import           Data.Random                    ( sample
                                                , shuffle
                                                , randomElement
                                                )
import           Control.Arrow                  ( second )
import           Game                           ( Game(..)
                                                , Result(Win)
                                                , Message(Start, Move)
                                                , AIPlayer
                                                )
import           Control.Concurrent.Process     ( Process
                                                , readProcess
                                                , writeProcess
                                                )

data Stats = Stats { _wins :: Float, _visits :: Float }

instance Show Stats where
  show (Stats wins visits) =
    show wins ++ "/" ++ show visits ++ "=" ++ show (wins / visits)

data State g m = State { _game :: g, _unplayedMoves :: [m], _stats :: Stats }

type GameTree g m = Tree m (State g m)

initialStats :: Stats
initialStats = Stats 0 0

-- Moves aren't shuffled on initial state, but it's not a problem since direct leaves of root are all expanded
state :: Game g m p -> g -> State g m
state Game { legalMoves } game = State game (legalMoves game) initialStats

gameTree :: Game g m p -> g -> GameTree g m
gameTree g game = node $ state g game

ucb1 :: Float -> Float -> Stats -> Float
ucb1 c parentVisits stats =
  exploit stats + 2 * c * sqrt (log parentVisits / _visits stats)

exploit :: Stats -> Float
exploit (Stats wins visits) = wins / visits

shouldExpand :: GameTree g m -> Bool
shouldExpand (Node (State _ unplayedMoves _) leaves) = not $ null unplayedMoves

expand
  :: (Eq p, Ord m) => Game g m p -> GameTree g m -> IO (GameTree g m, Result p)
expand g@Game { play } (Node (State game unplayedMoves stats) leaves) = do
  result <- simulate g game'
  leaf   <- makeLeaf g game' result
  return
    (Node (State game rest stats) (insert move' (node leaf) leaves), result)
 where
  move' : rest = unplayedMoves -- These are shuffled at initialization
  game'        = play move' game

-- Smart constructor, shuffles moves on initialization
makeLeaf :: Eq p => Game g m p -> g -> Result p -> IO (State g m)
makeLeaf Game { legalMoves, turn } game result = do
  unplayedMoves <- sample $ shuffle $ legalMoves game
  return $ State game unplayedMoves stats
  where stats = backprop (turn game) result initialStats

randomPlay :: Game g m p -> g -> IO g
randomPlay Game { legalMoves, play } game = do
  move <- sample $ randomElement $ legalMoves game
  return $ play move game

simulate :: Game g m p -> g -> IO (Result p)
simulate g@Game { result } game = case result game of
  Nothing     -> randomPlay g game >>= simulate g
  Just result -> return result

backprop :: Eq p => p -> Result p -> Stats -> Stats
backprop player result (Stats win visits) = Stats win' (visits + 1)
 where
  win' = case result of
    Win winner | winner /= player -> win + 1
    _                             -> win

data Selection p = Terminal (Result p) | Expand | Select

selection :: Game g m p -> GameTree g m -> Selection p
selection Game { result } tree@(Node (State game _ _) _) = case result game of
  Just result -> Terminal result
  Nothing     -> if shouldExpand tree then Expand else Select

select
  :: (Ord m, Eq p)
  => Game g m p
  -> Float
  -> GameTree g m
  -> IO (GameTree g m, Result p)
select g@Game { turn } c tree@(Node state leaves) =
  backprop' <$> case selection g tree of
    Terminal result -> return (tree, result)
    Expand          -> expand g tree
    Select          -> do
      let parentVisits = (_visits . _stats) state
      let Just (move, leaf) =
            findMax (ucb1 c parentVisits . _stats . rootLabel) leaves
      (leaf', result) <- select g c leaf
      return (Node state (insert move leaf' leaves), result)
 where
  backprop' (Node (State game moves stats) leaves, result) =
    (Node (State game moves (backprop (turn game) result stats)) leaves, result)

step
  :: (Ord m, Eq p) => Game g m p -> Float -> GameTree g m -> IO (GameTree g m)
step g c tree = do
  (tree', _) <- select g c tree
  return tree'

rootStats :: GameTree g m -> Stats
rootStats = _stats . rootLabel

stats :: GameTree g m -> [(m, Stats)]
stats tree = second (_stats . rootLabel) <$> subForest tree

selectChild :: (Stats -> Float) -> GameTree g m -> m
selectChild strategy tree = move
  where (move, _) = maximumBy (comparing (strategy . snd)) (stats tree)

type Strategy g m = GameTree g m -> m

maxChild :: Strategy g m
maxChild = selectChild exploit

robustChild :: Strategy g m
robustChild = selectChild _visits

mctsPlay
  :: (Ord m, Eq p) => Game g m p -> Float -> Strategy g m -> Int -> g -> IO m
mctsPlay g c strategy iterations game =
  strategy <$> iterateM iterations (step g c) (gameTree g game)

mctsPlay'
  :: (Ord m, Eq p)
  => Game g m p
  -> Float
  -> Strategy g m
  -> Int
  -> GameTree g m
  -> IO (m, GameTree g m)
mctsPlay' g c strategy iterations gameTree = do
  newTree <- iterateM iterations (step g c) gameTree
  return (strategy newTree, newTree)

iterateM :: (Monad m, Integral i) => i -> (a -> m a) -> a -> m a
iterateM 0 f a = return a
iterateM n f a = f a >>= iterateM (n - 1) f

mctsPlayChan :: (Ord m, Eq p) => Float -> Strategy g m -> Int -> AIPlayer g m p
mctsPlayChan c strategy iterations g@Game { play, initialGame } process =
  helper initialGame
 where
  helper game = do
    message <- readProcess process
    let game' = case message of
          Start  -> initialGame
          Move m -> play m game
    move <- mctsPlay g c strategy iterations game'
    writeProcess process (Move move)
    helper (play move game')

mctsPlayChanReuse
  :: (Ord m, Eq p) => Float -> Strategy g m -> Int -> AIPlayer g m p
mctsPlayChanReuse c strategy iterations g@Game { initialGame, play } process =
  helper (gameTree g initialGame)
 where
  helper tree = do
    message <- readProcess process
    let tree' = case message of
          Start  -> gameTree g initialGame
          Move m -> getLeaf' tree m
    (move, tree'') <- mctsPlay' g c strategy iterations tree'
    writeProcess process (Move move)
    helper (getLeaf' tree'' move)
  getLeaf' tree move = fromMaybe
    (gameTree g (play move (_game (rootLabel tree))))
    (getLeaf tree move)

data Params = Params {
  reuse :: Bool,
  iters :: Int,
  constant :: Float,
  robust :: Bool
} deriving Show

mcts :: (Ord m, Eq p) => Params -> AIPlayer g m p
mcts Params { reuse, iters, constant, robust } = mcts' constant strategy iters
 where
  mcts'    = if reuse then mctsPlayChanReuse else mctsPlayChan
  strategy = if robust then robustChild else maxChild
