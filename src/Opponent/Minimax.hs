{-# LANGUAGE NamedFieldPuns #-}
module Opponent.Minimax
  ( minimaxChan
  , minimax
  , eval
  )
where

import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )
import           Game                           ( Game(..)
                                                , Result(Win)
                                                , Message(Start, Move)
                                                )
import           Control.Concurrent.Chan        ( Chan
                                                , readChan
                                                , writeChan
                                                )

eval :: Eq p => Game g m p -> g -> Int
eval g@Game { turn, play, legalMoves, result } game = case result game of
  Nothing      -> -(maximum $ eval g . flip play game <$> legalMoves game)
  Just (Win p) -> if turn game /= p then 1 else -1
  Just _       -> 0

minimax :: Eq p => Game g m p -> g -> IO m
minimax g@Game { play, legalMoves } game =
  return $ maximumBy (comparing (eval g . flip play game)) $ legalMoves game

minimaxChan
  :: Eq p
  => Game g m p
  -> Int
  -> Chan (Message m)
  -> Chan (Message m)
  -> IO ()
minimaxChan g@Game { initialGame, play } _ input output = helper initialGame
 where
  helper game = do
    message <- readChan input
    let game' = case message of
          Start  -> initialGame
          Move m -> play m game
    move <- minimax g game'
    writeChan output (Move move)
    helper (play move game')
    helper (play move game')
