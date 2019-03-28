{-# LANGUAGE NamedFieldPuns #-}

module Opponent.Minimax
  ( minimaxChan
  )
where

import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )
import           Game                           ( Game(..)
                                                , Result(Win, Draw)
                                                , Message(Start, Move)
                                                , AIPlayer
                                                )
import           Control.Concurrent.Process     ( Process
                                                , readProcess
                                                , writeProcess
                                                )

eval :: Eq p => Game g m p -> g -> Int
eval g@Game { turn, play, legalMoves, result } game = case result game of
  Nothing      -> -(maximum $ eval g . flip play game <$> legalMoves game)
  Just (Win p) -> if turn game /= p then 1 else -1
  Just Draw    -> 0

minimax :: Eq p => Game g m p -> g -> IO m
minimax g@Game { play, legalMoves } game =
  return $ maximumBy (comparing (eval g . flip play game)) $ legalMoves game

minimaxChan :: Eq p => AIPlayer g m p
minimaxChan g@Game { initialGame, play } process = helper initialGame
 where
  helper game = do
    message <- readProcess process
    let game' = case message of
          Start  -> initialGame
          Move m -> play m game
    move <- minimax g game'
    writeProcess process (Move move)
    helper (play move game')
