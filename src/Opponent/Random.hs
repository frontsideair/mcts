{-# LANGUAGE NamedFieldPuns #-}
module Opponent.Random
  ( randomPlayChan
  )
where

import           Game                           ( Game(..)
                                                , Message(Start, Move)
                                                , AIPlayer
                                                )
import           Data.Random                    ( sample
                                                , randomElement
                                                )
import           Control.Concurrent.Process     ( readProcess
                                                , writeProcess
                                                )

randomPlay :: Game g m p -> g -> IO m
randomPlay Game { legalMoves } game = sample $ randomElement $ legalMoves game

randomPlayChan :: (Ord m, Eq p) => AIPlayer g m p
randomPlayChan g@Game { play, initialGame, legalMoves } process = helper
  initialGame
 where
  helper game = do
    message <- readProcess process
    let game' = case message of
          Start  -> initialGame
          Move m -> play m game
    move <- randomPlay g game
    writeProcess process (Move move)
    helper (play move game')
