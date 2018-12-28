{-# LANGUAGE NamedFieldPuns #-}
module Opponent.Random
  ( randomPlay
  )
where

import           Game                           ( Game(..) )
import           Data.Random                    ( sample
                                                , randomElement
                                                )

randomPlay :: Game g m p -> Int -> g -> IO m
randomPlay Game { legalMoves } _ game =
  sample $ randomElement $ legalMoves game
