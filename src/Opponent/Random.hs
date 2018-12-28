{-# LANGUAGE NamedFieldPuns #-}
module Opponent.Random
  ( randomPlay
  )
where

import           Game                           ( Game(..) )
import           Data.Random                    ( sample
                                                , randomElement
                                                )

randomPlay :: Game g m p -> Integer -> g -> IO m
randomPlay Game { legalMoves } _ game =
  sample $ randomElement $ legalMoves game
