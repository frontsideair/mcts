module Opponent.Random
  ( randomPlay
  )
where

import           Game                           ( Game
                                                , Move
                                                , legalMoves
                                                )
import           Data.Random                    ( sample
                                                , randomElement
                                                )

randomPlay :: Game g => Integer -> g -> IO (Move g)
randomPlay _ game = sample $ randomElement $ legalMoves game
