{-# LANGUAGE FlexibleContexts #-}
module GamePlay where

import           Data.Tuple
import           Debug.Trace                    ( traceIO )
import           Game                           ( Game
                                                , Move
                                                , Result
                                                , Player
                                                , result
                                                , initialGame
                                                , play
                                                )

type AIPlayer g = Integer -> g -> IO (Move g)

gamePlay
  :: Game g => Integer -> AIPlayer g -> AIPlayer g -> IO (Result (Player g))
gamePlay seconds =
  let helper game players@(currentPlayer, _) = do
        move <- currentPlayer seconds game
        traceIO (show move)
        let nextGame = play move game
        case result nextGame of
          Nothing  -> helper nextGame $ swap players
          Just res -> return res
  in  curry $ helper initialGame
