module GamePlay where

import           Data.Tuple
import           Game                           ( Game
                                                , Result
                                                , Player
                                                , result
                                                , initialGame
                                                )

type AIPlayer g = Integer -> g -> IO g

gamePlay
  :: Game g => Integer -> AIPlayer g -> AIPlayer g -> IO (Result (Player g))
gamePlay seconds =
  let helper game players@(currentPlayer, _) = do
        nextGame <- currentPlayer seconds game
        case result nextGame of
          Nothing  -> helper nextGame $ swap players
          Just res -> return res
  in  curry $ helper initialGame
