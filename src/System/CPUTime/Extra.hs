module System.CPUTime.Extra
  ( foldIOTimeout
  )
where

import           System.CPUTime                 ( getCPUTime )

foldIOTimeout :: Integer -> a -> (a -> IO a) -> IO a
foldIOTimeout seconds initial action = helper (seconds * 10 ^ 12) initial
 where
  helper timeout value = if timeout > 0
    then do
      start  <- getCPUTime
      value' <- action value
      end    <- getCPUTime
      helper (timeout - end + start) value'
    else return value
