{-# LANGUAGE RecursiveDo #-}

module Control.Concurrent.Process
  ( Process
  , forkProcess
  , readProcess
  , writeProcess
  , killProcess
  )
where

import           Control.Concurrent             ( forkIO
                                                , killThread
                                                , ThreadId
                                                )
import           Control.Concurrent.Chan        ( Chan
                                                , newChan
                                                , readChan
                                                , writeChan
                                                )

newtype Process a b = Process (Chan a, Chan b, ThreadId)

forkProcess :: (Process a b -> IO ()) -> IO (Process b a)
forkProcess f = do
  inChan  <- newChan
  outChan <- newChan
  rec threadId <- forkIO $ f (Process (inChan, outChan, threadId))
  return (Process (outChan, inChan, threadId))

readProcess :: Process a b -> IO a
readProcess (Process (a, b, t)) = readChan a

writeProcess :: Process a b -> b -> IO ()
writeProcess (Process (a, b, t)) = writeChan b

killProcess :: Process a b -> IO ()
killProcess (Process (a, b, t)) = killThread t
