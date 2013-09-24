module GraphicsFlag
	( Flag, newFlag, setFlag, resetFlag
	) where

import Concurrent
	( MVar, newEmptyMVar, newMVar, takeMVar, putMVar
	)

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

-- We maintain a list of blocked processes.
-- Blocked processes are "stored" in MVars; the outer MVar
-- is used to implement a critical section.
newtype Flag a = Flag (MVar [MVar a])

newFlag   :: IO (Flag a)
-- sets the flag, never blocks, never fails
setFlag   :: Flag a -> a -> IO ()
-- block until the flag is set (and reset it)
resetFlag :: Flag a -> IO a

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

newFlag = do
  queue <- newMVar []
  return (Flag queue)

setFlag (Flag queue) a = do
  ps <- takeMVar queue
  mapM_ (\ p -> putMVar p a) ps
  putMVar queue []

resetFlag (Flag queue) = do
  ps <- takeMVar queue
  p  <- newEmptyMVar 
  putMVar queue (p:ps)
  takeMVar p             -- block

----------------------------------------------------------------
-- End
----------------------------------------------------------------
