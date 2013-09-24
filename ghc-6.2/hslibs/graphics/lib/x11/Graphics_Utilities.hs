module Graphics_Utilities(
	bracket, bracket_,
	safeTry,
        modMVar, modMVar_
	) where

import qualified Exception as E (bracket, try)
import Concurrent( MVar, takeMVar, putMVar )

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket = E.bracket

-- Not exactly the same type as GHC's bracket_
bracket_ :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ left right m = bracket left right (const m)

safeTry = E.try

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

modMVar :: MVar a -> (a -> a) -> IO a
modMVar mv f = do
  x <- takeMVar mv
  putMVar mv (f x)
  return x

modMVar_ :: MVar a -> (a -> a) -> IO ()
modMVar_ mv f = do
  x <- takeMVar mv
  putMVar mv (f x)

