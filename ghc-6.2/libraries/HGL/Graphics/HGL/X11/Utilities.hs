-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.X11.Utilities
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple graphics library.
--
-----------------------------------------------------------------------------

module Graphics.HGL.X11.Utilities(
	bracket, bracket_,
	safeTry,
        modMVar, modMVar_
	) where

import qualified Control.Exception as E (bracket, try)
import Control.Concurrent( MVar, takeMVar, putMVar )

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

