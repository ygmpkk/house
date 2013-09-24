module World (loadWorld, storeWorld) where

import Data.IORef
import System.IO.Unsafe

world :: IORef Int
world = unsafePerformIO (newIORef 65536)
{-# NOINLINE world #-}

loadWorld  = readIORef world
storeWorld = writeIORef world
