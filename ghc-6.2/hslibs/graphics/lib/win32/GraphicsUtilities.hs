module GraphicsUtilities(
	bracket, bracket_,
	safeTry,
 	E.Exception,
	) where

import qualified Exception as E (bracket, try, Exception)

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket = E.bracket

-- Not exactly the same type as GHC's bracket_
bracket_ :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ left right m = bracket left right (const m)

safeTry = E.try

