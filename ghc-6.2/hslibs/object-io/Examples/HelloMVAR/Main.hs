module Main (main) where

import CleanStdMisc
import StdIO

main = startIO NDI init []
	where
		init = do {	error <- openDialog ddef;
				if   error/=NoError
				then abort "main could not open Dialog."
				else return ()
			  }
		ddef = Dialog "My first Haskell monadic GUI"
			(   TextControl "Hello..." []
			:+: ButtonControl "World!" []--ControlFunction closeProcess]
			)   [WindowClose closeProcess]
