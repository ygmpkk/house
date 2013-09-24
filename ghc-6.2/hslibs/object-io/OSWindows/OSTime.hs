module OSTime where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	This is a new module that provides similar functionality as StdPSt in the Clean
--	Object I/O library.
--	********************************************************************************


type OSTime = Int

osMaxTime :: OSTime
osMaxTime = 2^31-1

foreign import "WinGetTickCount" osGetTime ::  IO OSTime
foreign import "WinGetBlinkTime" osGetBlinkInterval :: IO Int