module StdSound(playSoundFile) where


--	********************************************************************************
--	Clean Standard Object I/O library, version 1.2
--
--	StdSound specifies sound playing functions.
--	********************************************************************************


import 	ClCCall_12(winPlaySound)

playSoundFile :: String -> IO Bool
playSoundFile soundFileName = winPlaySound soundFileName
