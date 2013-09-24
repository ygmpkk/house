module StdFileSelect(selectInputFile, selectOutputFile, selectDirectory) where

--	Clean Object I/O library, version 1.2

import OSFileSelect
import Scheduler
import IOState
import OSEvent

selectInputFile :: ps -> GUI ps (Maybe String, ps)
selectInputFile ps = do
	context <- accIOEnv ioStGetContext
	liftContextIO (\context -> osSelectInputFile (handleOSEvent context)) ps

selectOutputFile :: String -> String -> ps -> GUI ps (Maybe String, ps)
selectOutputFile prompt filename ps = do
	context <- accIOEnv ioStGetContext
	liftContextIO (\context -> osSelectOutputFile (handleOSEvent context) prompt filename) ps

selectDirectory :: ps -> GUI ps (Maybe String, ps)
selectDirectory ps = do
	context <- accIOEnv ioStGetContext
	liftContextIO (\context -> osSelectDirectory (handleOSEvent context)) ps

handleOSEvent :: Context -> OSEvent -> IO ()
handleOSEvent context osEvent = do
	handleContextOSEvent context (ScheduleOSEvent osEvent [])
	return ()
