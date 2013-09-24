module StdProcess (module StdProcess, module StdProcessDef) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdProcess contains the process creation and manipulation functions.
--	********************************************************************************

import CommonDef (dumpFatalError, Rect(..), rectSize)
import IOState
import ProcessDevice
import Scheduler
import StdProcessDef
import OSSystem(osGetProcessWindowDimensions)


stdprocessFatalError :: String -> String -> x
stdprocessFatalError function error
	= dumpFatalError function "StdProcess" error


--	General process topology creation functions:

class Processes pdef where
	startProcesses :: pdef -> IO ()
	openProcesses  :: pdef -> GUI ps ()

instance Processes Process where
	startProcesses (Process xDI ps_init io_init atts)
		= do {
			initialContext  <- initContext atts io_init1 ps_init xDI;			
			(case initialContext of
				Just context -> handleEvents context >> closeContext context
				initFailed   -> stdprocessFatalError "startProcesses" "should not be evaluated inside GUI context");
		  }
		where
			io_init1 ps = dOpen processFunctions ps >>= io_init
	
	openProcesses (Process xDI ps_init io_init atts)
		= addInteractiveProcess atts io_init1 ps_init xDI
		where
			io_init1 ps = dOpen processFunctions ps >>= io_init


instance Processes pdef => Processes [pdef] where
	startProcesses []
		= return ()
	startProcesses pdefs
		= do {			
			initialContext  <- initContext [] io_init1 () NDI;
			(case initialContext of
				Just context -> handleEvents context >> closeContext context
				initFailed   -> stdprocessFatalError "startProcesses" "should not be evaluated inside GUI context");
		  }
		where
			io_init1 ps = dOpen processFunctions ps >>= (\ps1 -> openProcesses pdefs >> closeProcess ps1)

	openProcesses pdefs
		= sequence_ (map openProcesses pdefs)


--	Specialised process creation functions:

startIO :: DocumentInterface -> ps -> ProcessInit ps -> [ProcessAttribute ps] -> IO ()
startIO xDI ps io_init atts
	= startProcesses (Process xDI ps io_init atts)


--	Close this interactive process.

closeProcess :: ps -> GUI ps ps
closeProcess ps = quitProcess ps

--	Get the current position of the ProcessWindow (on Macintosh: zero)

getProcessWindowPos :: GUI ps Point2
getProcessWindowPos = do	
	osdinfo <- accIOEnv ioStGetOSDInfo
	rect <- liftIO (osGetProcessWindowDimensions osdinfo)
	return (Point2{x=rleft rect,y=rtop rect})

--	Get the current size of the ProcessWindow (on Macintosh: ScreenSize)

getProcessWindowSize :: GUI ps Size
getProcessWindowSize = do
	osdinfo <- accIOEnv ioStGetOSDInfo
	rect <- liftIO (osGetProcessWindowDimensions osdinfo)	
	return (rectSize rect)