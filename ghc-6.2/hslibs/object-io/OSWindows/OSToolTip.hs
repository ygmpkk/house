module OSToolTip(osRemoveControlToolTip, osAddControlToolTip) where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	ControlCreate contains all control creation functions.
--	********************************************************************************


--	Operations to add and remove tooltip controls and areas.

{-	Tooltip controls are added and removed by OSaddControlTooltip and OSremoveControlTooltip.
	The first  OSWindowPtr argument identifies the parent window.
	The second OSWindowPtr argument identifies the control.
	The String argument is the tooltip text.
-}

import	ClCrossCall_12
import	OSTypes(OSWindowPtr)
import  Cutil_12(addr2int, newCString, free)

osIgnoreCallback :: CrossCallInfo -> IO CrossCallInfo
osIgnoreCallback _
	= return return0Cci

osAddControlToolTip :: OSWindowPtr -> OSWindowPtr -> String -> IO ()
osAddControlToolTip parentPtr controlPtr tip = do
	textptr <- newCString tip
	let cci = rq3Cci ccRqADDCONTROLTIP parentPtr controlPtr (addr2int textptr)
	issueCleanRequest2 osIgnoreCallback cci
	free textptr
		

osRemoveControlToolTip :: OSWindowPtr -> OSWindowPtr -> IO ()
osRemoveControlToolTip parentPtr controlPtr = do
	issueCleanRequest2 osIgnoreCallback (rq2Cci ccRqDELCONTROLTIP parentPtr controlPtr)
	return ()
