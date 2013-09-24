module ClCrossCall_12 ( module ClCrossCall_12 ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--
--	ClCrossCall_12 contains the operations to communicate between Clean and OS
--	thread.
--	********************************************************************************


import OSTypes
import Cutil_12
import System.IO.Unsafe
import Data.IORef


--	********************************************************************************
--	Crosscall infrastructure
--	********************************************************************************

--	CrossCallInfo is the basic record that is passed between the Clean thread and the OS thread:
data	CrossCallInfo
	= CrossCallInfo
		{ ccMsg :: !Int		-- The message nr: Clean->OS use ccRq...; OS->Clean use ccWm...
		, p1    :: !Int
		, p2    :: !Int
		, p3    :: !Int
		, p4    :: !Int
		, p5    :: !Int
		, p6    :: !Int
		}


--	2 versions of IssueCleanRequest: first with state parameter, second without.

issueCleanRequest :: (CrossCallInfo -> s -> IO (CrossCallInfo,s))
                   -> CrossCallInfo -> s -> IO (CrossCallInfo,s)
issueCleanRequest callback cci s
	= do {
		reply <- winKickOsThread cci;
		handleCallBacks callback reply s
	  }
	where
		handleCallBacks :: (CrossCallInfo -> s -> IO (CrossCallInfo,s))
		                 -> CrossCallInfo -> s -> IO (CrossCallInfo,s)
		handleCallBacks callback cci s
			| ccMsg cci>2000
				= error ("handleCallBacks " ++ show (ccMsg cci))
			| isReturnOrQuitCci (ccMsg cci)
				= return (cci,s)
			| otherwise
				= do {
					(returnCci,s1) <- callback cci s;
					replyCci       <- winKickOsThread returnCci;
					handleCallBacks callback replyCci s1
				  }

issueCleanRequest2 :: (CrossCallInfo -> IO CrossCallInfo)
                    -> CrossCallInfo -> IO CrossCallInfo
issueCleanRequest2 callback cci
	= do {
		reply <- winKickOsThread cci;
		handleCallBacks callback reply;
	  }
	where
		handleCallBacks :: (CrossCallInfo -> IO CrossCallInfo)
		                 -> CrossCallInfo -> IO CrossCallInfo
		handleCallBacks callback cci
			| ccMsg cci>2000
				= error ("handleCallBacks " ++ show (ccMsg cci))
			| isReturnOrQuitCci (ccMsg cci)
				= return cci
			| otherwise
				= do {
					returnCci <- callback cci;
					replyCci  <- winKickOsThread returnCci;
					handleCallBacks callback replyCci;
				  }

--	Functions for returning proper number of arguments within a CrossCallInfo.
rq0Cci msg = CrossCallInfo {ccMsg=msg,p1=0,p2=0,p3=0,p4=0,p5=0,p6=0}
rq1Cci msg v1 = CrossCallInfo {ccMsg=msg,p1=v1,p2=0,p3=0,p4=0,p5=0,p6=0}
rq2Cci msg v1 v2 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=0,p4=0,p5=0,p6=0}
rq3Cci msg v1 v2 v3 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=0,p5=0,p6=0}
rq4Cci msg v1 v2 v3 v4 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=v4,p5=0,p6=0}
rq5Cci msg v1 v2 v3 v4 v5 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=v4,p5=v5,p6=0}
rq6Cci msg v1 v2 v3 v4 v5 v6 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=v4,p5=v5,p6=v6}

return0Cci :: CrossCallInfo
return0Cci = rq0Cci ccRETURN0

return1Cci :: Int -> CrossCallInfo
return1Cci v = rq1Cci ccRETURN1 v

return2Cci :: Int -> Int -> CrossCallInfo
return2Cci v1 v2 = rq2Cci ccRETURN2 v1 v2

return3Cci :: Int -> Int -> Int -> CrossCallInfo
return3Cci v1 v2 v3 = rq3Cci ccRETURN3 v1 v2 v3

return4Cci :: Int -> Int -> Int -> Int -> CrossCallInfo
return4Cci v1 v2 v3 v4 = rq4Cci ccRETURN4 v1 v2 v3 v4

return5Cci :: Int -> Int -> Int -> Int -> Int -> CrossCallInfo
return5Cci v1 v2 v3 v4 v5 = rq5Cci ccRETURN5 v1 v2 v3 v4 v5

return6Cci :: Int -> Int -> Int -> Int -> Int -> Int -> CrossCallInfo
return6Cci v1 v2 v3 v4 v5 v6 = rq6Cci ccRETURN6 v1 v2 v3 v4 v5 v6

isReturnOrQuitCci :: Int -> Bool
isReturnOrQuitCci mess
	= mess==ccWASQUIT || (mess<=ccRETURNmax && mess>=ccRETURNmin)


{-	Two error callback routines that do not nothing. They can be used
	conveniently as argument of issueCleanRequest(2).
-}
errorCallback :: String -> CrossCallInfo -> s -> IO (CrossCallInfo,s)
errorCallback source cci s = return (return0Cci,s)

errorCallback2 :: String -> CrossCallInfo -> IO CrossCallInfo
errorCallback2 source cci = return return0Cci


--	********************************************************************************
--	Synchronisation operations between the Clean thread and OS thread.
--	********************************************************************************

{-# NOINLINE gEventsInited #-}
gEventsInited = unsafePerformIO (newIORef False) :: IORef Bool

osInitToolbox :: IO Bool
osInitToolbox = do
	eventsInited <- readIORef gEventsInited
	if not eventsInited
	  then do
		winStartOsThread
		osInstallFont
		osInitialiseFileSelectors
		osInstallMenus
		osInstallClipboard
		osInstallCursor
		osInstallPrinter
		osInstallWindows
		osInstallDI
		writeIORef gEventsInited True
		return True
	  else return False
	  
osCloseToolbox = do
	eventsInited <- readIORef gEventsInited
	if eventsInited
	  then do
	  	winKillOsThread
		writeIORef gEventsInited False
		return True
	  else return False

foreign import stdcall "cCrossCallFont_121.h 		InstallCrossCallFont" 		osInstallFont :: IO ()
foreign import stdcall "cCrossCallFileSelectors_121.h 	InstallCrossCallFileSelectors" 	osInitialiseFileSelectors :: IO ()
foreign import stdcall "cCrossCallMenus_121.h     	InstallCrossCallMenus" 		osInstallMenus :: IO ()
foreign import stdcall "cCrossCallClipboard_121.h 	InstallCrossCallClipboard" 	osInstallClipboard :: IO ()
foreign import stdcall "cCrossCallCursor_121.h    	InstallCrossCallCursor" 	osInstallCursor :: IO ()
foreign import stdcall "cCrossCallPrinter_121.h   	InstallCrossCallPrinter" 	osInstallPrinter :: IO ()
foreign import stdcall "cCrossCallWindows_121.h   	InstallCrossCallWindows" 	osInstallWindows :: IO ()
foreign import stdcall "cCrossCallxDI_121.h		InstallCrossCallxDI" 		osInstallDI :: IO ()

winKickOsThread :: CrossCallInfo -> IO CrossCallInfo
winKickOsThread cci@(CrossCallInfo {ccMsg=ccMsg,p1=p1,p2=p2,p3=p3,p4=p4,p5=p5,p6=p6})
	= do {
		-- Marshal out parameters:
		omess <- malloc;
		op1 <- malloc;
		op2 <- malloc;
		op3 <- malloc;
		op4 <- malloc;
		op5 <- malloc;
		op6 <- malloc;
		-- Call C routine:
		cWinKickOsThread ccMsg p1 p2 p3 p4 p5 p6 omess op1 op2 op3 op4 op5 op6;
		-- Read/free:
		omess' <- fpeek omess;
		op1' <- fpeek op1;
		op2' <- fpeek op2;
		op3' <- fpeek op3;
		op4' <- fpeek op4;
		op5' <- fpeek op5;
		op6' <- fpeek op6;
		return (CrossCallInfo omess' op1' op2' op3' op4' op5' op6')
	  }
foreign import stdcall "WinKickOsThread" cWinKickOsThread :: Int -> Int -> Int -> Int -> Int -> Int -> Int
                                   -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

foreign import stdcall "WinKillOsThread" winKillOsThread :: IO ()
foreign import stdcall "WinStartOsThread" winStartOsThread :: IO ()


--	********************************************************************************
--	The message numbers for communication from Clean/Haskell to OS (ccMsg field)
--	********************************************************************************

{- Game cross call codes -}
ccRqUSERGAMEEVENT		= 1905
ccRqCREATEGAMEOBJECT		= 1904
ccRqPLAYSOUNDSAMPLE		= 1903

ccRqRUNGAME			= 1901
ccRqCREATEGAMEWINDOW		= 1900

{- Print cross call codes -}
ccRqDO_PRINT_SETUP		= 1828
ccRqDO_HTML_HELP		= 1827

ccRqGET_PRINTER_DC		= 1824
ccRqDISPATCH_MESSAGES_WHILE_PRINTING
				= 1823
ccRqENDDOC			= 1822
ccRqSTARTDOC			= 1821

{- TCP cross call codes -}
ccRqCREATETCPWINDOW		= 1820		{- Create TCP window -}

{- GUI cross call codes -}
ccRqDESTROYMDIDOCWINDOW 	= 1817		{- Destroy MDI document window -}
ccRqCREATESDIDOCWINDOW		= 1816		{- Create SDI document window  -}
ccRqCREATEMDIDOCWINDOW		= 1815		{- Create MDI document window  -}
ccRqCREATEMDIFRAMEWINDOW	= 1814		{- Create MDI frame window     -}
ccRqCREATESDIFRAMEWINDOW	= 1813		{- Create SDI frame window     -}
ccRqCLIPBOARDHASTEXT		= 1812
ccRqGETCLIPBOARDTEXT		= 1811
ccRqSETCLIPBOARDTEXT		= 1810
ccRqGETCLIPBOARDCOUNT		= 1809		{- Retrieve clipboard count. -}

ccRqDIRECTORYDIALOG		= 1802		{- Create directory selector dialog. -}
ccRqFILESAVEDIALOG		= 1801
ccRqFILEOPENDIALOG		= 1800

ccRqUPDATEDESKTOP		= 1790		{- Force refresh of desktop. -}

ccRqSHOWCONTROL			= 1755
ccRqSELECTPOPUPITEM		= 1754
ccRqENABLEPOPUPITEM		= 1753
ccRqADDTOPOPUP			= 1752
ccRqSETITEMCHECK		= 1751
ccRqENABLECONTROL		= 1750

ccRqCREATECOMPOUND		= 1729
ccRqCREATESCROLLBAR		= 1728
ccRqCREATECUSTOM		= 1727
ccRqCREATEICONBUT		= 1726
ccRqCREATEPOPUP			= 1725
ccRqCREATECHECKBOX		= 1724
ccRqCREATERADIOBUT		= 1723
ccRqCREATEEDITTXT		= 1722
ccRqCREATESTATICTXT		= 1721
ccRqCREATEBUTTON		= 1720

ccRqCREATEMODALDIALOG		= 1701		{- Create modal dialog. -}
ccRqCREATEDIALOG		= 1700

ccRqCREATETOOLBARSEPARATOR	= 1603		{- Create a toolbar separator item.    -}
ccRqCREATETOOLBARITEM		= 1602		{- Create a toolbar bitmap item.       -}
ccRqCREATEMDITOOLBAR		= 1601		{- Create a toolbar for a MDI process. -}
ccRqCREATESDITOOLBAR		= 1600		{- Create a toolbar.                   -}

ccCbFONTSIZE			= 1530

ccCbFONTNAME			= 1520

ccRqGETFONTSIZES		= 1510

ccRqGETFONTNAMES		= 1500

ccRqSETCLIENTSIZE		= 1438		{- Set client size.                    -}
ccRqDELCONTROLTIP		= 1437		{- Remove controls from tooltip areas. -}
ccRqADDCONTROLTIP		= 1436		{- Add controls to tooltip areas.      -}
ccRqGETWINDOWSIZE		= 1435		{- Retrieve bounding size of windows.  -}
ccRqRESTACKWINDOW		= 1434		{- Restack windows.                    -}
ccRqSHOWWINDOW			= 1433		{- (hide/show) windows.                -}
ccRqSETWINDOWSIZE		= 1432		{- Resize windows/controls.            -}
ccRqSETSELECTWINDOW		= 1431		{- (en/dis)able windows.               -}
ccRqSETWINDOWPOS		= 1430		{- Move windows/controls.              -}

ccRqSETEDITSELECTION		= 1428		{- Handling edit control selections. -}
ccRqSETSCROLLSIZE		= 1427		{- Setting thumb size of scrollbar.  -}
ccRqSETSCROLLPOS		= 1426		{- Setting thumb of scrollbar.       -}
ccRqSETSCROLLRANGE		= 1425		{- Setting range of scrollbar.       -}
ccRqRESETCURSOR			= 1424
ccRqSETGLOBALCURSOR		= 1423
ccRqOBSCURECURSOR		= 1422
ccRqCHANGEWINDOWCURSOR		= 1421
ccRqACTIVATEWINDOW		= 1420		{- Activating window.   -}
ccRqACTIVATECONTROL		= 1419		{- Activating controls. -}

ccRqCREATECARET			= 1610
ccRqSETCARETPOS			= 1611
ccRqDESTROYCARET		= 1612

ccRqGETWINDOWPOS		= 1416
ccRqGETCLIENTSIZE		= 1415

ccRqUPDATEWINDOWRECT		= 1412		{- Updating rect part of a window/control. -}
ccRqGETWINDOWTEXT		= 1411
ccRqSETWINDOWTITLE		= 1410

ccRqFAKEPAINT			= 1405		{- Combination of BeginPaint; EndPaint; InvalidateRect; -}
ccRqENDPAINT			= 1404
ccRqBEGINPAINT			= 1403
ccRqDESTROYWINDOW		= 1402
ccRqDESTROYMODALDIALOG		= 1401		{- Destroy modal dialog. -}

ccRqDRAWMBAR			= 1265

ccRqTRACKPOPMENU		= 1256		{- Handling pop up menu. -}
ccRqCREATEPOPMENU		= 1255

ccRqINSERTSEPARATOR		= 1245

ccRqMENUENABLE			= 1235

ccRqMODIFYMENU			= 1230

ccRqINSERTMENU			= 1226		{- Inserting a new menu into the menu bar -}

ccRqITEMENABLE			= 1220

ccRqREMOVEMENUSHORTKEY		= 1217		{- Removing a shortkey of a menu item -}
ccRqADDMENUSHORTKEY		= 1216		{- Adding a shortkey of a menu item   -}
ccRqMODIFYMENUITEM		= 1215
ccRqDESTROYMENU			= 1214		{- Destroying a menu 'physically' -}
ccRqDELETEMENU			= 1213		{- Deleting a menu logically      -}
ccRqREMOVEMENUITEM		= 1212

ccRqCHECKMENUITEM		= 1210

ccRqINSERTMENUITEM		= 1205

ccRqDOMESSAGE			= 1100

----------------------------------------------------------------------------
--  The message numbers for communication from OS to Clean (ccMsg field)  --
----------------------------------------------------------------------------
ccWINMESSmax			= 999

{- Game cross calls: 500-599 -}
ccWmCHECKQUIT			= 513		{- Mike: check user's quit function   -}
ccWmUSEREVENT			= 512		{- Mike: user defined event           -}
ccWmSTATISTICS			= 511		{- Mike: request for statistics       -}
ccWmOBJECTKEYUP			= 510		{- Mike: key released                 -}
ccWmOBJECTKEYDOWN		= 509		{- Mike: key pressed for object       -}
ccWmOBJECTTIMER			= 508		{- Mike: framecounter reached 0       -}
ccWmANIMATION			= 507		{- Mike: animation sequence ended     -}
ccWmCOLLISION			= 506		{- Mike: collision of two objects     -}
ccWmTOUCHBOUND			= 505		{- Mike: object touches bound or code -}
ccWmOBJECTDONE			= 504		{- Mike: object is destroyed          -}
ccWmMOVEOBJECT			= 503		{- Mike: move object                  -}
ccWmINITOBJECT			= 502		{- Mike: initialize new object        -}
ccWmSCROLL			= 501		{- Mike: calculate layer position     -}
ccWmGAMEKEYBOARD		= 500		{- Mike: keyboard input for game      -}

{- TCP cross calls -}
ccWmINETEVENT			= 140

{- GUI cross calls -}
ccWmZEROTIMER			= 136		{- Sequence of zero timer events (generated only by Clean). -}
ccWmLOSTKEY			= 135		{- Loosing keyboard input (generated only by Clean). -}
ccWmLOSTMOUSE			= 134		{- Loosing mouse input    (generated only by Clean). -}
ccWmSPECIALBUTTON		= 133		{- Info about OK/CANCEL button selected. -}
ccWmPROCESSDROPFILES		= 132		{- Requesting opening of files. -}
ccWmGETTOOLBARTIPTEXT		= 131		{- Getting tooltip text. -}
ccWmSETFOCUS			= 130		{- Notifying obtaining keyboard input focus. -}
ccWmKILLFOCUS			= 129		{- Notifying loss of keyboard input focus. -}

ccWmPROCESSCLOSE		= 127		{- Requesting closing of process. -}
ccWmDRAWCLIPBOARD		= 126		{- Clipboard handling. Copied from Ronny. -}
ccWmGETSCROLLBARINFO		= 125		{- Info about scrollbars. -}
ccWmSCROLLBARACTION		= 124		{- Scrollbar handling. -}
ccWmDDEEXECUTE			= 123

ccWmIDLEDIALOG			= 121		{- Initialising modal dialogues. -}
ccWmDRAWCONTROL			= 120
ccWmCOMBOSELECT			= 119
ccWmBUTTONCLICKED		= 118
ccWmINITDIALOG			= 117
ccWmIDLETIMER			= 116
ccWmTIMER			= 115
ccWmNEWVTHUMB			= 114
ccWmNEWHTHUMB			= 113
ccWmGETVSCROLLVAL		= 112
ccWmGETHSCROLLVAL		= 111
ccWmSIZE			= 110		{- Passing resize information. -}
ccWmMOUSE			= 109
ccWmKEYBOARD			= 108
ccWmDEACTIVATE			= 107
ccWmACTIVATE			= 106
ccWmCLOSE			= 105
ccWmCOMMAND			= 103
ccWmCHAR			= 102
ccWmCREATE			= 101
ccWmPAINT			= 100

ccWINMESSmin			= 100

ccWmNOTIFY			= 78

ccRETURNmax			= 19

ccRETURN6			= 16
ccRETURN5			= 15
ccRETURN4			= 14
ccRETURN3			= 13
ccRETURN2			= 12
ccRETURN1			= 11
ccRETURN0			= 10

ccRETURNmin			= 10

ccWASQUIT			= 1

{-	All of the above ccXXX values are supposed to be Int.
-}
ccRqUSERGAMEEVENT,
	ccRqCREATEGAMEOBJECT,
	ccRqPLAYSOUNDSAMPLE,
	ccRqRUNGAME,
	ccRqCREATEGAMEWINDOW,
	ccRqDO_PRINT_SETUP,
	ccRqDO_HTML_HELP,
	ccRqGET_PRINTER_DC,
	ccRqDISPATCH_MESSAGES_WHILE_PRINTING,
	ccRqENDDOC,
	ccRqSTARTDOC,
	ccRqCREATETCPWINDOW,
	ccRqDESTROYMDIDOCWINDOW,
	ccRqCREATESDIDOCWINDOW,
	ccRqCREATEMDIDOCWINDOW,
	ccRqCREATEMDIFRAMEWINDOW,
	ccRqCREATESDIFRAMEWINDOW,
	ccRqCLIPBOARDHASTEXT,
	ccRqGETCLIPBOARDTEXT,
	ccRqSETCLIPBOARDTEXT,
	ccRqGETCLIPBOARDCOUNT,
	ccRqDIRECTORYDIALOG,
	ccRqFILESAVEDIALOG,
	ccRqFILEOPENDIALOG,
	ccRqUPDATEDESKTOP,
	ccRqSHOWCONTROL,
	ccRqSELECTPOPUPITEM,
	ccRqENABLEPOPUPITEM,
	ccRqADDTOPOPUP,
	ccRqSETITEMCHECK,
	ccRqENABLECONTROL,
	ccRqCREATECOMPOUND,
	ccRqCREATESCROLLBAR,
	ccRqCREATECUSTOM,
	ccRqCREATEICONBUT,
	ccRqCREATEPOPUP,
	ccRqCREATECHECKBOX,
	ccRqCREATERADIOBUT,
	ccRqCREATEEDITTXT,
	ccRqCREATESTATICTXT,
	ccRqCREATEBUTTON,
	ccRqCREATEMODALDIALOG,
	ccRqCREATEDIALOG,
	ccRqCREATETOOLBARSEPARATOR,
	ccRqCREATETOOLBARITEM,
	ccRqCREATEMDITOOLBAR,
	ccRqCREATESDITOOLBAR,
	ccCbFONTSIZE,
	ccCbFONTNAME,
	ccRqGETFONTSIZES,
	ccRqGETFONTNAMES,
	ccRqSETCLIENTSIZE,
	ccRqDELCONTROLTIP,
	ccRqADDCONTROLTIP,
	ccRqGETWINDOWSIZE,
	ccRqRESTACKWINDOW,
	ccRqSHOWWINDOW,
	ccRqSETWINDOWSIZE,
	ccRqSETSELECTWINDOW,
	ccRqSETWINDOWPOS,
	ccRqSETEDITSELECTION,
	ccRqSETSCROLLSIZE,
	ccRqSETSCROLLPOS,
	ccRqSETSCROLLRANGE,
	ccRqRESETCURSOR,
	ccRqSETGLOBALCURSOR,
	ccRqOBSCURECURSOR,
	ccRqCHANGEWINDOWCURSOR,
	ccRqACTIVATEWINDOW,
	ccRqACTIVATECONTROL,
	ccRqCREATECARET,
	ccRqSETCARETPOS,
	ccRqDESTROYCARET,
	ccRqGETWINDOWPOS,
	ccRqGETCLIENTSIZE,
	ccRqUPDATEWINDOWRECT,
	ccRqGETWINDOWTEXT,
	ccRqSETWINDOWTITLE,
	ccRqFAKEPAINT,
	ccRqENDPAINT,
	ccRqBEGINPAINT,
	ccRqDESTROYWINDOW,
	ccRqDESTROYMODALDIALOG,
	ccRqDRAWMBAR,
	ccRqTRACKPOPMENU,
	ccRqCREATEPOPMENU,
	ccRqINSERTSEPARATOR,
	ccRqMENUENABLE,
	ccRqMODIFYMENU,
	ccRqINSERTMENU,
	ccRqITEMENABLE,
	ccRqREMOVEMENUSHORTKEY,
	ccRqADDMENUSHORTKEY,
	ccRqMODIFYMENUITEM,
	ccRqDESTROYMENU,
	ccRqDELETEMENU,
	ccRqREMOVEMENUITEM,
	ccRqCHECKMENUITEM,
	ccRqINSERTMENUITEM,
	ccRqDOMESSAGE,
	ccWINMESSmax,
	ccWmCHECKQUIT,
	ccWmUSEREVENT,
	ccWmSTATISTICS,
	ccWmOBJECTKEYUP,
	ccWmOBJECTKEYDOWN,
	ccWmOBJECTTIMER,
	ccWmANIMATION,
	ccWmCOLLISION,
	ccWmTOUCHBOUND,
	ccWmOBJECTDONE,
	ccWmMOVEOBJECT,
	ccWmINITOBJECT,
	ccWmSCROLL,
	ccWmGAMEKEYBOARD,
	ccWmINETEVENT,
	ccWmZEROTIMER,
	ccWmLOSTKEY,
	ccWmLOSTMOUSE,
	ccWmSPECIALBUTTON,
	ccWmPROCESSDROPFILES,
	ccWmGETTOOLBARTIPTEXT,
	ccWmSETFOCUS,
	ccWmKILLFOCUS,
	ccWmPROCESSCLOSE,
	ccWmDRAWCLIPBOARD,
	ccWmGETSCROLLBARINFO,
	ccWmSCROLLBARACTION,
	ccWmDDEEXECUTE,
	ccWmIDLEDIALOG,
	ccWmDRAWCONTROL,
	ccWmCOMBOSELECT,
	ccWmBUTTONCLICKED,
	ccWmINITDIALOG,
	ccWmIDLETIMER,
	ccWmTIMER,
	ccWmNEWVTHUMB,
	ccWmNEWHTHUMB,
	ccWmGETVSCROLLVAL,
	ccWmGETHSCROLLVAL,
	ccWmSIZE,
	ccWmMOUSE,
	ccWmKEYBOARD,
	ccWmDEACTIVATE,
	ccWmACTIVATE,
	ccWmCLOSE,
	ccWmCOMMAND,
	ccWmCHAR,
	ccWmCREATE,
	ccWmPAINT,
	ccWINMESSmin,
	ccWmNOTIFY,
	ccRETURNmax,
	ccRETURN6,
	ccRETURN5,
	ccRETURN4,
	ccRETURN3,
	ccRETURN2,
	ccRETURN1,
	ccRETURN0,
	ccRETURNmin,
	ccWASQUIT
	:: Int


-- For tracing purposes:
showCcRq :: Int -> String
showCcRq msg
	=      if msg==ccRqUSERGAMEEVENT then "ccRqUSERGAMEEVENT"
	  else if msg==ccRqCREATEGAMEOBJECT then "ccRqCREATEGAMEOBJECT"
	  else if msg==ccRqPLAYSOUNDSAMPLE then "ccRqPLAYSOUNDSAMPLE"
	  else if msg==ccRqRUNGAME then "ccRqRUNGAME"
	  else if msg==ccRqCREATEGAMEWINDOW then "ccRqCREATEGAMEWINDOW"
	  else if msg==ccRqDO_PRINT_SETUP then "ccRqDO_PRINT_SETUP"
	  else if msg==ccRqDO_HTML_HELP then "ccRqDO_HTML_HELP"
	  else if msg==ccRqGET_PRINTER_DC then "ccRqGET_PRINTER_DC"
	  else if msg==ccRqDISPATCH_MESSAGES_WHILE_PRINTING then "ccRqDISPATCH_MESSAGES_WHILE_PRINTING"
	  else if msg==ccRqENDDOC then "ccRqENDDOC"
	  else if msg==ccRqSTARTDOC then "ccRqSTARTDOC"
	  else if msg==ccRqCREATETCPWINDOW then "ccRqCREATETCPWINDOW"
	  else if msg==ccRqDESTROYMDIDOCWINDOW then "ccRqDESTROYMDIDOCWINDOW"
	  else if msg==ccRqCREATESDIDOCWINDOW then "ccRqCREATESDIDOCWINDOW"
	  else if msg==ccRqCREATEMDIDOCWINDOW then "ccRqCREATEMDIDOCWINDOW"
	  else if msg==ccRqCREATEMDIFRAMEWINDOW then "ccRqCREATEMDIFRAMEWINDOW"
	  else if msg==ccRqCREATESDIFRAMEWINDOW then "ccRqCREATESDIFRAMEWINDOW"
	  else if msg==ccRqCLIPBOARDHASTEXT then "ccRqCLIPBOARDHASTEXT"
	  else if msg==ccRqGETCLIPBOARDTEXT then "ccRqGETCLIPBOARDTEXT"
	  else if msg==ccRqSETCLIPBOARDTEXT then "ccRqSETCLIPBOARDTEXT"
	  else if msg==ccRqGETCLIPBOARDCOUNT then "ccRqGETCLIPBOARDCOUNT"
	  else if msg==ccRqDIRECTORYDIALOG then "ccRqDIRECTORYDIALOG"
	  else if msg==ccRqFILESAVEDIALOG then "ccRqFILESAVEDIALOG"
	  else if msg==ccRqFILEOPENDIALOG then "ccRqFILEOPENDIALOG"
	  else if msg==ccRqUPDATEDESKTOP then "ccRqUPDATEDESKTOP"
	  else if msg==ccRqSHOWCONTROL then "ccRqSHOWCONTROL"
	  else if msg==ccRqSELECTPOPUPITEM then "ccRqSELECTPOPUPITEM"
	  else if msg==ccRqENABLEPOPUPITEM then "ccRqENABLEPOPUPITEM"
	  else if msg==ccRqADDTOPOPUP then "ccRqADDTOPOPUP"
	  else if msg==ccRqSETITEMCHECK then "ccRqSETITEMCHECK"
	  else if msg==ccRqENABLECONTROL then "ccRqENABLECONTROL"
	  else if msg==ccRqCREATECOMPOUND then "ccRqCREATECOMPOUND"
	  else if msg==ccRqCREATESCROLLBAR then "ccRqCREATESCROLLBAR"
	  else if msg==ccRqCREATECUSTOM then "ccRqCREATECUSTOM"
	  else if msg==ccRqCREATEICONBUT then "ccRqCREATEICONBUT"
	  else if msg==ccRqCREATEPOPUP then "ccRqCREATEPOPUP"
	  else if msg==ccRqCREATECHECKBOX then "ccRqCREATECHECKBOX"
	  else if msg==ccRqCREATERADIOBUT then "ccRqCREATERADIOBUT"
	  else if msg==ccRqCREATEEDITTXT then "ccRqCREATEEDITTXT"
	  else if msg==ccRqCREATESTATICTXT then "ccRqCREATESTATICTXT"
	  else if msg==ccRqCREATEBUTTON then "ccRqCREATEBUTTON"
	  else if msg==ccRqCREATEMODALDIALOG then "ccRqCREATEMODALDIALOG"
	  else if msg==ccRqCREATEDIALOG then "ccRqCREATEDIALOG"
	  else if msg==ccRqCREATETOOLBARSEPARATOR then "ccRqCREATETOOLBARSEPARATOR"
	  else if msg==ccRqCREATETOOLBARITEM then "ccRqCREATETOOLBARITEM"
	  else if msg==ccRqCREATEMDITOOLBAR then "ccRqCREATEMDITOOLBAR"
	  else if msg==ccRqCREATESDITOOLBAR then "ccRqCREATESDITOOLBAR"
	  else if msg==ccCbFONTSIZE then "ccCbFONTSIZE"
	  else if msg==ccCbFONTNAME then "ccCbFONTNAME"
	  else if msg==ccRqGETFONTSIZES then "ccRqGETFONTSIZES"
	  else if msg==ccRqGETFONTNAMES then "ccRqGETFONTNAMES"
	  else if msg==ccRqSETCLIENTSIZE then "ccRqSETCLIENTSIZE"
	  else if msg==ccRqDELCONTROLTIP then "ccRqDELCONTROLTIP"
	  else if msg==ccRqADDCONTROLTIP then "ccRqADDCONTROLTIP"
	  else if msg==ccRqGETWINDOWSIZE then "ccRqGETWINDOWSIZE"
	  else if msg==ccRqRESTACKWINDOW then "ccRqRESTACKWINDOW"
	  else if msg==ccRqSHOWWINDOW then "ccRqSHOWWINDOW"
	  else if msg==ccRqSETWINDOWSIZE then "ccRqSETWINDOWSIZE"
	  else if msg==ccRqSETSELECTWINDOW then "ccRqSETSELECTWINDOW"
	  else if msg==ccRqSETWINDOWPOS then "ccRqSETWINDOWPOS"
	  else if msg==ccRqSETEDITSELECTION then "ccRqSETEDITSELECTION"
	  else if msg==ccRqSETSCROLLSIZE then "ccRqSETSCROLLSIZE"
	  else if msg==ccRqSETSCROLLPOS then "ccRqSETSCROLLPOS"
	  else if msg==ccRqSETSCROLLRANGE then "ccRqSETSCROLLRANGE"
	  else if msg==ccRqRESETCURSOR then "ccRqRESETCURSOR"
	  else if msg==ccRqSETGLOBALCURSOR then "ccRqSETGLOBALCURSOR"
	  else if msg==ccRqOBSCURECURSOR then "ccRqOBSCURECURSOR"
	  else if msg==ccRqCHANGEWINDOWCURSOR then "ccRqCHANGEWINDOWCURSOR"
	  else if msg==ccRqACTIVATEWINDOW then "ccRqACTIVATEWINDOW"
	  else if msg==ccRqACTIVATECONTROL then "ccRqACTIVATECONTROL"
	  else if msg==ccRqCREATECARET then "ccRqCREATECARET"
	  else if msg==ccRqSETCARETPOS then "ccRqSETCARETPOS"
	  else if msg==ccRqDESTROYCARET then "ccRqDESTROYCARET"
	  else if msg==ccRqGETWINDOWPOS then "ccRqGETWINDOWPOS"
	  else if msg==ccRqGETCLIENTSIZE then "ccRqGETCLIENTSIZE"
	  else if msg==ccRqUPDATEWINDOWRECT then "ccRqUPDATEWINDOWRECT"
	  else if msg==ccRqGETWINDOWTEXT then "ccRqGETWINDOWTEXT"
	  else if msg==ccRqSETWINDOWTITLE then "ccRqSETWINDOWTITLE"
	  else if msg==ccRqFAKEPAINT then "ccRqFAKEPAINT"
	  else if msg==ccRqENDPAINT then "ccRqENDPAINT"
	  else if msg==ccRqBEGINPAINT then "ccRqBEGINPAINT"
	  else if msg==ccRqDESTROYWINDOW then "ccRqDESTROYWINDOW"
	  else if msg==ccRqDESTROYMODALDIALOG then "ccRqDESTROYMODALDIALOG"
	  else if msg==ccRqDRAWMBAR then "ccRqDRAWMBAR"
	  else if msg==ccRqTRACKPOPMENU then "ccRqTRACKPOPMENU"
	  else if msg==ccRqCREATEPOPMENU then "ccRqCREATEPOPMENU"
	  else if msg==ccRqINSERTSEPARATOR then "ccRqINSERTSEPARATOR"
	  else if msg==ccRqMENUENABLE then "ccRqMENUENABLE"
	  else if msg==ccRqMODIFYMENU then "ccRqMODIFYMENU"
	  else if msg==ccRqINSERTMENU then "ccRqINSERTMENU"
	  else if msg==ccRqITEMENABLE then "ccRqITEMENABLE"
	  else if msg==ccRqREMOVEMENUSHORTKEY then "ccRqREMOVEMENUSHORTKEY"
	  else if msg==ccRqADDMENUSHORTKEY then "ccRqADDMENUSHORTKEY"
	  else if msg==ccRqMODIFYMENUITEM then "ccRqMODIFYMENUITEM"
	  else if msg==ccRqDESTROYMENU then "ccRqDESTROYMENU"
	  else if msg==ccRqDELETEMENU then "ccRqDELETEMENU"
	  else if msg==ccRqREMOVEMENUITEM then "ccRqREMOVEMENUITEM"
	  else if msg==ccRqCHECKMENUITEM then "ccRqCHECKMENUITEM"
	  else if msg==ccRqINSERTMENUITEM then "ccRqINSERTMENUITEM"
	  else if msg==ccRqDOMESSAGE then "ccRqDOMESSAGE"
	  else "unknown ccRq message: "++show msg

showCcWm :: Int -> String
showCcWm msg
	=      if msg==ccWmCHECKQUIT then "ccWmCHECKQUIT"
	  else if msg==ccWmUSEREVENT then "ccWmUSEREVENT"
	  else if msg==ccWmSTATISTICS then "ccWmSTATISTICS"
	  else if msg==ccWmOBJECTKEYUP then "ccWmOBJECTKEYUP"
	  else if msg==ccWmOBJECTKEYDOWN then "ccWmOBJECTKEYDOWN"
	  else if msg==ccWmOBJECTTIMER then "ccWmOBJECTTIMER"
	  else if msg==ccWmANIMATION then "ccWmANIMATION"
	  else if msg==ccWmCOLLISION then "ccWmCOLLISION"
	  else if msg==ccWmTOUCHBOUND then "ccWmTOUCHBOUND"
	  else if msg==ccWmOBJECTDONE then "ccWmOBJECTDONE"
	  else if msg==ccWmMOVEOBJECT then "ccWmMOVEOBJECT"
	  else if msg==ccWmINITOBJECT then "ccWmINITOBJECT"
	  else if msg==ccWmSCROLL then "ccWmSCROLL"
	  else if msg==ccWmGAMEKEYBOARD then "ccWmGAMEKEYBOARD"
	  else if msg==ccWmINETEVENT then "ccWmINETEVENT"
	  else if msg==ccWmZEROTIMER then "ccWmZEROTIMER"
	  else if msg==ccWmLOSTKEY then "ccWmLOSTKEY"
	  else if msg==ccWmLOSTMOUSE then "ccWmLOSTMOUSE"
	  else if msg==ccWmSPECIALBUTTON then "ccWmSPECIALBUTTON"
	  else if msg==ccWmPROCESSDROPFILES then "ccWmPROCESSDROPFILES"
	  else if msg==ccWmGETTOOLBARTIPTEXT then "ccWmGETTOOLBARTIPTEXT"
	  else if msg==ccWmSETFOCUS then "ccWmSETFOCUS"
	  else if msg==ccWmKILLFOCUS then "ccWmKILLFOCUS"
	  else if msg==ccWmPROCESSCLOSE then "ccWmPROCESSCLOSE"
	  else if msg==ccWmDRAWCLIPBOARD then "ccWmDRAWCLIPBOARD"
	  else if msg==ccWmGETSCROLLBARINFO then "ccWmGETSCROLLBARINFO"
	  else if msg==ccWmSCROLLBARACTION then "ccWmSCROLLBARACTION"
	  else if msg==ccWmDDEEXECUTE then "ccWmDDEEXECUTE"
	  else if msg==ccWmIDLEDIALOG then "ccWmIDLEDIALOG"
	  else if msg==ccWmDRAWCONTROL then "ccWmDRAWCONTROL"
	  else if msg==ccWmCOMBOSELECT then "ccWmCOMBOSELECT"
	  else if msg==ccWmBUTTONCLICKED then "ccWmBUTTONCLICKED"
	  else if msg==ccWmINITDIALOG then "ccWmINITDIALOG"
	  else if msg==ccWmIDLETIMER then "ccWmIDLETIMER"
	  else if msg==ccWmTIMER then "ccWmTIMER"
	  else if msg==ccWmNEWVTHUMB then "ccWmNEWVTHUMB"
	  else if msg==ccWmNEWHTHUMB then "ccWmNEWHTHUMB"
	  else if msg==ccWmGETVSCROLLVAL then "ccWmGETVSCROLLVAL"
	  else if msg==ccWmGETHSCROLLVAL then "ccWmGETHSCROLLVAL"
	  else if msg==ccWmSIZE then "ccWmSIZE"
	  else if msg==ccWmMOUSE then "ccWmMOUSE"
	  else if msg==ccWmKEYBOARD then "ccWmKEYBOARD"
	  else if msg==ccWmDEACTIVATE then "ccWmDEACTIVATE"
	  else if msg==ccWmACTIVATE then "ccWmACTIVATE"
	  else if msg==ccWmCLOSE then "ccWmCLOSE"
	  else if msg==ccWmCOMMAND then "ccWmCOMMAND"
	  else if msg==ccWmCHAR then "ccWmCHAR"
	  else if msg==ccWmCREATE then "ccWmCREATE"
	  else if msg==ccWmPAINT then "ccWmPAINT"
	  else if msg==ccWmNOTIFY then "ccWmNOTIFY"
	  else if msg==ccRETURN6 then "ccRETURN6"
	  else if msg==ccRETURN5 then "ccRETURN5"
	  else if msg==ccRETURN4 then "ccRETURN4"
	  else if msg==ccRETURN3 then "ccRETURN3"
	  else if msg==ccRETURN2 then "ccRETURN2"
	  else if msg==ccRETURN1 then "ccRETURN1"
	  else if msg==ccRETURN0 then "ccRETURN0"
	  else if msg==ccWASQUIT then "ccWASQUIT"
	  else "unknown ccWm message: "++show msg
