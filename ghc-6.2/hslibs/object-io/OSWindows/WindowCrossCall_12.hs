module WindowCrossCall_12 where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	WindowCrossCall_12 collects all the cross call routines related to windows.
--	********************************************************************************


import ClCrossCall_12
import Cutil_12(newCString, peekCString, free, addr2int, int2addr)
import OSTypes(HWND)
import ClCrossCall_12
import PictCCall_12(HDC)
import Foreign.Ptr
import Foreign.Marshal.Utils(fromBool)

--	Cursor shape constants:
cursHIDDEN, cursARROW, cursFATCROSS, cursCROSS, cursIBEAM, cursBUSY :: Int
cursHIDDEN		= 6
cursARROW		= 5
cursFATCROSS		= 4
cursCROSS		= 3
cursIBEAM		= 2
cursBUSY		= 1

--	Constants for handling scrollbars.
sb_HORZ, sb_VERT, sb_CTL, sb_BOTH :: Int
sb_HORZ			= 0
sb_VERT			= 1
sb_CTL			= 2
sb_BOTH			= 3

sb_LINEUP, sb_LINELEFT, sb_LINEDOWN, sb_LINERIGHT, 
	sb_PAGEUP, sb_PAGELEFT, sb_PAGEDOWN, sb_PAGERIGHT, 
	sb_THUMBPOSITION, sb_THUMBTRACK, 
	sb_TOP, sb_LEFT, sb_BOTTOM, sb_RIGHT, 
	sb_ENDSCROLL :: Int
sb_LINEUP		= 0
sb_LINELEFT		= 0
sb_LINEDOWN		= 1
sb_LINERIGHT		= 1
sb_PAGEUP		= 2
sb_PAGELEFT		= 2
sb_PAGEDOWN		= 3
sb_PAGERIGHT		= 3
sb_THUMBPOSITION	= 4
sb_THUMBTRACK		= 5
sb_TOP			= 6
sb_LEFT			= 6
sb_BOTTOM		= 7
sb_RIGHT		= 7
sb_ENDSCROLL		= 8

--	constants for handling window styles.
ws_OVERLAPPED, ws_POPUP, ws_CHILD, ws_MINIMIZE, ws_VISIBLE, ws_DISABLED, 
	ws_CLIPSIBLINGS, ws_CLIPCHILDREN, ws_MAXIMIZE, ws_CAPTION, ws_BORDER, 
	ws_DLGFRAME, ws_VSCROLL, ws_HSCROLL, ws_SYSMENU, ws_THICKFRAME, ws_GROUP, ws_TABSTOP :: Int
ws_OVERLAPPED		= 0x00000000
ws_POPUP		= 0x80000000
ws_CHILD		= 0x40000000
ws_MINIMIZE		= 0x20000000
ws_VISIBLE		= 0x10000000
ws_DISABLED		= 0x08000000
ws_CLIPSIBLINGS		= 0x04000000
ws_CLIPCHILDREN		= 0x02000000
ws_MAXIMIZE		= 0x01000000
ws_CAPTION		= 0x00C00000		{- ws_BORDER | ws_DLGFRAME  -}
ws_BORDER		= 0x00800000
ws_DLGFRAME		= 0x00400000
ws_VSCROLL		= 0x00200000
ws_HSCROLL		= 0x00100000
ws_SYSMENU		= 0x00080000
ws_THICKFRAME		= 0x00040000
ws_GROUP		= 0x00020000
ws_TABSTOP		= 0x00010000

ws_MINIMIZEBOX		= 0x00020000
ws_MAXIMIZEBOX		= 0x00010000

ws_TILED		= ws_OVERLAPPED
ws_ICONIC		= ws_MINIMIZE
ws_SIZEBOX		= ws_THICKFRAME

--	constants for stacking windows.
hwnd_TOP, hwnd_BOTTOM, hwnd_TOPMOST, hwnd_NOTOPMOST :: Int 
hwnd_TOP		= 0
hwnd_BOTTOM		= 1
hwnd_TOPMOST		= -1
hwnd_NOTOPMOST		= -2

--	flag values for passing information about edit controls from Clean to OS.
editISMULTILINE, editISKEYSENSITIVE :: Int
editISMULTILINE		= 1			{- flag value: edit control is multi-line. -}
editISKEYSENSITIVE	= 2			{- flag value: edit control sends keyboard events to Clean. -}

--	values for telling Windows if a (custom)button control is OK, CANCEL, or normal. 
isNORMALBUTTON, isOKBUTTON, isCANCELBUTTON :: Int
isNORMALBUTTON		= 0			{- The button is a normal button.   -}
isOKBUTTON		= 1			{- The button is the OK button.     -}
isCANCELBUTTON		= 2			{- The button is the CANCEL button. -}


winSetWindowCursor :: HWND -> Int -> IO ()
winSetWindowCursor hwnd cursorcode
	= issueCleanRequest2 (errorCallback2 "winSetWindowCursor") (rq2Cci ccRqCHANGEWINDOWCURSOR hwnd cursorcode) >> return ()

winObscureCursor :: IO ()
winObscureCursor
	= issueCleanRequest2 (errorCallback2 "winObscureCursor") (rq0Cci ccRqOBSCURECURSOR) >> return ()

winSetWindowTitle :: HWND -> String -> IO ()
winSetWindowTitle hwnd title
	= do {
		textptr <- newCString title;
		issueCleanRequest2 (errorCallback2 "SetWindowTitle") (rq2Cci ccRqSETWINDOWTITLE hwnd (addr2int textptr));
		free textptr
	  }

winGetWindowText :: HWND -> IO String
winGetWindowText hwnd
	= do {
		rcci <- issueCleanRequest2 (errorCallback2 "winGetWindowText") (rq1Cci ccRqGETWINDOWTEXT hwnd);
		let msg = ccMsg rcci
		in  if   msg==ccRETURN1
		    then do
		       r <- peekCString (int2addr (p1 rcci))
		       free (int2addr (p1 rcci))
		       return r
		    else if   msg==ccWASQUIT
		         then return ""
		         else error "[winGetWindowText] expected ccRETURN1 value."
	  }

winUpdateWindowRect :: HWND -> (Int,Int,Int,Int) -> IO ()
winUpdateWindowRect hwnd (left,top,right,bottom)
	= issueCleanRequest2 (errorCallback2 "winUpdateWindowRect") (rq5Cci ccRqUPDATEWINDOWRECT hwnd left top right bottom) >> return ()

winSetSelectStateWindow :: HWND -> (Bool,Bool) -> Bool -> Bool -> IO ()
winSetSelectStateWindow hwnd (hasHScroll,hasVScroll) toAble modalContext
	= issueCleanRequest2 (errorCallback2 "winSetSelectStateWindow") selectCci >> return ()
	where
		selectCci = rq5Cci ccRqSETSELECTWINDOW hwnd (fromBool hasHScroll) (fromBool hasVScroll) (fromBool toAble) (fromBool modalContext)

winBeginPaint :: HWND -> IO HDC
winBeginPaint hwnd
	= do {
		rcci <- issueCleanRequest2 (errorCallback2 "BeginPaint") (rq1Cci ccRqBEGINPAINT hwnd);
		let msg = ccMsg rcci
		in  if   msg==ccRETURN1
		    then return (int2addr (p1 rcci))
		    else if   msg==ccWASQUIT
		         then return nullPtr
		         else error "[winBeginPaint] expected ccRETURN1 value."
	  }

winEndPaint :: HWND -> HDC -> IO ()
winEndPaint hwnd hdc
	= issueCleanRequest2 (errorCallback2 "winEndPaint") (rq2Cci ccRqENDPAINT hwnd (addr2int hdc)) >> return ()

winFakePaint :: HWND -> IO ()
winFakePaint hwnd
	= issueCleanRequest2 (errorCallback2 "winFakePaint") (rq1Cci ccRqFAKEPAINT hwnd) >> return ()

winGetClientSize :: HWND -> IO (Int,Int)
winGetClientSize hwnd
	= do {
		rcci <- issueCleanRequest2 (errorCallback2 "winGetClientSize") (rq1Cci ccRqGETCLIENTSIZE hwnd);
		let msg = ccMsg rcci
		in  if   msg==ccRETURN2
		    then return (p1 rcci,p2 rcci)
		    else if   msg==ccWASQUIT
		         then return (0,0)
		         else error "[winGetClientSize] expected ccRETURN2 value."
	  }

winGetWindowSize :: HWND -> IO (Int,Int)
winGetWindowSize hwnd
	= do {
		rcci <- issueCleanRequest2 (errorCallback2 "winGetWindowSize") (rq1Cci ccRqGETWINDOWSIZE hwnd);
		let msg = ccMsg rcci
		in  if   msg==ccRETURN2
		    then return (p1 rcci,p2 rcci)
		    else if   msg==ccWASQUIT
		         then return (0,0)
		         else error "[winGetWindowSize] expected ccRETURN2 value."
	  }

winSetClientSize :: HWND -> (Int,Int) -> IO ()
winSetClientSize hwnd (w,h)
	= issueCleanRequest2 (errorCallback2 "winSetClientSize") (rq3Cci ccRqSETCLIENTSIZE hwnd w h) >> return ()

winSetWindowSize :: HWND -> (Int,Int) -> Bool -> IO ()
winSetWindowSize hwnd (w,h) update
	= issueCleanRequest2 (errorCallback2 "winSetWindowSize") (rq4Cci ccRqSETWINDOWSIZE hwnd w h (fromBool update)) >> return ()

winGetWindowPos :: HWND -> IO (Int,Int)
winGetWindowPos hwnd
	= do {
		rcci <- issueCleanRequest2 (errorCallback2 "winGetWindowPos") (rq1Cci ccRqGETWINDOWPOS hwnd);
		let msg = ccMsg rcci
		in  if   msg==ccRETURN2
		    then return (p1 rcci,p2 rcci)
		    else if   msg==ccWASQUIT
		         then return (0,0)
		         else error "[winGetWindowPos] expected ccRETURN2 value."
	  }

winSetWindowPos :: HWND -> (Int,Int) -> Bool -> Bool -> IO ()
winSetWindowPos hwnd (x,y) update inclScrollbars
	= issueCleanRequest2 (errorCallback2 "winSetWindowPos") (rq5Cci ccRqSETWINDOWPOS hwnd x y (fromBool update) (fromBool inclScrollbars)) >> return ()

winSetScrollRange :: HWND -> Int -> Int -> Int -> Bool -> IO ()
winSetScrollRange scrollHWND iBar min max redraw
	= issueCleanRequest2 (errorCallback2 "winSetScrollRange") (rq5Cci ccRqSETSCROLLRANGE scrollHWND iBar min max (fromBool redraw)) >> return ()
	
winSetScrollPos :: HWND -> Int -> Int -> Int -> Int -> Int -> IO ()
winSetScrollPos scrollHWND iBar thumb maxx maxy extent
	= issueCleanRequest2 (errorCallback2 "winSetScrollPos") (rq6Cci ccRqSETSCROLLPOS scrollHWND iBar thumb maxx maxy extent) >> return ()

winSetScrollThumbSize :: HWND -> Int -> Int -> Int -> Int -> Int -> IO ()
winSetScrollThumbSize scrollHWND iBar size maxx maxy extent
	= issueCleanRequest2 (errorCallback2 "winSetScrollThumbSize") (rq6Cci ccRqSETSCROLLSIZE scrollHWND iBar size maxx maxy extent) >> return ()

winSetEditSelection :: HWND -> Int -> Int -> IO ()
winSetEditSelection editHWND first last
	= issueCleanRequest2 (errorCallback2 "winSetEditSelection") (rq3Cci ccRqSETEDITSELECTION editHWND first last) >> return ()

winShowControl :: HWND -> Bool -> IO ()
winShowControl hwnd bool
	= issueCleanRequest2 (errorCallback2 "winShowControl") (rq2Cci ccRqSHOWCONTROL hwnd (fromBool bool)) >> return ()

winEnableControl :: HWND -> Bool -> IO ()
winEnableControl hwnd bool
	= issueCleanRequest2 (errorCallback2 "winEnableControl") (rq2Cci ccRqENABLECONTROL hwnd (fromBool bool)) >> return ()

winEnablePopupItem :: HWND -> Int -> Bool -> IO ()
winEnablePopupItem hwnd pos bool
	= issueCleanRequest2 (errorCallback2 "winEnablePopupItem") (rq3Cci ccRqENABLEPOPUPITEM hwnd pos (fromBool bool)) >> return ()

winCheckControl :: HWND -> Bool -> IO ()
winCheckControl hwnd bool
	= issueCleanRequest2 (errorCallback2 "winCheckControl") (rq2Cci ccRqSETITEMCHECK hwnd (fromBool bool)) >> return ()

winSelectPopupItem :: HWND -> Int -> IO ()
winSelectPopupItem hwnd pos
	= issueCleanRequest2 (errorCallback2 "winSelectPopupItem") (rq2Cci ccRqSELECTPOPUPITEM hwnd pos) >> return ()

winCreateCaret :: HWND -> Int -> Int -> IO ()
winCreateCaret hwnd width height
	= issueCleanRequest2 (errorCallback2 "winCreateCaret") (rq3Cci ccRqCREATECARET hwnd width height) >> return ()
	
winSetCaretPos :: HWND -> Int -> Int -> IO ()
winSetCaretPos hwnd x y
	= issueCleanRequest2 (errorCallback2 "winSetCaretPos") (rq3Cci ccRqSETCARETPOS hwnd x y) >> return ()
	
winDestroyCaret :: HWND -> IO ()
winDestroyCaret hwnd
	= issueCleanRequest2 (errorCallback2 "winDestroyCaret") (rq1Cci ccRqDESTROYCARET hwnd) >> return ()
