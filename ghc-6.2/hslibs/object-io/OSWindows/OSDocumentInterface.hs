{-# OPTIONS -#include "OSWindows\Windows_C_12\cCrossCallxDI_121.h" #-}

module OSDocumentInterface ( OSDInfo(..), OSInfo(..), OSMenuBar(..)
                           , emptyOSDInfo
                           , getOSDInfoDocumentInterface
                           , getOSDInfoOSMenuBar, setOSDInfoOSMenuBar
                           , getOSDInfoOSInfo, setOSDInfoOSInfo
                           , osOpenMDI, osOpenSDI, osCloseOSDInfo
                           , getOSDInfoOSToolbar
                           ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	OSDocumentInterface creates the infrastructure for (N/S/M)DI frame windows.
--	********************************************************************************


import ClCrossCall_12
import CommonDef          (dumpFatalError)
import MenuCrossCall_12   (HMENU)
import OSToolBar
import OSTypes            (OSWindowPtr, HWND, osNoWindowPtr)
import StdIOCommon        (DocumentInterface(MDI, SDI, NDI))
import Maybe
import WindowCrossCall_12 (winFakePaint)
import Foreign.Marshal.Utils(fromBool)


data	OSDInfo
	= OSMDInfo
		{ osmdOSInfo     :: !OSInfo		-- The general document interface infrastructure
		, osmdWindowMenu :: !HMENU		-- The Window menu in the MDI menu bar
		}
	| OSSDInfo
		{ ossdOSInfo     :: !OSInfo		-- The general document interface infrastructure
		}
	| OSNoInfo
data	OSInfo
	= OSInfo
		{ osFrame        :: !HWND		-- The frame window of the (M/S)DI frame window
		, osToolbar      :: !(Maybe OSToolbar)	-- The toolbar of the (M/S)DI frame window (Nothing if no toolbar)
		, osClient       :: !HWND		-- The client window of the (M/S)DI frame window
		, osMenuBar      :: !HMENU		-- The menu bar of the (M/S)DI frame window
		}
data	OSMenuBar
	= OSMenuBar
		{ menuBar        :: !HMENU
		, menuWindow     :: !HWND
		, menuClient     :: !HWND
		}


osdocumentinterfaceFatalError :: String -> String -> x
osdocumentinterfaceFatalError function error
	= dumpFatalError function "OSDocumentInterface" error


{-	emptyOSDInfo creates a OSDInfo with dummy values for the argument document interface.
-}
emptyOSDInfo :: DocumentInterface -> OSDInfo
emptyOSDInfo di
	= case di of
		MDI -> OSMDInfo {osmdOSInfo=emptyOSInfo,osmdWindowMenu=(-1)}
		SDI -> OSSDInfo {ossdOSInfo=emptyOSInfo}
		NDI -> OSNoInfo
	where
		emptyOSInfo = OSInfo {osFrame=(-1),osToolbar=Nothing,osClient=(-1),osMenuBar=(-1)}


{-	getOSDInfoDocumentInterface returns the DocumentInterface of the argument OSDInfo.
-}
getOSDInfoDocumentInterface :: OSDInfo -> DocumentInterface
getOSDInfoDocumentInterface (OSMDInfo _ _) = MDI
getOSDInfoDocumentInterface (OSSDInfo _)   = SDI
getOSDInfoDocumentInterface OSNoInfo       = NDI


{-	getOSDInfoOSMenuBar returns the OSMenuBar info from the argument OSDInfo.
	setOSDInfoOSMenuBar sets the OSMenuBar info in the OSDInfo.
-}
getOSDInfoOSMenuBar :: OSDInfo -> Maybe OSMenuBar
getOSDInfoOSMenuBar osdInfo
	= case osdInfo of
		OSMDInfo {osmdOSInfo=info} -> get info
		OSSDInfo {ossdOSInfo=info} -> get info
		osnoinfo                   -> Nothing
	where
		get (OSInfo {osFrame=osFrame,osClient=osClient,osMenuBar=osMenuBar})
			= Just (OSMenuBar {menuBar=osMenuBar,menuWindow=osFrame,menuClient=osClient})

setOSDInfoOSMenuBar :: OSMenuBar -> OSDInfo -> OSDInfo
setOSDInfoOSMenuBar (OSMenuBar {menuBar=menuBar,menuWindow=menuWindow,menuClient=menuClient}) osdInfo
	= case osdInfo of
		mdi@(OSMDInfo {osmdOSInfo=info}) -> mdi {osmdOSInfo=set info}
		sdi@(OSSDInfo {ossdOSInfo=info}) -> sdi {ossdOSInfo=set info}
		osnoinfo                         -> osnoinfo
	where
		set info = info {osMenuBar=menuBar,osFrame=menuWindow,osClient=menuClient}


{-	getOSDInfoOSInfo returns the OSInfo from the argument OSDInfo if present.
	setOSDInfoOSInfo sets the OSInfo in the OSDInfo.
-}
getOSDInfoOSInfo :: OSDInfo -> Maybe OSInfo
getOSDInfoOSInfo (OSMDInfo {osmdOSInfo=osmdOSInfo}) = Just osmdOSInfo
getOSDInfoOSInfo (OSSDInfo {ossdOSInfo=ossdOSInfo}) = Just ossdOSInfo
getOSDInfoOSInfo osnoinfo                           = Nothing

setOSDInfoOSInfo :: OSInfo -> OSDInfo -> OSDInfo
setOSDInfoOSInfo osinfo osm@(OSMDInfo _ _) = osm {osmdOSInfo=osinfo}
setOSDInfoOSInfo osinfo oss@(OSSDInfo _)   = oss {ossdOSInfo=osinfo}
setOSDInfoOSInfo _      osnoinfo           = osnoinfo


{-	osOpenMDI creates the infrastructure of an MDI process.
		If the first Bool argument is True, then the frame window is shown, otherwise it is hidden.
		The second Bool indicates whether the process accepts file open events.
-}
osOpenMDI :: Bool -> Bool -> IO OSDInfo
osOpenMDI shown acceptFileOpen
	= do {
		returncci <- issueCleanRequest2 osCreateMDIWindowCallback (rq2Cci ccRqCREATEMDIFRAMEWINDOW (fromBool shown) (fromBool acceptFileOpen));
		let	msg     = ccMsg returncci
			(framePtr,clientPtr,menuBar,windowMenu)
				= if      msg==ccRETURN4 then (p1 returncci,p2 returncci,p3 returncci,p4 returncci)
				  else if msg==ccWASQUIT then (osNoWindowPtr,osNoWindowPtr,osNoWindowPtr,osNoWindowPtr)
				  else    osdocumentinterfaceFatalError "osOpenMDI" ("ccRETURN4 expected instead of "++show msg)
			osmdinfo = OSMDInfo
			             { osmdOSInfo     = OSInfo
			                                   { osFrame   = framePtr
			                                   , osToolbar = Nothing
			                                   , osClient  = clientPtr
			                                   , osMenuBar = menuBar
			                                   }
			             , osmdWindowMenu = windowMenu
			             }
		in
		return osmdinfo
	     }
	where
		osCreateMDIWindowCallback :: CrossCallInfo -> IO CrossCallInfo
		osCreateMDIWindowCallback (CrossCallInfo {ccMsg=msg})
			| msg==ccWmDEACTIVATE || msg==ccWmACTIVATE || msg==ccWmKILLFOCUS
				= return return0Cci
			| otherwise
				= osdocumentinterfaceFatalError "osCreateMDIWindowCallback" ("received message nr:"++show msg)

osOpenSDI :: Bool -> IO OSDInfo
osOpenSDI acceptFileOpen
	= do {
		returncci <- issueCleanRequest2 osCreateSDIWindowCallback (rq1Cci ccRqCREATESDIFRAMEWINDOW (fromBool acceptFileOpen));
		let	msg      = ccMsg returncci
			(framePtr,menuBar)
			         = if      msg==ccRETURN2 then (p1 returncci,p2 returncci)
			           else if msg==ccWASQUIT then (osNoWindowPtr,osNoWindowPtr)
			           else    osdocumentinterfaceFatalError "osOpenSDI" ("ccRETURN2 expected instead of "++show msg)
			ossdinfo = OSSDInfo
			             { ossdOSInfo = OSInfo {osFrame=framePtr,osToolbar=Nothing,osClient=osNoWindowPtr,osMenuBar=menuBar} }
		in return ossdinfo
	     }
	where
		osCreateSDIWindowCallback :: CrossCallInfo -> IO CrossCallInfo
		osCreateSDIWindowCallback (CrossCallInfo {ccMsg=msg})
			= if   msg==ccWmDEACTIVATE || msg==ccWmACTIVATE || msg==ccWmKILLFOCUS
			  then return return0Cci
			  else osdocumentinterfaceFatalError "osCreateSDIWindowCallback" ("received message nr:"++show msg)

osCloseOSDInfo :: OSDInfo -> IO ()
osCloseOSDInfo (OSMDInfo {osmdOSInfo=OSInfo {osFrame=osFrame}})
	= issueCleanRequest2 (osDestroyProcessWindowCallback "osCloseMDI") (rq1Cci ccRqDESTROYWINDOW osFrame) >> return ()
osCloseOSDInfo (OSSDInfo {ossdOSInfo=OSInfo {osFrame=osFrame}})
	= issueCleanRequest2 (osDestroyProcessWindowCallback "osCloseSDI") (rq1Cci ccRqDESTROYWINDOW osFrame) >> return ()
osCloseOSDInfo _
	= return ()

osDestroyProcessWindowCallback :: String -> CrossCallInfo -> IO CrossCallInfo
osDestroyProcessWindowCallback function cci@(CrossCallInfo {ccMsg=msg})
	| msg==ccWmDEACTIVATE || msg==ccWmACTIVATE || msg==ccWmKEYBOARD
		= return return0Cci
	| msg==ccWmPAINT
		= winFakePaint (p1 cci) >> return return0Cci
	| otherwise
		= osdocumentinterfaceFatalError function ("received message nr:"++show msg)

--	getOSDInfoOSToolbar retrieves the OSToolbar, if any.
getOSDInfoOSToolbar :: OSDInfo -> Maybe OSToolbar
getOSDInfoOSToolbar info@(OSMDInfo _ _) = osToolbar $ osmdOSInfo info
getOSDInfoOSToolbar info@(OSSDInfo _)   = osToolbar $ ossdOSInfo info
getOSDInfoOSToolbar _                   = Nothing
