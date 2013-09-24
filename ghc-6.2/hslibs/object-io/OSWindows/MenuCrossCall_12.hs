module MenuCrossCall_12 ( HITEM, HMENU
			, winCreatePopupMenuHandle, winTrackPopupMenu, winInsertMenu
			, winInsertMenuItem, winInsertSeparator, winChangeMenuItemCheck
			, winModifyMenu, winModifyMenuItem, winDestroyMenu
			, winDeleteMenu, winRemoveMenuItem, winChangeItemAbility, winChangeMenuAbility 
			, winDrawMenuBar, winAddMenuShortKey, winRemoveMenuShortKey 
			) where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	MenuCrossCall_12 contains all cross calls for menu handling.
--	Currently only the menu types are defined.
--	********************************************************************************


import ClCrossCall_12
import Cutil_12
import OSTypes(HWND)
import Foreign.Marshal.Utils(fromBool)
import Char(ord)

type	HITEM	= Int
type	HMENU	= Int

winCreatePopupMenuHandle :: IO HMENU
winCreatePopupMenuHandle = do
	rcci <- issueCleanRequest2 (errorCallback2 "CreatePopupMenuHandle ") (rq0Cci ccRqCREATEPOPMENU)
	let hmenu =
		if ccMsg rcci == ccRETURN1 then p1 rcci else
		if ccMsg rcci == ccWASQUIT then 0	else
					        error "[CreatePopupMenuHandle] expected CcRETURN1 value."
	return hmenu

winTrackPopupMenu :: HMENU -> HWND -> IO Bool
winTrackPopupMenu menu framePtr = do
	rcci <- issueCleanRequest2 (errorCallback2 "TrackPopupMenu") (rq2Cci ccRqTRACKPOPMENU menu framePtr)
	let ok = 
		if ccMsg rcci == ccRETURN1 then p1 rcci /= 0 else
	  	if ccMsg rcci == ccWASQUIT then False	     else
	  				        error "[TrackPopupMenu] expected CcRETURN1 value."
	return ok

winInsertMenu :: String -> Bool -> HMENU -> HMENU -> Int -> IO ()
winInsertMenu text state submenu menu pos = do
	textPtr <- newCString text
	issueCleanRequest2 (errorCallback2 "AppendMenu") (rq5Cci ccRqINSERTMENU (fromBool state) menu (addr2int textPtr) submenu pos)
	free textPtr

winInsertMenuItem :: String -> Bool -> Bool -> HMENU -> Int -> IO HITEM
winInsertMenuItem text ablestate markstate menu pos = do
	textPtr <- newCString text
	let insertCci = rq5Cci ccRqINSERTMENUITEM (fromBool ablestate) menu (addr2int textPtr) (fromBool markstate) pos
	rcci <- issueCleanRequest2 (errorCallback2 "InsertMenuItem") insertCci
	let hitem =
		if ccMsg rcci == ccRETURN1 then p1 rcci else
		if ccMsg rcci == ccWASQUIT then 0       else
						error "[WinInsertMenuItem] expected CcRETURN1 value."
	free textPtr
	return hitem

winInsertSeparator :: HMENU -> Int -> IO ()
winInsertSeparator menu pos = do
	issueCleanRequest2 (errorCallback2 "InsertSeparator") (rq2Cci ccRqINSERTSEPARATOR menu pos)
	return ()

winChangeMenuItemCheck :: HMENU -> HITEM -> Bool -> IO ()
winChangeMenuItemCheck menu hitem state = do
	issueCleanRequest2 (errorCallback2 "CheckMenuItem") (rq3Cci ccRqCHECKMENUITEM menu hitem (fromBool state))
	return ()

winModifyMenu :: String -> HMENU -> HMENU -> IO ()
winModifyMenu text submenu menu = do
	textPtr <- newCString text
	issueCleanRequest2 (errorCallback2 "ModifyMenu") (rq3Cci ccRqMODIFYMENU submenu menu (addr2int textPtr))
	free textPtr

winModifyMenuItem :: String -> HITEM -> HMENU -> IO ()
winModifyMenuItem text hitem menu = do
	textPtr <- newCString text
	issueCleanRequest2 (errorCallback2 "ModifyMenuItem") (rq3Cci ccRqMODIFYMENUITEM hitem menu (addr2int textPtr))
	free textPtr

winDestroyMenu :: HMENU -> IO ()
winDestroyMenu menu = do
	issueCleanRequest2 (errorCallback2 "DestroyMenu") (rq1Cci ccRqDESTROYMENU menu)
	return ()

winDeleteMenu :: HMENU -> HITEM -> IO ()
winDeleteMenu menu hitem = do
	issueCleanRequest2 (errorCallback2 "DeleteMenu") (rq2Cci ccRqDELETEMENU menu hitem)
	return ()

winRemoveMenuItem :: HMENU -> HITEM -> IO ()
winRemoveMenuItem menu hitem = do
	issueCleanRequest2 (errorCallback2 "RemoveMenuItem") (rq2Cci ccRqREMOVEMENUITEM menu hitem)
	return ()

winChangeItemAbility :: HMENU -> HITEM -> Bool -> IO ()
winChangeItemAbility parent hitem onoff = do
	issueCleanRequest2 (errorCallback2 "ChangeItemAbility") (rq3Cci ccRqITEMENABLE parent hitem (fromBool onoff))
	return ()

winChangeMenuAbility :: HMENU -> Int -> Bool -> IO ()
winChangeMenuAbility parent zIndex onoff = do
	issueCleanRequest2 (errorCallback2 "ChangeMenuAbility") (rq3Cci ccRqMENUENABLE parent zIndex (fromBool onoff))
	return ()

winDrawMenuBar :: HWND -> HWND -> IO ()
winDrawMenuBar framePtr clientPtr = do
	issueCleanRequest2 (errorCallback2 "DrawMenuBar") (rq2Cci ccRqDRAWMBAR framePtr clientPtr)
	return ()

winAddMenuShortKey :: HWND -> Int -> Char -> IO ()
winAddMenuShortKey framePtr cmd key = do
	issueCleanRequest2 (errorCallback2 "AddMenuShortKey") (rq3Cci ccRqADDMENUSHORTKEY framePtr cmd (ord key))
	return ()

winRemoveMenuShortKey :: HWND -> Int -> IO ()
winRemoveMenuShortKey framePtr cmd = do
	issueCleanRequest2 (errorCallback2 "RemoveMenuShortKey") (rq2Cci ccRqREMOVEMENUSHORTKEY framePtr cmd)
	return ()
