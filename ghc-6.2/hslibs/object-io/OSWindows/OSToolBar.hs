module OSToolBar ( OSToolbar(..), OSToolbarHandle
                 , osDefaultToolbarHeight
                 , osCreateToolbar, osCreateBitmapToolbarItem, osCreateToolbarSeparator
                 ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	OSToolBar contains operations to add and remove tools.
--	In this implementation only the type constructors are included, not the
--	functions.
--	********************************************************************************


import Cutil_12(addr2int)
import ClCrossCall_12
import PictCCall_12
import WindowCCall_12
import OSTypes (HWND, OSWindowPtr, osNoWindowPtr)
import OSBitmap(OSBitmap(..), osGetBitmapSize)


data	OSToolbar
	= OSToolbar
		{ toolbarPtr    :: !OSToolbarHandle	-- The toolbar of the frame window (zero if no toolbar)
		, toolbarHeight :: !Int			-- The height of the toolbar       (zero if no toolbar)
		}
type	OSToolbarHandle
	= HWND

osDefaultToolbarHeight = 16 :: Int			-- The default height of the toolbar


{-	osCreateToolbar wPtr height
		creates a toolbar in the argument window with the given size of the bitmap images.
		The return Int is the actual height of the toolbar. 
-}
osCreateToolbar :: Bool -> HWND -> (Int,Int) -> IO (OSToolbarHandle,Int)
osCreateToolbar forMDI hwnd (w,h)
	= do {
		rcci <- issueCleanRequest2 (errorCallback2 "osCreateToolbar") (rq3Cci (if forMDI then ccRqCREATEMDITOOLBAR else ccRqCREATESDITOOLBAR) hwnd w h);
		let msg          = ccMsg rcci
		    tbPtr_Height = if      msg==ccRETURN2 then (p1 rcci,p2 rcci)
		                   else if msg==ccWASQUIT then (osNoWindowPtr,0)
		                   else    error "[osCreateToolbar] expected ccRETURN1 value."
		in  return tbPtr_Height
	  }

osCreateBitmapToolbarItem :: OSToolbarHandle -> OSBitmap -> Int -> IO ()
osCreateBitmapToolbarItem tbPtr osBitmap index = do
	issueCleanRequest2 (errorCallback2 "osCreateBitmapToolbarItem") (rq3Cci ccRqCREATETOOLBARITEM tbPtr (addr2int (bitmapHandle osBitmap)) index)
	return ()
	where
		(w,_)     = osGetBitmapSize    osBitmap

osCreateToolbarSeparator :: OSToolbarHandle -> IO ()
osCreateToolbarSeparator tbPtr
	= issueCleanRequest2 (errorCallback2 "osCreateToolbarSeparator") (rq1Cci ccRqCREATETOOLBARSEPARATOR tbPtr) >> return ()
