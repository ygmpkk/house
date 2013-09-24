{-# OPTIONS -#include "OSWindows\Windows_C_12\cCCallWindows_121.h" #-}

module OSSystem where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	OSSystem defines system dependent functions.
--	********************************************************************************


import ClCCall_12
import ClCrossCall_12
import OSDocumentInterface
import OSFont
import OSTypes(Rect(..))
import Maybe
import WindowCrossCall_12


data	OSWindowMetrics
	= OSWindowMetrics
		{ osmFont          :: Font		-- The internal Font used in Windows for controls
		, osmFontMetrics   :: (Int,Int,Int)	-- The ascent, descent, leading of osmFont
		, osmHeight        :: Int		-- The height of the internal Font
		, osmHorMargin     :: Int		-- The default horizontal margin
		, osmVerMargin     :: Int		-- The default vertical   margin
		, osmHorItemSpace  :: Int		-- The default horizontal item space
		, osmVerItemSpace  :: Int		-- The default vertical   item space
		, osmHSliderHeight :: Int		-- The default height of a horizontal slider control
		, osmVSliderWidth  :: Int		-- The default width  of a vertical   slider control
		}


osNewLineChars = "\r\n"					-- OS newline convention

osTicksPerSecond = 1000	:: Int				-- OS max resolution of ticks per second

osMMtoHPixels :: Float -> Int
osMMtoHPixels mm = round ( (mm/25.4) * (fromIntegral winGetHorzResolution) )

osMMtoVPixels :: Float -> Int
osMMtoVPixels mm = round ( (mm/25.4) * (fromIntegral winGetVertResolution) )

osMaxScrollWindowSize :: (Int,Int)
osMaxScrollWindowSize = winMaxScrollWindowSize

osMaxFixedWindowSize :: (Int,Int)
osMaxFixedWindowSize = winMaxFixedWindowSize

osScreenRect :: IO Rect
osScreenRect
	= do {
		screenWidth  <- winScreenXSize;
		screenHeight <- winScreenYSize;
		return (Rect {rleft=0,rtop=0,rright=screenWidth,rbottom=screenHeight})
	  }
	  
osPrintSetupTypical :: Bool -- MW11++
osPrintSetupTypical = False

osGetProcessWindowDimensions :: OSDInfo -> IO Rect
osGetProcessWindowDimensions osdinfo
	| isNothing maybeOSInfo
		= osScreenRect
	| otherwise
		= let osinfo = fromJust maybeOSInfo
		  in  do {
		  		(x,y) <- winGetWindowPos  (osFrame  osinfo);
				(w,h) <- winGetClientSize (osClient osinfo);
				return (Rect {rleft=x,rtop=y,rright=x+w,rbottom=y+h})
		      }
	where
		maybeOSInfo = getOSDInfoOSInfo osdinfo

osDefaultWindowMetrics :: IO OSWindowMetrics
osDefaultWindowMetrics
	= do {
		font                       <- osDialogFont;
		(ascent,descent,leading,_) <- osGetFontMetrics Nothing font;
		(scrollWidth,scrollHeight) <- winScrollbarSize;
		let
			height              = ascent+descent+leading
			unit                = (fromIntegral height)/8.0
			margin              = round (unit*7.0)
			itemspace           = round (unit*4.0)
		in return (OSWindowMetrics
				{ osmFont          = font
				, osmFontMetrics   = (ascent,descent,leading)
				, osmHeight        = height
				, osmHorMargin     = margin
				, osmVerMargin     = margin
				, osmHorItemSpace  = itemspace
				, osmVerItemSpace  = itemspace
				, osmHSliderHeight = scrollHeight
				, osmVSliderWidth  = scrollWidth
				}
			  )
	  }

{-	osStripOuterSize isMDI isResizable (width,height)
		returns (dw,dh) required to add/subtract to view size/outer size in order to obtain
		outer size/view size.
-}
osStripOuterSize :: Bool -> Bool -> IO (Int,Int)
osStripOuterSize isMDI isResizable
	| isMDI
		= winMDIClientToOuterSizeDims styleFlags
	| otherwise
		= winSDIClientToOuterSizeDims styleFlags
	where
		styleFlags = if isResizable then ws_THICKFRAME else 0
