{-# OPTIONS -#include "OSWindows\Windows_C_12\cpicture_121.h" #-}

module PictCCall_12 ( HDC, HDC, HBITMAP, Pt, RGBcolor, Fnt
                    , iWhitePattern, iLtGreyPattern, iGreyPattern, iDkGreyPattern, iBlackPattern
                    , iModeNotBic, iModeNotXor, iModeNotOr, iModeNotCopy, iModeBic, iModeXor, iModeOr, iModeCopy
                    , iStrikeOut, iUnderline, iItalic, iBold
                    , alternate, winding
                    , winCreateScreenHDC, winDestroyScreenHDC
                    , winGetStringWidth, winGetCharWidth
                    , winGetPicFontInfo, winGetFontInfo, winSetFontStyle, winSetFontSize, winSetFontName, winSetFont
                    , winDrawBitmap, winDrawResizedBitmap, winCreateBitmap
                    , winInvertPolygon, winErasePolygon, winFillPolygon, winDrawPolygon, winAddPolygonPoint
                    , winStartPolygon, winEndPolygon, winAllocPolyShape, winSetPolyPoint, winFreePolyShape
                    , winInvertWedge, winEraseWedge, winFillWedge, winDrawWedge
                    , winInvertCircle, winEraseCircle, winFillCircle, winDrawCircle
                    , winInvertOval, winEraseOval, winFillOval, winDrawOval
                    , winInvertRoundRectangle, winEraseRoundRectangle, winFillRoundRectangle, winDrawRoundRectangle
                    , winScrollRectangle, winCopyRectangle, winCopyRectangleTo, winMoveRectangle, winMoveRectangleTo
                    , winInvertRectangle, winEraseRectangle, winFillRectangle, winDrawRectangle
                    , winDrawChar, winDrawString
                    , winDrawCCurve, winDrawCLine, winDrawCPoint, winDrawCurve, winDrawLine, winDrawPoint
                    , winLinePen, winLinePenTo
                    , winMovePen, winMovePenTo, winGetPenPos
                    , winSetPenSize, winSetPattern, winSetMode, winSetBackColor, winSetPenColor
                    , winClipPicture, winClipRgnPicture, winSetClipRgnPicture, winGetClipRgnPicture
                    , winDeleteObject
                    , winDonePicture, winInitPicture
                    , getPictureScalingFactors, getResolutionC
                    ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	PictCCall_12 contains all C calls related to drawing.
--	********************************************************************************


import OSTypes(Rect(..))
import RgnCCall_12
import Cutil_12
--import Storable

type	HDC
	= Ptr ()
type	Pt
	= ( Int
	  , Int
	  )
type	RGBcolor
	= ( Int
	  , Int
	  , Int
	  )
type	Fnt
	= ( String
	  , Int
	  , Int
	  )
type  HBITMAP = Ptr ()

iWhitePattern, iLtGreyPattern, iGreyPattern, iDkGreyPattern, iBlackPattern :: Int
iWhitePattern	= 4
iLtGreyPattern	= 3
iGreyPattern	= 2
iDkGreyPattern	= 1
iBlackPattern	= 0

iModeNotBic, iModeNotXor, iModeNotOr, iModeNotCopy, iModeBic, iModeXor, iModeOr, iModeCopy :: Int
iModeNotBic	= 7
iModeNotXor	= 6
iModeNotOr	= 5
iModeNotCopy	= 4
iModeBic	= 3
iModeXor	= 2
iModeOr		= 1
iModeCopy	= 0

iStrikeOut, iUnderline, iItalic, iBold :: Int
iStrikeOut	= 8
iUnderline	= 4
iItalic		= 2
iBold		= 1

--	Constants for drawing polygons.
alternate, winding :: Int
alternate	= 1
winding		= 2


return_fst (a,_) = return a

--	Win(Create/Destroy)ScreenHDC added to temporarily create a HDC of a screen.
--		Never use these values for a window or control.

foreign import stdcall "WinCreateScreenHDC"  winCreateScreenHDC  :: IO HDC
foreign import stdcall "WinDestroyScreenHDC" winDestroyScreenHDC :: HDC -> IO ()

winGetStringWidth :: String -> Fnt -> Bool -> HDC -> IO Int
winGetStringWidth a1 (a2,a3,a4) a5 a6
	= do {
		-- Marshal arguments:
		s1 <- newCString a1;
		s2 <- newCString a2;
		-- Call C:
		width <- cWinGetStringWidth s1 s2 a3 a4 a5 a6;
		-- Read/free:
		free s1;
		free s2;		
		return width
	  }
foreign import stdcall "WinGetStringWidth" cWinGetStringWidth :: CString -> CString -> Int -> Int -> Bool -> HDC -> IO Int

winGetCharWidth :: Char -> Fnt -> Bool -> HDC -> IO Int
winGetCharWidth a1 (a2,a3,a4) a5 a6 = withCString a2 (\s -> cWinGetCharWidth a1 s a3 a4 a5 a6)
	
foreign import stdcall "WinGetCharWidth" cWinGetCharWidth :: Char -> CString -> Int -> Int -> Bool -> HDC -> IO Int

winGetPicFontInfo :: HDC -> IO (Int,Int,Int,Int)
winGetPicFontInfo a1
	= do {
		-- Marshal arguments:
		o1 <- malloc;
		o2 <- malloc;
		o3 <- malloc;
		o4 <- malloc;		
		-- Call C:
		cWinGetPicFontInfo a1 o1 o2 o3 o4;
		-- Read/free:
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		r3 <- fpeek o3;
		r4 <- fpeek o4;
		return (r1,r2,r3,r4)
	  }
foreign import stdcall "WinGetPicFontInfo" cWinGetPicFontInfo :: HDC -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

winGetFontInfo :: Fnt -> Bool -> HDC -> IO (Int,Int,Int,Int)
winGetFontInfo (a1,a2,a3) a4 a5
	= do {
		-- Marshal arguments:
		s1 <- newCString a1;
		o1 <- malloc;
		o2 <- malloc;
		o3 <- malloc;
		o4 <- malloc;
		-- Call C:
		cWinGetFontInfo s1 a2 a3 a4 a5 o1 o2 o3 o4;
		-- Read/free:
		free s1;
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		r3 <- fpeek o3;
		r4 <- fpeek o4;
		return (r1,r2,r3,r4)
	  }
foreign import stdcall "WinGetFontInfo" cWinGetFontInfo :: CString -> Int -> Int -> Bool -> HDC -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

foreign import stdcall "WinSetFontStyle" winSetFontStyle :: Int -> HDC -> IO ()

foreign import stdcall "WinSetFontSize" winSetFontSize :: Int -> HDC -> IO ()

winSetFontName :: String -> HDC -> IO ()
winSetFontName a1 a2 = withCString a1 (\s -> cWinSetFontName s a2)
foreign import stdcall "WinSetFontName" cWinSetFontName :: CString -> HDC -> IO ()

winSetFont :: Fnt -> HDC -> IO ()
winSetFont (a1,a2,a3) a4 = withCString a1 (\s -> cWinSetFont s a2 a3 a4)
foreign import stdcall "WinSetFont" cWinSetFont :: CString -> Int -> Int -> HDC -> IO ()

--	Routines to DRAW bitmaps.
winDrawBitmap :: (Int,Int) -> (Int,Int) -> HBITMAP -> HDC -> IO ()
winDrawBitmap (a1,a2) (a3,a4) a5 a6 = cWinDrawBitmap a1 a2 a3 a4 a5 a6

foreign import stdcall "WinDrawBitmap" cWinDrawBitmap :: Int -> Int -> Int -> Int -> HBITMAP -> HDC -> IO ()

winDrawResizedBitmap :: (Int,Int) -> (Int,Int) -> (Int,Int) -> HBITMAP -> HDC -> IO ()
winDrawResizedBitmap (a1,a2) (a3,a4) (a5,a6) a7 a8 = cWinDrawResizedBitmap a1 a2 a3 a4 a5 a6 a7 a8

foreign import stdcall "WinDrawResizedBitmap" cWinDrawResizedBitmap :: Int -> Int -> Int -> Int -> Int -> Int -> HBITMAP -> HDC -> IO ()
foreign import stdcall "WinCreateBitmap" winCreateBitmap :: HDC -> CString -> Ptr Int -> Ptr Int -> IO HBITMAP
foreign import stdcall "WinInvertPolygon" winInvertPolygon :: HDC -> IO ()
foreign import stdcall "WinErasePolygon"  winErasePolygon  :: HDC -> IO ()
foreign import stdcall "WinFillPolygon"   winFillPolygon   :: HDC -> IO ()
foreign import stdcall "WinDrawPolygon"   winDrawPolygon   :: HDC -> IO ()

winAddPolygonPoint :: Pt -> IO ()
winAddPolygonPoint (a1,a2) = cWinAddPolygonPoint a1 a2
foreign import stdcall "WinAddPolygonPoint" cWinAddPolygonPoint :: Int -> Int -> IO ()
foreign import stdcall "WinStartPolygon"    winStartPolygon :: Int -> IO ()
foreign import stdcall "WinEndPolygon"      winEndPolygon :: IO ()


{-	Operations to create, modify, and destroy polygon shapes.
-}

foreign import stdcall "WinAllocPolyShape" winAllocPolyShape :: Int -> IO (Ptr Int)
foreign import stdcall "WinSetPolyPoint"   winSetPolyPoint   :: Int -> Int -> Int -> Ptr Int -> IO ()
foreign import stdcall "WinFreePolyShape"  winFreePolyShape  :: Ptr Int -> IO ()


winInvertWedge :: Rect -> Pt -> Pt -> HDC -> IO ()
winInvertWedge (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) (a7,a8) a9
	= cWinInvertWedge a1 a2 a3 a4 a5 a6 a7 a8 a9
foreign import stdcall "WinInvertWedge" cWinInvertWedge :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winEraseWedge :: Rect -> Pt -> Pt -> HDC -> IO ()
winEraseWedge (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) (a7,a8) a9
	= cWinEraseWedge a1 a2 a3 a4 a5 a6 a7 a8 a9
foreign import stdcall "WinEraseWedge" cWinEraseWedge :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winFillWedge :: Rect -> Pt -> Pt -> HDC -> IO ()
winFillWedge (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) (a7,a8) a9
	= cWinFillWedge a1 a2 a3 a4 a5 a6 a7 a8 a9
foreign import stdcall "WinFillWedge" cWinFillWedge :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winDrawWedge :: Rect -> Pt -> Pt -> HDC -> IO ()
winDrawWedge (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) (a7,a8) a9
	= cWinDrawWedge a1 a2 a3 a4 a5 a6 a7 a8 a9
foreign import stdcall "WinDrawWedge" cWinDrawWedge :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winInvertCircle :: Pt -> Int -> HDC -> IO ()
winInvertCircle (a1,a2) a3 a4
	= cWinInvertCircle a1 a2 a3 a4
foreign import stdcall "WinInvertCircle" cWinInvertCircle :: Int -> Int -> Int -> HDC -> IO ()

winEraseCircle :: Pt -> Int -> HDC -> IO ()
winEraseCircle (a1,a2) a3 a4
	= cWinEraseCircle a1 a2 a3 a4
foreign import stdcall "WinEraseCircle" cWinEraseCircle :: Int -> Int -> Int -> HDC -> IO ()

winFillCircle :: Pt -> Int -> HDC -> IO ()
winFillCircle (a1,a2) a3 a4
	= cWinFillCircle a1 a2 a3 a4
foreign import stdcall "WinFillCircle" cWinFillCircle :: Int -> Int -> Int -> HDC -> IO ()

winDrawCircle :: Pt -> Int -> HDC -> IO ()
winDrawCircle (a1,a2) a3 a4
	= cWinDrawCircle a1 a2 a3 a4
foreign import stdcall "WinDrawCircle" cWinDrawCircle :: Int -> Int -> Int -> HDC -> IO ()

winInvertOval :: Rect -> HDC -> IO ()
winInvertOval (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5
	= cWinInvertOval a1 a2 a3 a4 a5
foreign import stdcall "WinInvertOval" cWinInvertOval :: Int -> Int -> Int -> Int -> HDC -> IO ()

winEraseOval :: Rect -> HDC -> IO ()
winEraseOval (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5
	= cWinEraseOval a1 a2 a3 a4 a5

foreign import stdcall "WinEraseOval" cWinEraseOval :: Int -> Int -> Int -> Int -> HDC -> IO ()

winFillOval :: Rect -> HDC -> IO ()
winFillOval (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5
	= cWinFillOval a1 a2 a3 a4 a5
foreign import stdcall "WinFillOval" cWinFillOval :: Int -> Int -> Int -> Int -> HDC -> IO ()

winDrawOval :: Rect -> HDC -> IO ()
winDrawOval (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5
	= cWinDrawOval a1 a2 a3 a4 a5
foreign import stdcall "WinDrawOval" cWinDrawOval :: Int -> Int -> Int -> Int -> HDC -> IO ()

winInvertRoundRectangle :: Rect -> Int -> Int -> HDC -> IO ()
winInvertRoundRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5 a6 a7
	= cWinInvertRoundRectangle a1 a2 a3 a4 a5 a6 a7
foreign import stdcall "WinInvertRoundRectangle" cWinInvertRoundRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winEraseRoundRectangle :: Rect -> Int -> Int -> HDC -> IO ()
winEraseRoundRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5 a6 a7
	= cWinEraseRoundRectangle a1 a2 a3 a4 a5 a6 a7
foreign import stdcall "WinEraseRoundRectangle" cWinEraseRoundRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winFillRoundRectangle :: Rect -> Int -> Int -> HDC -> IO ()
winFillRoundRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5 a6 a7
	= cWinFillRoundRectangle a1 a2 a3 a4 a5 a6 a7
foreign import stdcall "WinFillRoundRectangle" cWinFillRoundRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winDrawRoundRectangle :: Rect -> Int -> Int -> HDC -> IO ()
winDrawRoundRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5 a6 a7
	= cWinDrawRoundRectangle a1 a2 a3 a4 a5 a6 a7
foreign import stdcall "WinDrawRoundRectangle" cWinDrawRoundRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winScrollRectangle :: Rect -> Pt -> HDC -> IO Rect
winScrollRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) a7
	= do {
		-- Marshal arguments:
		o1 <- malloc;
		o2 <- malloc;
		o3 <- malloc;
		o4 <- malloc;
		-- Call C:
		cWinScrollRectangle a1 a2 a3 a4 a5 a6 a7 o1 o2 o3 o4;
		-- Read/free:
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		r3 <- fpeek o3;
		r4 <- fpeek o4;
		return (Rect {rleft=r1,rtop=r2,rright=r3,rbottom=r4})
	  }
foreign import stdcall "WinScrollRectangle" cWinScrollRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> HDC -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

winCopyRectangle :: Rect -> Pt -> HDC -> IO ()
winCopyRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) a7
	= cWinCopyRectangle a1 a2 a3 a4 a5 a6 a7
foreign import stdcall "WinCopyRectangle" cWinCopyRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winCopyRectangleTo :: Rect -> Pt -> HDC -> IO ()
winCopyRectangleTo (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) a7
	= cWinCopyRectangleTo a1 a2 a3 a4 a5 a6 a7
foreign import stdcall "WinCopyRectangleTo" cWinCopyRectangleTo :: Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winMoveRectangle :: Rect -> Pt -> HDC -> IO ()
winMoveRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) a7
	= cWinMoveRectangle a1 a2 a3 a4 a5 a6 a7
foreign import stdcall "WinMoveRectangle" cWinMoveRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winMoveRectangleTo :: Rect -> Pt -> HDC -> IO ()
winMoveRectangleTo (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) a7
	= cWinMoveRectangleTo a1 a2 a3 a4 a5 a6 a7
foreign import stdcall "WinMoveRectangleTo" cWinMoveRectangleTo :: Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winInvertRectangle :: Rect -> HDC -> IO ()
winInvertRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5
	= cWinInvertRectangle a1 a2 a3 a4 a5
foreign import stdcall "WinInvertRectangle" cWinInvertRectangle :: Int -> Int -> Int -> Int -> HDC -> IO ()

winEraseRectangle :: Rect -> HDC -> IO ()
winEraseRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5
	= cWinEraseRectangle a1 a2 a3 a4 a5
foreign import stdcall "WinEraseRectangle" cWinEraseRectangle :: Int -> Int -> Int -> Int -> HDC -> IO ()

winFillRectangle :: Rect -> HDC -> IO ()
winFillRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5
	= cWinFillRectangle a1 a2 a3 a4 a5
foreign import stdcall "WinFillRectangle" cWinFillRectangle :: Int -> Int -> Int -> Int -> HDC -> IO ()

winDrawRectangle :: Rect -> HDC -> IO ()
winDrawRectangle (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5
	= cWinDrawRectangle a1 a2 a3 a4 a5
foreign import stdcall "WinDrawRectangle" cWinDrawRectangle :: Int -> Int -> Int -> Int -> HDC -> IO ()

foreign import stdcall "WinDrawChar" winDrawChar :: Char -> HDC -> IO ()

winDrawString :: String -> HDC -> IO ()
winDrawString a1 a2 = withCString a1 (\s -> cWinDrawString s a2)
foreign import stdcall "WinDrawString" cWinDrawString :: CString -> HDC -> IO ()

winDrawCCurve :: Rect -> Pt -> Pt -> RGBcolor -> HDC -> IO ()
winDrawCCurve (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) (a7,a8) (a9,a10,a11) a12
	= cWinDrawCCurve a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
foreign import stdcall "WinDrawCCurve" cWinDrawCCurve :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winDrawCLine :: Pt -> Pt -> RGBcolor -> HDC -> IO ()
winDrawCLine (a1,a2) (a3,a4) (a5,a6,a7) a8
	= cWinDrawCLine a1 a2 a3 a4 a5 a6 a7 a8
foreign import stdcall "WinDrawCLine" cWinDrawCLine :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winDrawCPoint :: Pt -> RGBcolor -> HDC -> IO ()
winDrawCPoint (a1,a2) (a3,a4,a5) a6
	= cWinDrawCPoint a1 a2 a3 a4 a5 a6
foreign import stdcall "WinDrawCPoint" cWinDrawCPoint :: Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winDrawCurve :: Rect -> Pt -> Pt -> HDC -> IO ()
winDrawCurve (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) (a5,a6) (a7,a8) a9
	= cWinDrawCurve a1 a2 a3 a4 a5 a6 a7 a8 a9 
foreign import stdcall "WinDrawCurve" cWinDrawCurve :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> HDC -> IO ()

winDrawLine :: Pt -> Pt -> HDC -> IO ()
winDrawLine (a1,a2) (a3,a4) a5
	= cWinDrawLine a1 a2 a3 a4 a5
foreign import stdcall "WinDrawLine" cWinDrawLine :: Int -> Int -> Int -> Int -> HDC -> IO ()

winDrawPoint :: Pt -> HDC -> IO ()
winDrawPoint (a1,a2) a3
	= cWinDrawPoint a1 a2 a3
foreign import stdcall "WinDrawPoint" cWinDrawPoint :: Int -> Int -> HDC -> IO ()

winLinePen :: Pt -> HDC -> IO ()
winLinePen (a1,a2) a3
	= cWinLinePen a1 a2 a3
foreign import stdcall "WinLinePen" cWinLinePen :: Int -> Int -> HDC -> IO ()

winLinePenTo :: Pt -> HDC -> IO ()
winLinePenTo (a1,a2) a3
	= cWinLinePenTo a1 a2 a3
foreign import stdcall "WinLinePenTo" cWinLinePenTo :: Int -> Int -> HDC -> IO ()

winMovePen :: Pt -> HDC -> IO ()
winMovePen (a1,a2) a3
	= cWinMovePen a1 a2 a3
foreign import stdcall "WinMovePen" cWinMovePen :: Int -> Int -> HDC -> IO ()

winMovePenTo :: Pt -> HDC -> IO ()
winMovePenTo (a1,a2) a3
	= cWinMovePenTo a1 a2 a3
foreign import stdcall "WinMovePenTo" cWinMovePenTo :: Int -> Int -> HDC -> IO ()

winGetPenPos :: HDC -> IO (Int,Int)
winGetPenPos a1
	= do {
		o1 <- malloc;
		o2 <- malloc;
		cWinGetPenPos a1 o1 o2;
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		return (r1,r2)
	  }
foreign import stdcall "WinGetPenPos" cWinGetPenPos :: HDC -> Ptr Int -> Ptr Int -> IO ()

foreign import stdcall "WinSetPenSize" winSetPenSize :: Int -> HDC -> IO ()

foreign import stdcall "WinSetPattern" winSetPattern :: Int -> HDC -> IO ()

foreign import stdcall "WinSetMode" winSetMode :: Int -> HDC -> IO ()

winSetBackColor :: RGBcolor -> HDC -> IO ()
winSetBackColor (a1,a2,a3) a4
	= cWinSetBackColor a1 a2 a3 a4
foreign import stdcall "WinSetBackColor" cWinSetBackColor :: Int -> Int -> Int -> HDC -> IO ()

winSetPenColor :: RGBcolor -> HDC -> IO ()
winSetPenColor (a1,a2,a3) a4
	= cWinSetPenColor a1 a2 a3 a4
foreign import stdcall "WinSetPenColor" cWinSetPenColor :: Int -> Int -> Int -> HDC -> IO ()

winClipPicture :: Rect -> HDC -> IO ()
winClipPicture (Rect {rleft=a1,rtop=a2,rright=a3,rbottom=a4}) a5
	= cWinClipPicture a1 a2 a3 a4 a5
foreign import stdcall "WinClipPicture" cWinClipPicture :: Int -> Int -> Int -> Int -> HDC -> IO ()

--	Operation to set the clipping region.
foreign import stdcall "WinClipRgnPicture" winClipRgnPicture :: HRGN -> HDC -> IO ()

--	Operation to set the complete clipping region.
foreign import stdcall "WinSetClipRgnPicture" winSetClipRgnPicture :: HRGN -> HDC -> IO ()

--	Operation to retrieve the current clipping region.
foreign import stdcall "WinGetClipRgnPicture" winGetClipRgnPicture :: HDC -> IO HRGN

foreign import stdcall "WinDeleteObject" winDeleteObject :: Ptr () -> IO ()

winDonePicture :: HDC -> IO (Int,Int,RGBcolor,RGBcolor,Pt,Fnt)
winDonePicture a1
	= do {
		o1 <- malloc;
		o2 <- malloc;
		o3 <- malloc;
		o4 <- malloc;
		o5 <- malloc;
		o6 <- malloc;
		o7 <- malloc;
		o8 <- malloc;
		o9 <- malloc;
		o10 <- malloc;
		o11 <- malloc;
		o12 <- malloc;
		o13 <- malloc;
		cWinDonePicture a1 o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13;
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		r3 <- fpeek o3;
		r4 <- fpeek o4;
		r5 <- fpeek o5;
		r6 <- fpeek o6;
		r7 <- fpeek o7;
		r8 <- fpeek o8;
		r9 <- fpeek o9;
		r10 <- fpeek o10;
		clstr11 <- fpeek o11;
		s11 <- peekCString clstr11;
		free clstr11;
		r12 <- fpeek o12;
		r13 <- fpeek o13;
		return (r1,r2,(r3,r4,r5),(r6,r7,r8),(r9,r10),(s11,r12,r13))
	  }
foreign import stdcall "WinDonePicture" cWinDonePicture :: HDC -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr CString -> Ptr Int -> Ptr Int -> IO ()

winInitPicture :: Int -> Int -> RGBcolor -> RGBcolor -> Pt -> Fnt -> Pt -> HDC -> IO ()
winInitPicture a1 a2 (a3,a4,a5) (a6,a7,a8) (a9,a10) (a11,a12,a13) (a14,a15) a16
	= withCString a11 (\s -> cWinInitPicture a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 s a12 a13 a14 a15 a16)
foreign import stdcall "WinInitPicture" cWinInitPicture :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> CString -> Int -> Int -> Int -> Int -> HDC -> IO ()


-- MW...

getResolutionC :: HDC -> IO (Int, Int)
getResolutionC a1 = do
	o1 <- malloc
	o2 <- malloc
	cgetResolutionC a1 o1 o2
	r1 <- fpeek o1
	r2 <- fpeek o2
	return (r1, r2)

foreign import stdcall "getResolutionC" cgetResolutionC :: HDC -> Ptr Int -> Ptr Int -> IO ()


getPictureScalingFactors :: HDC -> IO ((Int, Int), (Int, Int))
getPictureScalingFactors a1 = do
	o1 <- malloc
	o2 <- malloc
	o3 <- malloc
	o4 <- malloc
	cWinGetPictureScaleFactor a1 o1 o2 o3 o4
	r1 <- fpeek o1
	r2 <- fpeek o2
	r3 <- fpeek o3
	r4 <- fpeek o4
	return ((r1, r2), (r3, r4))
	
foreign import stdcall "WinGetPictureScaleFactor" cWinGetPictureScaleFactor :: HDC -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
-- .. MW
