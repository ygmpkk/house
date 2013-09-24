{-# OPTIONS -#include "OSWindows\Windows_C_12\cpicture_121.h" #-}

module RgnCCall_12 ( HRGN
                   , rgn_AND, rgn_OR, rgn_XOR, rgn_DIFF, rgn_COPY
                   , winCreateRectRgn, winCreatePolygonRgn
                   , winSetRgnToRect
                   , winCombineRgn
                   , winGetRgnBox
                   ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	RgnCCall_12 contains OS operations to manage regions.
--	********************************************************************************


import Cutil_12

type HRGN = Ptr ()


--	CombineRgn() Styles.
rgn_AND, rgn_OR, rgn_XOR, rgn_DIFF, rgn_COPY :: Int
rgn_AND		= 1
rgn_OR		= 2
rgn_XOR		= 3
rgn_DIFF	= 4
rgn_COPY	= 5


{-	Operations to create, modify and destroy regions.
-}
foreign import stdcall "WinCreateRectRgn" winCreateRectRgn :: Int -> Int -> Int -> Int -> IO HRGN
foreign import stdcall "WinCreatePolygonRgn" winCreatePolygonRgn :: Ptr Int -> Int -> Int -> IO HRGN
foreign import stdcall "WinSetRgnToRect" winSetRgnToRect :: Int -> Int -> Int -> Int -> HRGN -> IO HRGN
foreign import stdcall "WinCombineRgn" winCombineRgn :: HRGN -> HRGN -> HRGN -> Int -> IO HRGN

winGetRgnBox :: HRGN -> IO (Int,Int,Int,Int,Bool,Bool)
winGetRgnBox a1 = do
    -- Marshal arguments:
    o1 <- malloc
    o2 <- malloc
    o3 <- malloc
    o4 <- malloc
    o5 <- malloc
    o6 <- malloc
    -- Call C:
    cWinGetRgnBox a1 o1 o2 o3 o4 o5 o6
    -- Read/free:
    r1 <- fpeek o1
    r2 <- fpeek o2
    r3 <- fpeek o3
    r4 <- fpeek o4
    r5 <- fpeek o5
    r6 <- fpeek o6
    return (r1,r2,r3,r4,r5,r6)

foreign import stdcall "WinGetRgnBox" cWinGetRgnBox :: HRGN -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Bool -> Ptr Bool -> IO ()
