{-# OPTIONS -#include "OSWindows\Windows_C_12\cCCallWindows_121.h" #-}
{-# OPTIONS -#include "OSWindows\Windows_C_12\cpicture_121.h" #-}
{-# OPTIONS -#include "OSWindows\Windows_C_12\cCrossCallWindows_121.h" #-}

module WindowCCall_12 ( winInvalidateWindow, winInvalidateRect
                      , winValidateRect, winValidateRgn
                      , winGetDC, winReleaseDC                      
                      , module OSTypes
                      ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	WindowCCall_12 contains C calls for handling windows. 
--	********************************************************************************


import Cutil_12
import OSTypes
import RgnCCall_12
import PictCCall_12


foreign import stdcall "WinInvalidateWindow" winInvalidateWindow :: HWND -> IO ()

winInvalidateRect :: HWND -> (Int,Int,Int,Int) -> IO ()
winInvalidateRect a1 (a2,a3,a4,a5)
	= cWinInvalidateRect a1 a2 a3 a4 a5
foreign import stdcall "WinInvalidateRect" cWinInvalidateRect :: HWND -> Int -> Int -> Int -> Int -> IO ()

winValidateRect :: HWND -> (Int,Int,Int,Int) -> IO ()
winValidateRect a1 (a2,a3,a4,a5)
	= cWinValidateRect a1 a2 a3 a4 a5
foreign import stdcall "WinValidateRect" cWinValidateRect :: HWND -> Int -> Int -> Int -> Int -> IO ()


foreign import stdcall "WinValidateRgn" winValidateRgn :: HWND -> HRGN -> IO ()
foreign import stdcall "WinGetDC" winGetDC :: HWND -> IO HDC
foreign import stdcall "WinReleaseDC" winReleaseDC :: HWND -> HDC -> IO ()