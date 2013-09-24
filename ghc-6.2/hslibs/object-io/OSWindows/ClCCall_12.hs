{-# OPTIONS -#include "OSWindows\Windows_C_12\cCCallWindows_121.h" #-}
{-# OPTIONS -#include "OSWindows\Windows_C_12\cCrossCall_121.h" #-}
{-# OPTIONS -#include "OSWindows\Windows_C_12\cCCallSystem_121.h" #-}
{-# OPTIONS -#include "OSWindows\Windows_C_12\cpicture_121.h" #-}
{-# OPTIONS -#include "OSWindows\Windows_C_12\cCrossCallxDI_121.h" #-}

module ClCCall_12 ( ACCLPTR
                  , winHelpKey, winEscapeKey, winReturnKey, winTabKey, winDelKey, winBackSpKey
                  , winEndKey, winBeginKey, winPgDownKey, winPgUpKey, winRightKey, winLeftKey, winDownKey, winUpKey
                  , winF1Key, winF2Key, winF3Key, winF4Key, winF5Key, winF6Key
                  , winF7Key, winF8Key, winF9Key, winF10Key, winF11Key, winF12Key
                  , ctrlBIT, altBIT, shiftBIT
                  , keyREPEAT, keyUP, keyDOWN
                  , buttonUP, buttonSTILLDOWN, buttonTRIPLEDOWN, buttonDOUBLEDOWN, buttonDOWN, buttonSTILLUP                  
                  , winBeep
                  , winSetDoubleDownDist
                  , winGetHorzResolution, winGetVertResolution
                  , winMaxFixedWindowSize, winMaxScrollWindowSize
                  , winScreenYSize, winScreenXSize
                  , winMinimumWinSize
                  , winScrollbarSize
                  , winMDIClientToOuterSizeDims, winSDIClientToOuterSizeDims
                  , winPlaySound
                  ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	ClCCall_12 collects all the C call routines related to windows.
--	********************************************************************************


import Cutil_12
import System.IO.Unsafe


type	ACCLPTR		= Int

winHelpKey, winEscapeKey, winReturnKey, winTabKey, winDelKey, winBackSpKey, winEndKey, winBeginKey
	  , winPgDownKey, winPgUpKey, winRightKey, winLeftKey, winDownKey, winUpKey
	  , winF1Key, winF2Key, winF3Key, winF4Key, winF5Key, winF6Key, winF7Key
	  , winF8Key, winF9Key, winF10Key, winF11Key, winF12Key :: Int
winHelpKey		= 5
winEscapeKey		= 27
winReturnKey		= 13
winTabKey		= 9
winDelKey		= 127
winBackSpKey		= 8
winEndKey		= 4
winBeginKey		= 1
winPgDownKey		= 12
winPgUpKey		= 11
winRightKey		= 29
winLeftKey		= 28
winDownKey		= 31
winUpKey		= 30
winF1Key		= 1001
winF2Key		= 1002
winF3Key		= 1003
winF4Key		= 1004
winF5Key		= 1005
winF6Key		= 1006
winF7Key		= 1007
winF8Key		= 1008
winF9Key		= 1009
winF10Key		= 1010
winF11Key		= 1011
winF12Key		= 1012

ctrlBIT, altBIT, shiftBIT :: Int
ctrlBIT			= 4
altBIT			= 2
shiftBIT		= 1

keyREPEAT, keyUP, keyDOWN :: Int
keyREPEAT		= 4
keyUP			= 2
keyDOWN			= 1

buttonUP, buttonSTILLDOWN, buttonTRIPLEDOWN, buttonDOUBLEDOWN, buttonDOWN, buttonSTILLUP :: Int
buttonUP		= 50
buttonSTILLDOWN		= 40
buttonTRIPLEDOWN	= 3
buttonDOUBLEDOWN	= 2
buttonDOWN		= 1
buttonSTILLUP		= 0		{- constant for passing mouse move events. -}

foreign import stdcall "WinBeep" winBeep :: IO ()
foreign import stdcall "WinSetDoubleDownDist" winSetDoubleDownDist :: Int -> IO ()

winGetHorzResolution :: Int
winGetHorzResolution
	= unsafePerformIO cWinGetHorzResolution
foreign import stdcall "WinGetHorzResolution" cWinGetHorzResolution :: IO Int

winGetVertResolution :: Int
winGetVertResolution
	= unsafePerformIO cWinGetVertResolution
foreign import stdcall "WinGetVertResolution" cWinGetVertResolution :: IO Int

winMaxFixedWindowSize :: (Int,Int)
winMaxFixedWindowSize
	= unsafePerformIO winMaxFixedWindowSize'
	where
		winMaxFixedWindowSize' :: IO (Int,Int)
		winMaxFixedWindowSize'
			= do {
				-- Marshal arguments:
				o1 <- malloc;
				o2 <- malloc;
				-- Call C:
				cWinMaxFixedWindowSize o1 o2;
				-- Read/free:
				r1 <- fpeek o1;
				r2 <- fpeek o2;
				return (r1,r2)
			  }
foreign import stdcall "WinMaxFixedWindowSize" cWinMaxFixedWindowSize :: Ptr Int -> Ptr Int -> IO ()

winMaxScrollWindowSize :: (Int,Int)
winMaxScrollWindowSize
	= unsafePerformIO winMaxScrollWindowSize'
	where
		winMaxScrollWindowSize' :: IO (Int,Int)
		winMaxScrollWindowSize'
			= do {
				-- Marshal arguments:
				o1 <- malloc;
				o2 <- malloc;
				-- Call C:
				cWinMaxScrollWindowSize o1 o2;
				-- Read/free:
				r1 <- fpeek o1;
				r2 <- fpeek o2;
				return (r1,r2)
			  }
foreign import stdcall "WinMaxScrollWindowSize" cWinMaxScrollWindowSize :: Ptr Int -> Ptr Int -> IO ()

foreign import stdcall "WinScreenYSize" winScreenYSize :: IO Int

foreign import stdcall "WinScreenXSize" winScreenXSize :: IO Int

winMinimumWinSize :: (Int,Int)
winMinimumWinSize
	= unsafePerformIO winMinimumWinSize'
	where
		winMinimumWinSize' :: IO (Int,Int)
		winMinimumWinSize'
			= do {
				-- Marshal arguments:
				o1 <- malloc;
				o2 <- malloc;
				-- Call C:
				cWinMinimumWinSize o1 o2;
				-- Read/free:
				r1 <- fpeek o1;
				r2 <- fpeek o2;
				return (r1, r2)
			  }
foreign import stdcall "WinMinimumWinSize" cWinMinimumWinSize :: Ptr Int -> Ptr Int -> IO ()

winScrollbarSize :: IO (Int,Int)
winScrollbarSize
	= do {
		-- Marshal arguments:
		o1 <- malloc;
		o2 <- malloc;
		-- Call C:
		cWinScrollbarSize o1 o2;
		-- Read/free:
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		return (r1, r2)
	  }
foreign import stdcall "WinScrollbarSize" cWinScrollbarSize :: Ptr Int -> Ptr Int -> IO ()

{-	The routines (win(M/S)DIClientToOuterSizeDims convert between the
		client and outer size of (M/S)DI windows. The Int argument contains the style flags 
		of the window.
-}
winMDIClientToOuterSizeDims :: Int -> IO (Int,Int)
winMDIClientToOuterSizeDims a1
	= do {
		-- Marshal arguments:
		o1 <- malloc;
		o2 <- malloc;
		-- Call C:
		cWinMDIClientToOuterSizeDims a1 o1 o2;
		-- Read/free:
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		return (r1, r2)
	  }
foreign import stdcall "WinMDIClientToOuterSizeDims" cWinMDIClientToOuterSizeDims :: Int -> Ptr Int -> Ptr Int -> IO ()

winSDIClientToOuterSizeDims :: Int -> IO (Int,Int)
winSDIClientToOuterSizeDims a1
	= do {
		-- Marshal arguments:
		o1 <- malloc;
		o2 <- malloc;
		-- Call C:
		cWinSDIClientToOuterSizeDims a1 o1 o2;
		-- Read/free:
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		return (r1, r2)
	  }
foreign import stdcall "WinSDIClientToOuterSizeDims" cWinSDIClientToOuterSizeDims :: Int -> Ptr Int -> Ptr Int -> IO ()

winPlaySound :: String -> IO Bool
winPlaySound a1 = withCString a1 cWinPlaySound

foreign import stdcall "WinPlaySound" cWinPlaySound :: CString -> IO Bool
