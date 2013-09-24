module StdSystem where


--  ********************************************************************************
--  Clean to Haskell Standard Object I/O library, version 1.2
--  
--  WindowHandle contains the internal data structures that represent the 
--  state of windows. 
--  ********************************************************************************


import	OSSystem
import	StdIOBasic(Size(..))


-- MW11++
newLineChars :: String
newLineChars = osNewLineChars

--	System dependencies concerning the time resolution
ticksPerSecond :: Int
ticksPerSecond = osTicksPerSecond


--	System dependencies concerning the screen resolution
mmperinch = 25.4

hmm :: Float -> Int
hmm mm = osMMtoHPixels mm

vmm :: Float -> Int
vmm mm = osMMtoVPixels mm

hinch :: Float -> Int
hinch inch = osMMtoHPixels (inch*mmperinch)

vinch :: Float -> Int
vinch inch = osMMtoVPixels (inch*mmperinch)

maxScrollWindowSize :: Size
maxScrollWindowSize = Size {w=w,h=h}
	where (w,h) = osMaxScrollWindowSize

maxFixedWindowSize :: Size
maxFixedWindowSize = Size {w=w,h=h}
	where (w,h) = osMaxScrollWindowSize

-- MW11++
printSetupTypical :: Bool
printSetupTypical = osPrintSetupTypical
