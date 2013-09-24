module OSTypes where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	OSTypes defines standard types for the OS.
--	********************************************************************************

data	Rect					-- A Rect is supposed to be an ordered rectangle with
	= Rect
		{ rleft   :: !Int		-- rleft<=rright && rtop<=rbottom
		, rtop    :: !Int
		, rright  :: !Int
		, rbottom :: !Int
		}
	deriving (Eq)
type	OSWindowPtr
	= HWND
type	HWND
	= Int

osNoWindowPtr :: OSWindowPtr
osNoWindowPtr = -1

data	DelayActivationInfo
	= DelayActivatedWindow    OSWindowPtr			-- the window has become active
	| DelayDeactivatedWindow  OSWindowPtr			-- the window has become inactive
	| DelayActivatedControl   OSWindowPtr OSWindowPtr	-- the control (@2) in window (@1) has become active
	| DelayDeactivatedControl OSWindowPtr OSWindowPtr	-- the control (@2) in window (@1) has become inactive
