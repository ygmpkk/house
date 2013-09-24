module StdIOCommon ( module StdIOCommon
                   , module StdIOBasic
                   , module StdKey
                   , module Maybe
                   , Id.Id, Id.R2Id, Id.RId, Id.rIdtoId, Id.r2IdtoId
                   ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdIOCommon defines common types and access functions for the I/O library.
--	********************************************************************************


import Id (Id, RId, R2Id, rIdtoId, r2IdtoId)
import StdIOBasic
import StdKey
import Maybe
import Data.Dynamic


{-	The SelectState and MarkState types.			-}

data	SelectState
	= Able | Unable
	deriving (Eq,Show)
	
data	MarkState
	= Mark | NoMark
	deriving (Eq,Show)

enabled :: SelectState -> Bool					-- @1 == Able
enabled Able   = True
enabled unable = False

marked :: MarkState -> Bool					-- @1 == Able
marked Mark   = True
marked unable = False

instance Toggle SelectState where				-- Able <-> Unable
	toggle Able = Unable
	toggle _    = Able
	
	
instance Toggle MarkState where					-- Mark <-> NoMark
	toggle Mark = NoMark
	toggle _    = Mark


{-	The KeyboardState type.					-}

data	KeyboardState
	= CharKey    Char       KeyState			-- ASCII character input
	| SpecialKey SpecialKey KeyState Modifiers		-- Special key input
	| KeyLost						-- Key input lost while key was down
	deriving (Eq,Show)
data	KeyState
	= KeyDown    IsRepeatKey				-- Key is down
	| KeyUp							-- Key goes up
	deriving (Eq,Show)
type	IsRepeatKey						-- Flag on key down:
	= Bool							-- True iff key is repeating
data	Key
	= IsCharKey    Char
	| IsSpecialKey SpecialKey
type	KeyboardStateFilter					-- Predicate on KeyboardState:
	= KeyboardState -> Bool					-- evaluate KeyFunction only if True

getKeyboardStateKeyState:: KeyboardState -> KeyState		-- KeyUp   if KeyLost
getKeyboardStateKeyState (CharKey _ keyState)
	= keyState
getKeyboardStateKeyState (SpecialKey _ keyState _)
	= keyState
getKeyboardStateKeyState KeyLost
	= KeyUp

getKeyboardStateKey :: KeyboardState -> Maybe Key		-- Nothing if KeyLost
getKeyboardStateKey (CharKey char _)
	= Just (IsCharKey char)
getKeyboardStateKey (SpecialKey special _ _)
	= Just (IsSpecialKey special)
getKeyboardStateKey KeyLost
	= Nothing
	
	
{-	The MouseState type.					-}

data	MouseState
	= MouseMove	Point2 Modifiers		-- Mouse is up     (position,modifiers)
	| MouseDown	Point2 Modifiers Int		-- Mouse goes down (and nr down)
	| MouseDrag	Point2 Modifiers		-- Mouse is down   (position,modifiers)
	| MouseUp	Point2 Modifiers		-- Mouse goes up   (position,modifiers)
	| MouseLost					-- Mouse input lost while mouse was down
	deriving (Eq, Show)
	
data	ButtonState
 	= ButtonStillUp					-- MouseMove
 	| ButtonDown					-- MouseDown _ _ 1
	| ButtonDoubleDown				--			 _ _ 2
	| ButtonTripleDown				--           _ _ >2
	| ButtonStillDown				-- MouseDrag
 	| ButtonUp					-- MouseUp/MouseLost
 	deriving (Eq, Show)
 	
type	MouseStateFilter =				-- Predicate on MouseState:
		MouseState -> Bool			-- evaluate MouseFunction only if True


getMouseStatePos :: MouseState -> Point2
getMouseStatePos (MouseMove pos _)	= pos
getMouseStatePos (MouseDown pos _ _)	= pos
getMouseStatePos (MouseDrag pos _)	= pos
getMouseStatePos (MouseUp   pos _)	= pos
getMouseStatePos MouseLost		= zero

getMouseStateModifiers :: MouseState -> Modifiers
getMouseStateModifiers (MouseMove _ mods)	= mods
getMouseStateModifiers (MouseDown _ mods _)	= mods
getMouseStateModifiers (MouseDrag _ mods)	= mods
getMouseStateModifiers (MouseUp   _ mods)	= mods
getMouseStateModifiers MouseLost		= noModifiers

getMouseStateButtonState:: MouseState	-> ButtonState
getMouseStateButtonState (MouseMove _ _)	= ButtonStillUp
getMouseStateButtonState (MouseDown _ _ nr)	= 
	case nr of
	  1 -> ButtonDown 
	  2 -> ButtonDoubleDown
	  _ -> ButtonTripleDown

getMouseStateButtonState (MouseDrag _ _)	= ButtonStillDown
getMouseStateButtonState (MouseUp   _ _)	= ButtonUp
getMouseStateButtonState MouseLost		= ButtonUp


{-	The SliderState type.					-}

data	SliderState 
	= SliderState
		{ sliderMin	:: !Int
		, sliderMax	:: !Int
		, sliderThumb	:: !Int
		}
	deriving (Eq, Show)
		
		
{-	The UpdateState type.					-}
data	UpdateState
	= UpdateState
		{ oldFrame	:: !ViewFrame
		, newFrame	:: !ViewFrame
		, updArea	:: !UpdateArea
		}
	deriving (Show)
type	ViewDomain = Rectangle
type	ViewFrame  = Rectangle
type	UpdateArea = [ViewFrame]

rectangleToUpdateState :: Rectangle -> UpdateState
rectangleToUpdateState frame
	= UpdateState {oldFrame=frame,newFrame=frame,updArea=[frame]}

{-	viewDomainRange defines the minimum and maximum values for ViewDomains.
	viewFrameRange  defines the minimum and maximum values for ViewFrames.
	Values based on Clean maxSigned4ByteInt (see also CommonDef.hs).
-}
viewDomainRange :: ViewDomain
viewDomainRange
	= Rectangle
		{ corner1 = Point2 {x = -1073741824,y = -1073741824}
		, corner2 = Point2 {x =  1073741824,y =  1073741824}
		}

viewFrameRange :: ViewFrame
viewFrameRange
	= Rectangle
		{ corner1 = Point2 {x = 2147483647,y = 2147483647}
		, corner2 = Point2 {x = 2147483647,y = 2147483647}
		}


{-	Modifiers indicates the meta keys that have been pressed (True) or not (False). -}
data	Modifiers
	= Modifiers
		{ shiftDown	:: !Bool			-- True iff shift   down
		, optionDown	:: !Bool			-- True iff option  down
		, commandDown	:: !Bool			-- True iff command down
		, controlDown	:: !Bool			-- True iff control down
		, altDown	:: !Bool			-- True iff alt     down
		}
	deriving (Eq,Show)


--	Constants to check which of the Modifiers are down.

noModifiers = Modifiers {shiftDown = False, optionDown = False, commandDown = False, controlDown = False, altDown = False}
shiftOnly   = Modifiers {shiftDown = True,  optionDown = False, commandDown = False, controlDown = False, altDown = False}
optionOnly  = Modifiers {shiftDown = False, optionDown = True,  commandDown = False, controlDown = False, altDown = True }
commandOnly = Modifiers {shiftDown = False, optionDown = False, commandDown = True,  controlDown = True,  altDown = False}
controlOnly = Modifiers {shiftDown = False, optionDown = False, commandDown = True,  controlDown = True,  altDown = False}
altOnly     = Modifiers {shiftDown = False, optionDown = True,  commandDown = False, controlDown = False, altDown = True }


{-	The layout language used for windows and controls.	-}
type	ItemPos
	=	( ItemLoc
		, ItemOffset
		)
data	ItemLoc
 --	Absolute:
	= Fix
 --	Relative to corner:
	| LeftTop
	| RightTop
	| LeftBottom
	| RightBottom
 --	Relative in next line:
	| Left
	| Center
	| Right
 --	Relative to other item:
	| LeftOf  Id
	| RightTo Id
	| Above   Id
	| Below   Id
 --	Relative to previous item:
	| LeftOfPrev
	| RightToPrev
	| AbovePrev
	| BelowPrev
	deriving (Eq,Show)
type	ItemOffset
	= Vector2						-- A constant offset vector
	
	
{-	The Direction type.					-}

data	Direction
	= Horizontal
	| Vertical
	deriving Eq

{-	The CursorShape type.					-}

data	CursorShape
	= StandardCursor
	| BusyCursor
	| IBeamCursor
	| CrossCursor
	| FatCrossCursor
	| ArrowCursor
	| HiddenCursor
	deriving Eq

{-	Document interface of interactive processes.		-}

data	DocumentInterface
	= NDI							-- No       Document Interface
	| SDI							-- Single   Document Interface
	| MDI							-- Multiple Document Interface
	deriving (Eq,Show)

data	SliderMove
	= SliderIncSmall
	| SliderDecSmall
	| SliderIncLarge
	| SliderDecLarge
	| SliderThumb Int
	deriving Show


{-	Common error report types.				-}

data	ErrorReport						-- Usual cause:	
	= ErrorViolateDI					-- Violation against DocumentInterface
	| ErrorIdsInUse						-- Object contains Ids that are bound
	| ErrorUnknownObject					-- Object can not be found
	| ErrorNotifierOpen					-- It was tried to open a second send notifier
	| ErrorUnableReceiver
	| ErrorDeadlock
	| OtherError !String					-- Other kind of error
	deriving (Eq,Show)

{-# NOINLINE errTy #-}
errTy = mkAppTy (mkTyCon "ErrorReport") []

instance Typeable ErrorReport where
	typeOf _ = errTy

handleErrorReport :: Monad m => ErrorReport -> m a
handleErrorReport ErrorViolateDI = fail "Object I/O: Violation against DocumentInterface"
handleErrorReport ErrorIdsInUse	= fail "Object I/O: Object contains Ids that are bound"
handleErrorReport ErrorUnknownObject = fail "Object I/O: Object can not be found"
handleErrorReport ErrorNotifierOpen = fail "Object I/O: It was tried to open a second send notifier"
handleErrorReport ErrorUnableReceiver = fail "Object I/O: unable receiver"
handleErrorReport ErrorDeadlock = fail "Object I/O: deadlock"
handleErrorReport (OtherError msg) = fail ("Object I/O: " ++ msg)