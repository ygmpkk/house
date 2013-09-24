module StdWindowDef ( module StdWindowDef
                    , module StdIOCommon
                    , module StdGUI
                    , Look, PenAttribute(..)
                    ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdWindowDef contains the types to define the standard set of dialogs.
--	********************************************************************************


import StdGUI
import StdIOCommon
import StdPicture(PenAttribute(..), Look)


data	Dialog c ls ps = Dialog Title (c ls ps) [WindowAttribute ls ps]
data	Window c ls ps = Window Title (c ls ps) [WindowAttribute ls ps]

data	WindowAttribute ls ps                        -- Default:
 --	Attributes for Windows and Dialogs:
 =	WindowActivate   (GUIFun ls ps)              -- id
 |	WindowClose      (GUIFun ls ps)              -- user can't close window
 |	WindowDeactivate (GUIFun ls ps)              -- id
 |	WindowHMargin	 Int Int		     -- system dependent
 |	WindowId         Id                          -- system defined id
 |	WindowIndex	 Int			     -- open front-most
 |	WindowInit       (GUIFun ls ps)              -- no actions after opening window
 |	WindowInitActive Id			     -- system dependent
 |	WindowItemSpace	 Int Int		     -- system dependent
 |	WindowOuterSize	 Size			     -- screen size
 |	WindowPos	 ItemPos		     -- system dependent
 |	WindowViewSize   Size                        -- screen size
 |	WindowVMargin	 Int Int		     -- system dependent
 --	Attributes for Dialog only:	
 |	WindowCancel	 Id			     -- no cancel  (Custom)ButtonControl
 |	WindowOk	 Id			     -- no default (Custom)ButtonControl
 --	Attributes for Windows only:	
 | 	WindowCursor	 CursorShape		     -- no change of cursor
 |	WindowHScroll	 ScrollFunction		     -- no horizontal scrolling
 |	WindowKeyboard	 KeyboardStateFilter SelectState (KeyboardFunction ls ps) -- no keyboard input
 |	WindowLook	 Bool Look		     -- show system dependent background
 |	WindowMouse	 MouseStateFilter SelectState (MouseFunction ls ps) -- no mouse input
 |	WindowOrigin	 Point2			     -- left top of picture domain
 |	WindowPen	 [PenAttribute]		     -- default pen attributes
 |	WindowSelectState	SelectState	     -- Able
 |	WindowViewDomain	ViewDomain	     -- {zero,max range}
 |	WindowVScroll		ScrollFunction	     -- no vertical scrolling
 |	WindowCaret	Point2 Size		     -- no caret