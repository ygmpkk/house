module StdControlDef ( module StdControlDef
                     , module StdIOCommon
                     , module StdGUI
                     ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--
--	StdControlDef contains the types to define the standard set of controls.
--	********************************************************************************


import StdGUI
import StdIOCommon
import StdPicture(PenAttribute(..), Look)


data	ButtonControl ls ps
 =	ButtonControl String                      				[ControlAttribute ls ps]

data	CheckControl  ls ps
 = 	CheckControl  [CheckControlItem ps (ls,ps)]  RowsOrColumns       	[ControlAttribute ls ps]

data	CompoundControl c ls ps
 = 	CompoundControl (c ls ps)                                		[ControlAttribute ls ps]

data	CustomButtonControl ls ps
 = 	CustomButtonControl Size Look                                        	[ControlAttribute ls ps]

data	CustomControl       ls ps
 = 	CustomControl       Size Look                                        	[ControlAttribute ls ps]

data	EditControl   ls ps
 =	EditControl   String ControlWidth NrLines 				[ControlAttribute ls ps]

data	LayoutControl     c ls ps
 = 	LayoutControl     (c ls ps)                                  		[ControlAttribute ls ps]

data	PopUpControl 	ls ps
 = 	PopUpControl   	[PopUpControlItem ps (ls,ps)] Index 			[ControlAttribute ls ps]

data	RadioControl   	ls ps
 = 	RadioControl    [RadioControlItem ps (ls,ps)] RowsOrColumns Index 	[ControlAttribute ls ps]

data	SliderControl  	ls ps
 = 	SliderControl   Direction ControlWidth SliderState  (SliderAction  ls ps) [ControlAttribute ls ps]

data	TextControl   ls ps
 =	TextControl   String                      				[ControlAttribute ls ps]


type	CheckControlItem ps st = (String, Maybe ControlWidth, MarkState, st -> GUI ps st)
type	PopUpControlItem ps st = (String,                                st -> GUI ps st)
type	RadioControlItem ps st = (String, Maybe ControlWidth,            st -> GUI ps st)


type	NrLines
 =	Int
data	RowsOrColumns
	= Rows       Int
	| Columns    Int
data	ControlWidth                                -- The width of the control:
 =	PixelWidth   Int                            -- the exact number of pixels
 |	TextWidth    String                         -- the exact string width in dialog font
 |	ContentWidth String                         -- width of the control as if string is its content

data	ControlAttribute ls ps            	    -- Default:
 -- General control attributes:
 =	ControlActivate     (GUIFun ls ps)          -- return
 |	ControlDeactivate   (GUIFun ls ps)   	    -- return
 |	ControlFunction (GUIFun ls ps)              -- (\st->return st)
 |	ControlHide                                 -- initially visible
 |	ControlId       Id                          -- no id
 |	ControlKeyboard KeyboardStateFilter SelectState (KeyboardFunction ls ps)
	                                            -- no keyboard input/overruled
 |	ControlMinimumSize  Size                    -- zero
 |	ControlModsFunction (ModifiersFunction ls ps)
                                                    -- ControlFunction
 |	ControlMouse        MouseStateFilter    SelectState (MouseFunction ls ps)
		                                    -- no mouse input/overruled
 |	ControlPen	    [PenAttribute]	    -- default pen attributes
 |	ControlPos          ItemPos                 -- (RightTo previous,zero)
 |	ControlResize       ControlResizeFunction   -- no resize
 |	ControlSelectState  SelectState             -- control Able
 |	ControlTip          String                  -- no tip
 |	ControlWidth        ControlWidth            -- system derived
 --	For CompoundControls only:
 |	ControlHMargin      Int Int                 -- system dependent
 |	ControlHScroll      ScrollFunction          -- no horizontal scrolling
 |	ControlItemSpace    Int Int                 -- system dependent
 |	ControlLook         Bool Look               -- control is transparant
 |	ControlOrigin       Point2                  -- Left top of ViewDomain
 |	ControlOuterSize    Size		    -- enclose elements
 |	ControlViewDomain   ViewDomain              -- {zero,max range}
 |	ControlViewSize     Size                    -- enclose elements
 |	ControlVMargin      Int Int                 -- system dependent
 |	ControlVScroll      ScrollFunction          -- no vertical   scrolling




type ControlResizeFunction =
	Size ->                                     		-- current control outer size
	Size ->                                     		-- old     parent  view  size
	Size ->                                     		-- new     parent  view  size
	Size                                        		-- new     control outer size



type ControlType = String