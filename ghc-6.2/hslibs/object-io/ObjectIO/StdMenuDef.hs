module StdMenuDef where


-- ********************************************************************************
-- Clean to Haskell Standard Object I/O library, version 1.2
-- Definition of Menus and MenuElements:
-- ********************************************************************************

import  StdGUI
import	StdIOCommon


--	Menus:

data	Menu        m ls ps = Menu        Title         (m ls ps)        [MenuAttribute ls ps]
data	PopUpMenu   m ls ps = PopUpMenu                 (m ls ps)


--	Menu elements:

data	MenuItem      ls ps = MenuItem    Title                       	   [MenuAttribute ls ps]
data	MenuSeparator ls ps = MenuSeparator                           	   [MenuAttribute ls ps]
data	RadioMenu     ls ps = RadioMenu   [MenuRadioItem (ls,ps) ps] Index [MenuAttribute ls ps]
data	SubMenu     m ls ps = SubMenu     Title        (m ls ps)       	   [MenuAttribute ls ps]

type	MenuRadioItem st ps = (Title,Maybe Id,Maybe Char,st -> GUI ps st)

data	MenuAttribute ls ps							-- Default:
 --	Attributes for Menus and MenuElements:
	=	MenuId			Id					-- no Id
	|	MenuSelectState		SelectState				-- menu(item) Able
 --	Attributes only for Menus:
	|	MenuIndex		Int					-- at the end of the current menu list
	|	MenuInit		(ps -> GUI ps ps)			-- no actions after opening menu
 --	Attributes ignored by (Sub)Menus:
	|	MenuFunction		(GUIFun ls ps)				-- return
	|	MenuMarkState		MarkState				-- NoMark
	|	MenuModsFunction	(ModifiersFunction ls ps)		-- MenuFunction
	|	MenuShortKey		Char					-- no ShortKey

