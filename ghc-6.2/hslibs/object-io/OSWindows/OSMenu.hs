module OSMenu where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	********************************************************************************

import	MenuCrossCall_12
import	OSDocumentInterface(OSMenuBar(..))
import	OSTypes(OSWindowPtr, osNoWindowPtr)
import  Monad(when)
import  Char(toUpper)


--	Types for menus and menu elements:
type	OSMenuHandle	= HMENU
type	OSMenu		= HMENU
type	OSMenuItem	= HITEM
type	OSMenuSeparator	= HITEM

--	Dummy values:
osNoMenu 		= 0 :: OSMenu
osNoMenuItem 		= 0 :: OSMenuItem
osNoMenuSeparator 	= 0 :: OSMenuSeparator

--	Enabling and disabling menus and menu elements:

osDisableMenu :: Int -> OSMenuBar -> IO ()
osDisableMenu zIndex osMenuBar@(OSMenuBar {menuBar=menuBar})
	= winChangeMenuAbility menuBar zIndex False

osEnableMenu :: Int -> OSMenuBar -> IO ()
osEnableMenu zIndex osMenuBar@(OSMenuBar {menuBar=menuBar})
	= winChangeMenuAbility menuBar zIndex True

osEnableMenuItem :: OSMenu -> OSMenuItem -> IO ()
osEnableMenuItem menuHandle item
	= winChangeItemAbility menuHandle item True

osDisableMenuItem :: OSMenu -> OSMenuItem -> IO ()
osDisableMenuItem menuHandle item
	= winChangeItemAbility menuHandle item False


--	Changing and updating the menu bar:

drawMenuBar :: OSMenuBar -> IO ()
drawMenuBar (OSMenuBar {menuWindow=menuWindow,menuClient=menuClient})
	= winDrawMenuBar menuWindow (if (menuClient==osNoWindowPtr) then 0 else menuClient)

osMenuBarClear :: IO ()
osMenuBarClear = return ()

osMenuBarSet :: OSMenuBar -> IO ()
osMenuBarSet menuBar = return ()
	
osMenuInsert :: Int -> String -> OSMenuBar -> IO OSMenu
osMenuInsert index title mbar = do
	menu <- winCreatePopupMenuHandle
	winInsertMenu title True menu (menuBar mbar) index
	return menu
	
osSubMenuInsert :: Int -> String -> OSMenu -> IO (OSMenu, OSMenu)
osSubMenuInsert index title parentMenu = do
	menu <- winCreatePopupMenuHandle
	winInsertMenu title True menu parentMenu index
	return (menu,parentMenu)

osMenuRemove :: OSMenu -> OSMenuBar -> IO ()
osMenuRemove menu menuBar@(OSMenuBar {menuBar=hmenu}) = do
	winDeleteMenu hmenu menu
	winDestroyMenu menu
	return ()

osSubMenuRemove :: OSMenu -> OSMenu -> IO ()
osSubMenuRemove submenu hmenu = do
	winDeleteMenu hmenu submenu
	winDestroyMenu submenu
	return ()

osCreatePopUpMenu :: IO OSMenu
osCreatePopUpMenu = winCreatePopupMenuHandle

osTrackPopUpMenu :: OSMenu -> OSWindowPtr -> IO Bool
osTrackPopUpMenu menu framePtr = winTrackPopupMenu menu framePtr


--	Changing (sub)menus:
osAppendMenuItem :: OSMenuBar -> Int -> OSMenu -> String -> Bool -> Bool -> Char -> IO (OSMenuItem,OSMenu)
osAppendMenuItem (OSMenuBar {menuWindow=menuWindow}) index menu title able mark key = do
	item <- winInsertMenuItem (if (key /= '\0') then (title ++ "\tCtrl+" ++ show (toUpper key)) else title) able mark menu index
	when (key /= '\0') (winAddMenuShortKey menuWindow item key)
	return (item,menu)


osAppendMenuSeparator :: Int -> OSMenu -> IO (OSMenuSeparator,OSMenu)
osAppendMenuSeparator index menu = do
	winInsertSeparator menu index
	return (osNoMenuSeparator,menu)

osChangeMenuTitle :: OSMenuBar -> OSMenu -> String -> IO ()
osChangeMenuTitle (OSMenuBar {menuBar=menuBar}) menu title = winModifyMenu title menu menuBar

osChangeMenuItemTitle :: OSMenu -> OSMenuItem -> String -> IO ()
osChangeMenuItemTitle menu item title = winModifyMenuItem title item menu

osMenuItemCheck :: Bool -> OSMenu -> OSMenuItem -> IO ()
osMenuItemCheck check menu item = winChangeMenuItemCheck menu item check

osMenuRemoveItem :: OSMenuItem -> OSMenu -> IO OSMenu
osMenuRemoveItem item menu = do
	winRemoveMenuItem menu item
	return menu
