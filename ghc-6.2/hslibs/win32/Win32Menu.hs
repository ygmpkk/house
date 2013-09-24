module Win32Menu 
{-
       (
         MenuName
       , checkMenuItem
       , checkMenuRadioItem
       , createMenu
       , createPopupMenu
       , deleteMenu
       , destroyMenu
       , drawMenuBar
       , enableMenuItem
       , getMenu
       , getMenuDefaultItem
       , getMenuItemCount
       , getMenuItemID
       , getMenuItemInfo
       , getMenuItemRect
       , getMenuState
       , getSubMenu
       , getSystemMenu
       , hiliteMenuItem
       , insertMenuItem
       , isMenu
       , loadMenu
       , menuItemFromPoint
       , setMenu
       , setMenuDefaultItem
       , setMenuItemBitmaps
       , setMenuItemInfo
       , trackPopupMenu
       , trackPopupMenuEx
       
       , GMDIFlag(..)
       , MenuItem(..)
       , MenuFlag(..)
       , MenuState(..)
       , TrackMenuFlag(..)
       , SystemMenuCommand(..)

         -- Obsolete:
       , appendMenu
       , insertMenu
       , modifyMenu
       , removeMenu
       
       ) -} where

import StdDIS
  
import Win32Types
import GDITypes


type MenuName = LPCSTR



checkMenuItem :: HMENU -> MenuItem -> MenuFlag -> IO Bool
checkMenuItem arg1 arg2 arg3 =
  prim_checkMenuItem arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_checkMenuItem_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_checkMenuItem_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_checkMenuItem_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_bool_ gc_res2) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_checkMenuItem" prim_checkMenuItem :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_checkMenuItem_gc_res2 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_checkMenuItem_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_checkMenuItem_gc_failstring :: Addr -> IO (Addr)

checkMenuRadioItem :: HMENU -> MenuItem -> MenuItem -> MenuItem -> MenuFlag -> IO ()
checkMenuRadioItem arg1 arg2 arg3 arg4 arg5 =
  prim_checkMenuRadioItem arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_checkMenuRadioItem_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_checkMenuRadioItem_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_checkMenuRadioItem" prim_checkMenuRadioItem :: Addr -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_checkMenuRadioItem_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_checkMenuRadioItem_gc_failstring :: Addr -> IO (Addr)

createMenu :: IO HMENU
createMenu =
  prim_createMenu
  >>= \ gc_result ->
  access_prim_createMenu_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_createMenu" prim_createMenu :: IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_createMenu_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_createMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_createMenu_gc_failstring :: Addr -> IO (Addr)

createPopupMenu :: IO HMENU
createPopupMenu =
  prim_createPopupMenu
  >>= \ gc_result ->
  access_prim_createPopupMenu_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createPopupMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createPopupMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_createPopupMenu" prim_createPopupMenu :: IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_createPopupMenu_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_createPopupMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_createPopupMenu_gc_failstring :: Addr -> IO (Addr)

drawMenuBar :: HWND -> IO ()
drawMenuBar arg1 =
  prim_drawMenuBar arg1
  >>= \ gc_result ->
  access_prim_drawMenuBar_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_drawMenuBar_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_drawMenuBar" prim_drawMenuBar :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_drawMenuBar_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_drawMenuBar_gc_failstring :: Addr -> IO (Addr)

type MenuState = MenuFlag

enableMenuItem :: HMENU -> MenuItem -> MenuFlag -> IO MenuState
enableMenuItem arg1 arg2 arg3 =
  prim_enableMenuItem arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_enableMenuItem_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_enableMenuItem_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_enableMenuItem_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_enableMenuItem" prim_enableMenuItem :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_enableMenuItem_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_enableMenuItem_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_enableMenuItem_gc_failstring :: Addr -> IO (Addr)

type GMDIFlag = UINT

type MenuFlag = UINT

foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_gMDI_USEDISABLED" gMDI_USEDISABLED :: GMDIFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_gMDI_GOINTOPOPUPS" gMDI_GOINTOPOPUPS :: GMDIFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_BYCOMMAND" mF_BYCOMMAND :: MenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_BYPOSITION" mF_BYPOSITION :: MenuFlag

type MenuItem = UINT

foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_INSERT" mF_INSERT :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_CHANGE" mF_CHANGE :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_APPEND" mF_APPEND :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_DELETE" mF_DELETE :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_REMOVE" mF_REMOVE :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_USECHECKBITMAPS" mF_USECHECKBITMAPS :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_POPUP" mF_POPUP :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_SYSMENU" mF_SYSMENU :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_HELP" mF_HELP :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_MOUSESELECT" mF_MOUSESELECT :: MenuItem
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mF_END" mF_END :: MenuItem

foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFT_STRING" mFT_STRING :: MenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFT_BITMAP" mFT_BITMAP :: MenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFT_MENUBARBREAK" mFT_MENUBARBREAK :: MenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFT_MENUBREAK" mFT_MENUBREAK :: MenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFT_OWNERDRAW" mFT_OWNERDRAW :: MenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFT_RADIOCHECK" mFT_RADIOCHECK :: MenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFT_SEPARATOR" mFT_SEPARATOR :: MenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFT_RIGHTORDER" mFT_RIGHTORDER :: MenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFT_RIGHTJUSTIFY" mFT_RIGHTJUSTIFY :: MenuFlag


foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFS_GRAYED" mFS_GRAYED :: MenuState
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFS_DISABLED" mFS_DISABLED :: MenuState
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFS_CHECKED" mFS_CHECKED :: MenuState
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFS_HILITE"  mFS_HILITE :: MenuState
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFS_ENABLED" mFS_ENABLED :: MenuState
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFS_UNCHECKED" mFS_UNCHECKED :: MenuState
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFS_UNHILITE" mFS_UNHILITE :: MenuState
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mFS_DEFAULT" mFS_DEFAULT :: MenuState

type TrackMenuFlag = UINT

foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_LEFTBUTTON" tPM_LEFTBUTTON :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_RIGHTBUTTON" tPM_RIGHTBUTTON :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_LEFTALIGN" tPM_LEFTALIGN :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_CENTERALIGN" tPM_CENTERALIGN :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_RIGHTALIGN" tPM_RIGHTALIGN :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_TOPALIGN" tPM_TOPALIGN :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_VCENTERALIGN" tPM_VCENTERALIGN :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_BOTTOMALIGN" tPM_BOTTOMALIGN :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_HORIZONTAL" tPM_HORIZONTAL :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_VERTICAL" tPM_VERTICAL :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_NONOTIFY" tPM_NONOTIFY :: TrackMenuFlag
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_tPM_RETURNCMD" tPM_RETURNCMD :: TrackMenuFlag

type SystemMenuCommand = UINT

foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_SIZE" sC_SIZE :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_MOVE" sC_MOVE :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_MINIMIZE" sC_MINIMIZE :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_MAXIMIZE" sC_MAXIMIZE :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_NEXTWINDOW" sC_NEXTWINDOW :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_PREVWINDOW" sC_PREVWINDOW :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_CLOSE" sC_CLOSE :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_VSCROLL" sC_VSCROLL :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_HSCROLL" sC_HSCROLL :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_MOUSEMENU" sC_MOUSEMENU :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_KEYMENU" sC_KEYMENU :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_ARRANGE" sC_ARRANGE :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_RESTORE" sC_RESTORE :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_TASKLIST" sC_TASKLIST :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_SCREENSAVE" sC_SCREENSAVE :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_HOTKEY" sC_HOTKEY :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_DEFAULT" sC_DEFAULT :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_MONITORPOWER" sC_MONITORPOWER :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_CONTEXTHELP" sC_CONTEXTHELP :: SystemMenuCommand
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_sC_SEPARATOR" sC_SEPARATOR :: SystemMenuCommand

isMenu :: HMENU -> IO Bool
isMenu arg1 =
  prim_isMenu arg1
  >>= \  gc_res2  ->
  (unmarshall_bool_ gc_res2) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_isMenu" prim_isMenu :: Addr -> IO (Int)

getSystemMenu :: HWND -> Bool -> IO MbHMENU
getSystemMenu arg1 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg2) ->
  prim_getSystemMenu arg1 arg2
  >>= \  res1  ->
  (if nullHANDLE == (res1)
   then return Nothing
   else (return ((Just res1)))) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_getSystemMenu" prim_getSystemMenu :: Addr -> Int -> IO (Addr)

getMenu :: HWND -> IO MbHMENU
getMenu arg1 =
  prim_getMenu arg1
  >>= \  res1  ->
  (if nullHANDLE == (res1)
   then return Nothing
   else (return ((Just res1)))) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_getMenu" prim_getMenu :: Addr -> IO (Addr)

getMenuDefaultItem :: HMENU -> Bool -> GMDIFlag -> IO MenuItem
getMenuDefaultItem arg1 gc_arg1 arg3 =
  (marshall_bool_ gc_arg1) >>= \ (arg2) ->
  prim_getMenuDefaultItem arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_getMenuDefaultItem_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getMenuDefaultItem_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getMenuDefaultItem_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_getMenuDefaultItem" prim_getMenuDefaultItem :: Addr -> Int -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuDefaultItem_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuDefaultItem_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuDefaultItem_gc_failstring :: Addr -> IO (Addr)

getMenuState :: HMENU -> MenuItem -> MenuFlag -> IO MenuState
getMenuState arg1 arg2 arg3 =
  prim_getMenuState arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_getMenuState_res (gc_result :: Addr) >>= \ res ->
  access_prim_getMenuState_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getMenuState_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_getMenuState" prim_getMenuState :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuState_res :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuState_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuState_gc_failstring :: Addr -> IO (Addr)


getSubMenu :: HMENU -> MenuItem -> IO MbHMENU
getSubMenu arg1 arg2 =
  prim_getSubMenu arg1 arg2
  >>= \  res1  ->
  (if nullHANDLE == (res1)
   then return Nothing
   else (return ((Just res1)))) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_getSubMenu" prim_getSubMenu :: Addr -> Word32 -> IO (Addr)

setMenu :: HWND -> HMENU -> IO ()
setMenu arg1 arg2 =
  prim_setMenu arg1 arg2
  >>= \ gc_result ->
  access_prim_setMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_setMenu" prim_setMenu :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_setMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_setMenu_gc_failstring :: Addr -> IO (Addr)

getMenuItemCount :: HMENU -> IO Int
getMenuItemCount arg1 =
  prim_getMenuItemCount arg1
  >>= \ gc_result ->
  access_prim_getMenuItemCount_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getMenuItemCount_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getMenuItemCount_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_getMenuItemCount" prim_getMenuItemCount :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemCount_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemCount_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemCount_gc_failstring :: Addr -> IO (Addr)

type MenuID = UINT

getMenuItemID :: HMENU -> MenuItem -> IO MenuID
getMenuItemID arg1 arg2 =
  prim_getMenuItemID arg1 arg2
  >>= \ gc_result ->
  access_prim_getMenuItemID_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getMenuItemID_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getMenuItemID_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_getMenuItemID" prim_getMenuItemID :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemID_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemID_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemID_gc_failstring :: Addr -> IO (Addr)

data MenuItemInfo
 = MenuItemInfo  {
      menuItemType    :: MenuFlag,
      menuItemState   :: MenuState,
      menuItemID      :: UINT,
      menuItemSubMenu :: HMENU,
      menuItemBitmapChecked :: HBITMAP,
      menuItemBitmapUnchecked :: HBITMAP,
      menuItemData    :: DWORD,
      menuItemTypeData :: String
   }



marshall_menuItemInfo :: MenuItemInfo -> IO Addr
marshall_menuItemInfo (MenuItemInfo miType 
				    miState
				    miItemID
				    miSubMenu
				    miChecked
				    miUnchecked
				    miData
				    miTypeData) = do
  ptr <- mallocMenuItemInfo
  pstr <- marshall_string_ miTypeData
  assignMenuItemInfo ptr
                     miType miState miItemID miSubMenu 
                     miChecked miUnchecked miData pstr (length miTypeData)
  return ptr

unmarshall_menuItemInfo :: Addr -> IO MenuItemInfo
unmarshall_menuItemInfo ptr = do
   mi <- unravelItemInfo ptr
   free ptr
   return mi

unravelItemInfo :: Addr -> IO MenuItemInfo
unravelItemInfo arg1 =
  prim_unravelItemInfo arg1
  >>= \ gc_result ->
  access_prim_unravelItemInfo_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_unravelItemInfo_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_unravelItemInfo_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_unravelItemInfo_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_unravelItemInfo_gc_res5 (gc_result :: Addr) >>= \ gc_res5 ->
  access_prim_unravelItemInfo_gc_res6 (gc_result :: Addr) >>= \ gc_res6 ->
  access_prim_unravelItemInfo_gc_res7 (gc_result :: Addr) >>= \ gc_res7 ->
  access_prim_unravelItemInfo_gc_res9 (gc_result :: Addr) >>= \ gc_res9 ->
  access_prim_unravelItemInfo_gc_res10 (gc_result :: Addr) >>= \ gc_res10 ->
  (unmarshall_stringLen_ gc_res9 gc_res10) >>= \ gc_res8 ->
  (return ((MenuItemInfo gc_res1 gc_res2 gc_res3 gc_res4 gc_res5 gc_res6 gc_res7 gc_res8)))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_unravelItemInfo" prim_unravelItemInfo :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_unravelItemInfo_gc_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_unravelItemInfo_gc_res2 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_unravelItemInfo_gc_res3 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_unravelItemInfo_gc_res4 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_unravelItemInfo_gc_res5 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_unravelItemInfo_gc_res6 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_unravelItemInfo_gc_res7 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_unravelItemInfo_gc_res9 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_unravelItemInfo_gc_res10 :: Addr -> IO (Int)

mallocMenuItemInfo :: IO Addr
mallocMenuItemInfo =
  prim_mallocMenuItemInfo
  >>= \ gc_result ->
  access_prim_mallocMenuItemInfo_mp (gc_result :: Addr) >>= \ mp ->
  access_prim_mallocMenuItemInfo_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_mallocMenuItemInfo_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (mp))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mallocMenuItemInfo" prim_mallocMenuItemInfo :: IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_mallocMenuItemInfo_mp :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_mallocMenuItemInfo_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_mallocMenuItemInfo_gc_failstring :: Addr -> IO (Addr)

assignMenuItemInfo :: Addr -> UINT -> UINT -> UINT -> HMENU -> HBITMAP -> HBITMAP -> DWORD -> Addr -> Int -> IO ()
assignMenuItemInfo arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 =
  prim_assignMenuItemInfo arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_assignMenuItemInfo" prim_assignMenuItemInfo :: Addr -> Word32 -> Word32 -> Word32 -> Addr -> Addr -> Addr -> Word32 -> Addr -> Int -> IO ()

type MenuItemMask = UINT

foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mIIM_CHECKMARKS" mIIM_CHECKMARKS :: MenuItemMask
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mIIM_DATA" mIIM_DATA :: MenuItemMask
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mIIM_ID" mIIM_ID :: MenuItemMask
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mIIM_STATE" mIIM_STATE :: MenuItemMask
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mIIM_SUBMENU" mIIM_SUBMENU :: MenuItemMask
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_mIIM_TYPE" mIIM_TYPE :: MenuItemMask

getMenuItemInfo :: HMENU -> MenuItem -> Bool -> MenuItemMask -> IO MenuItemInfo
getMenuItemInfo arg1 arg2 gc_arg1 arg4 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_getMenuItemInfo arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_getMenuItemInfo_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getMenuItemInfo_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getMenuItemInfo_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else ( unmarshall_menuItemInfo  (gc_res2)) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_getMenuItemInfo" prim_getMenuItemInfo :: Addr -> Word32 -> Int -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemInfo_gc_res2 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemInfo_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemInfo_gc_failstring :: Addr -> IO (Addr)

getMenuItemRect :: HWND -> HMENU -> UINT -> IO RECT
getMenuItemRect arg1 arg2 arg3 =
  prim_getMenuItemRect arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_getMenuItemRect_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getMenuItemRect_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getMenuItemRect_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_getMenuItemRect_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_getMenuItemRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getMenuItemRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2,gc_res3,gc_res4)))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_getMenuItemRect" prim_getMenuItemRect :: Addr -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemRect_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemRect_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemRect_gc_res3 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemRect_gc_res4 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_getMenuItemRect_gc_failstring :: Addr -> IO (Addr)

hiliteMenuItem :: HWND -> HMENU -> MenuItem -> MenuFlag -> IO Bool
hiliteMenuItem arg1 arg2 arg3 arg4 =
  prim_hiliteMenuItem arg1 arg2 arg3 arg4
  >>= \  gc_res2  ->
  (unmarshall_bool_ gc_res2) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_hiliteMenuItem" prim_hiliteMenuItem :: Addr -> Addr -> Word32 -> Word32 -> IO (Int)

insertMenuItem :: HMENU -> MenuItem -> Bool -> MenuItemInfo -> IO ()
insertMenuItem arg1 arg2 gc_arg1 gc_arg2 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  ( marshall_menuItemInfo  gc_arg2) >>= \ arg4 ->
  prim_insertMenuItem arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_insertMenuItem_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_insertMenuItem_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_insertMenuItem" prim_insertMenuItem :: Addr -> Word32 -> Int -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_insertMenuItem_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_insertMenuItem_gc_failstring :: Addr -> IO (Addr)

type Menu = LPCSTR
-- intToMenu :: Int -> Menu
-- intToMenu i = makeIntResource (toWord i)

loadMenu :: MbHINSTANCE -> Menu -> IO HMENU
loadMenu arg1 arg2 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_loadMenu arg1 arg2
  >>= \ gc_result ->
  access_prim_loadMenu_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_loadMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_loadMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_loadMenu" prim_loadMenu :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_loadMenu_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_loadMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_loadMenu_gc_failstring :: Addr -> IO (Addr)

-- Dealing with mappings to/from structs is a pain in GC,
-- so we'll leave this one out for now.
-- %fun LoadMenuIndirect :: MenuTemplate -> IO HMENU

menuItemFromPoint :: HWND -> HMENU -> POINT -> IO UINT
menuItemFromPoint arg1 arg2 gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  prim_menuItemFromPoint arg1 arg2 gc_arg2 gc_arg3
  >>= \  res1  ->
  (return (res1))}
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_menuItemFromPoint" prim_menuItemFromPoint :: Addr -> Addr -> Int32 -> Int32 -> IO (Word32)

setMenuDefaultItem :: HMENU -> MenuItem -> Bool -> IO ()
setMenuDefaultItem arg1 arg2 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_setMenuDefaultItem arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_setMenuDefaultItem_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setMenuDefaultItem_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_setMenuDefaultItem" prim_setMenuDefaultItem :: Addr -> Word32 -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_setMenuDefaultItem_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_setMenuDefaultItem_gc_failstring :: Addr -> IO (Addr)

setMenuItemBitmaps :: HMENU -> MenuItem -> MenuFlag -> HBITMAP -> HBITMAP -> IO ()
setMenuItemBitmaps arg1 arg2 arg3 arg4 arg5 =
  prim_setMenuItemBitmaps arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_setMenuItemBitmaps_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setMenuItemBitmaps_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_setMenuItemBitmaps" prim_setMenuItemBitmaps :: Addr -> Word32 -> Word32 -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_setMenuItemBitmaps_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_setMenuItemBitmaps_gc_failstring :: Addr -> IO (Addr)

destroyMenu :: HMENU -> IO ()
destroyMenu arg1 =
  prim_destroyMenu arg1
  >>= \ gc_result ->
  access_prim_destroyMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_destroyMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_destroyMenu" prim_destroyMenu :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_destroyMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_destroyMenu_gc_failstring :: Addr -> IO (Addr)

deleteMenu :: HMENU -> MenuItem -> MenuFlag -> IO ()
deleteMenu arg1 arg2 arg3 =
  prim_deleteMenu arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_deleteMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_deleteMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_deleteMenu" prim_deleteMenu :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_deleteMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_deleteMenu_gc_failstring :: Addr -> IO (Addr)

setMenuItemInfo :: HMENU -> MenuItem -> Bool -> MenuItemMask -> MenuItemInfo -> IO ()
setMenuItemInfo arg1 arg2 gc_arg1 arg4 gc_arg2 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  ( marshall_menuItemInfo  gc_arg2) >>= \ arg5 ->
  prim_setMenuItemInfo arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_setMenuItemInfo_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setMenuItemInfo_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_setMenuItemInfo" prim_setMenuItemInfo :: Addr -> Word32 -> Int -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_setMenuItemInfo_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_setMenuItemInfo_gc_failstring :: Addr -> IO (Addr)

trackPopupMenu :: HMENU -> TrackMenuFlag -> Int -> Int -> HWND -> RECT -> IO ()
trackPopupMenu arg1 arg2 arg3 arg4 arg5 gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3,gc_arg4,gc_arg5) ->
  prim_trackPopupMenu arg1 arg2 arg3 arg4 arg5 gc_arg2 gc_arg3 gc_arg4 gc_arg5
  >>= \ gc_result ->
  access_prim_trackPopupMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_trackPopupMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_trackPopupMenu" prim_trackPopupMenu :: Addr -> Word32 -> Int -> Int -> Addr -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_trackPopupMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_trackPopupMenu_gc_failstring :: Addr -> IO (Addr)

trackPopupMenuEx :: HMENU -> TrackMenuFlag -> Int -> Int -> HWND -> MbLPRECT -> IO ()
trackPopupMenuEx arg1 arg2 arg3 arg4 arg5 arg6 =
  (case arg6 of {
      Nothing -> (return (nullAddr));
      (Just arg6) -> (return ((arg6)))
   }) >>= \ (arg6) ->
  prim_trackPopupMenuEx arg1 arg2 arg3 arg4 arg5 arg6
  >>= \ gc_result ->
  access_prim_trackPopupMenuEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_trackPopupMenuEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_trackPopupMenuEx" prim_trackPopupMenuEx :: Addr -> Word32 -> Int -> Int -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_trackPopupMenuEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_trackPopupMenuEx_gc_failstring :: Addr -> IO (Addr)



appendMenu :: HMENU -> MenuFlag -> MenuID -> String -> IO ()
appendMenu arg1 arg2 arg3 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg4) ->
  prim_appendMenu arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_appendMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_appendMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_appendMenu" prim_appendMenu :: Addr -> Word32 -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_appendMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_appendMenu_gc_failstring :: Addr -> IO (Addr)

insertMenu :: HMENU -> MenuItem -> MenuFlag -> MenuID -> String -> IO ()
insertMenu arg1 arg2 arg3 arg4 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg5) ->
  prim_insertMenu arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_insertMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_insertMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_insertMenu" prim_insertMenu :: Addr -> Word32 -> Word32 -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_insertMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_insertMenu_gc_failstring :: Addr -> IO (Addr)

modifyMenu :: HMENU -> MenuItem -> MenuFlag -> MenuID -> String -> IO ()
modifyMenu arg1 arg2 arg3 arg4 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg5) ->
  prim_modifyMenu arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_modifyMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_modifyMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_modifyMenu" prim_modifyMenu :: Addr -> Word32 -> Word32 -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_modifyMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_modifyMenu_gc_failstring :: Addr -> IO (Addr)

removeMenu :: HMENU -> MenuItem -> MenuFlag -> IO ()
removeMenu arg1 arg2 arg3 =
  prim_removeMenu arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_removeMenu_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_removeMenu_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Menu_stub_ffi.h prim_removeMenu" prim_removeMenu :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_removeMenu_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Menu_stub_ffi.h" access_prim_removeMenu_gc_failstring :: Addr -> IO (Addr)


----------------------------------------------------------------
-- End
----------------------------------------------------------------
