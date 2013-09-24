module Win32WinMessage where

import StdDIS
import Win32Types


type WindowMessage   = DWORD

foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_COMPACTING" wM_COMPACTING :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_WININICHANGE" wM_WININICHANGE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SYSCOLORCHANGE" wM_SYSCOLORCHANGE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_QUERYNEWPALETTE" wM_QUERYNEWPALETTE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_PALETTEISCHANGING" wM_PALETTEISCHANGING :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_PALETTECHANGED" wM_PALETTECHANGED :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_FONTCHANGE" wM_FONTCHANGE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SPOOLERSTATUS" wM_SPOOLERSTATUS :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_DEVMODECHANGE" wM_DEVMODECHANGE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_TIMECHANGE" wM_TIMECHANGE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_POWER" wM_POWER :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_QUERYENDSESSION" wM_QUERYENDSESSION :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_ENDSESSION" wM_ENDSESSION :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_QUIT" wM_QUIT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_CREATE" wM_CREATE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCCREATE" wM_NCCREATE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_DESTROY" wM_DESTROY :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCDESTROY" wM_NCDESTROY :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SHOWWINDOW" wM_SHOWWINDOW :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SETREDRAW" wM_SETREDRAW :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_ENABLE" wM_ENABLE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SETTEXT" wM_SETTEXT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_GETTEXT" wM_GETTEXT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_GETTEXTLENGTH" wM_GETTEXTLENGTH :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_WINDOWPOSCHANGING" wM_WINDOWPOSCHANGING :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_WINDOWPOSCHANGED" wM_WINDOWPOSCHANGED :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MOVE" wM_MOVE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SIZE" wM_SIZE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_QUERYOPEN" wM_QUERYOPEN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_CLOSE" wM_CLOSE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_GETMINMAXINFO" wM_GETMINMAXINFO :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_PAINT" wM_PAINT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_ERASEBKGND" wM_ERASEBKGND :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_ICONERASEBKGND" wM_ICONERASEBKGND :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCPAINT" wM_NCPAINT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCCALCSIZE" wM_NCCALCSIZE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_QUERYDRAGICON" wM_QUERYDRAGICON :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_DROPFILES" wM_DROPFILES :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_ACTIVATE" wM_ACTIVATE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_ACTIVATEAPP" wM_ACTIVATEAPP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCACTIVATE" wM_NCACTIVATE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SETFOCUS" wM_SETFOCUS :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_KILLFOCUS" wM_KILLFOCUS :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_KEYDOWN" wM_KEYDOWN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_KEYUP" wM_KEYUP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_CHAR" wM_CHAR :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_DEADCHAR" wM_DEADCHAR :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SYSKEYDOWN" wM_SYSKEYDOWN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SYSKEYUP" wM_SYSKEYUP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SYSCHAR" wM_SYSCHAR :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SYSDEADCHAR" wM_SYSDEADCHAR :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_KEYFIRST" wM_KEYFIRST :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_KEYLAST" wM_KEYLAST :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MOUSEMOVE" wM_MOUSEMOVE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_LBUTTONDOWN" wM_LBUTTONDOWN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_LBUTTONUP" wM_LBUTTONUP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_LBUTTONDBLCLK" wM_LBUTTONDBLCLK :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_RBUTTONDOWN" wM_RBUTTONDOWN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_RBUTTONUP" wM_RBUTTONUP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_RBUTTONDBLCLK" wM_RBUTTONDBLCLK :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MBUTTONDOWN" wM_MBUTTONDOWN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MBUTTONUP" wM_MBUTTONUP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MBUTTONDBLCLK" wM_MBUTTONDBLCLK :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MOUSEFIRST" wM_MOUSEFIRST :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MOUSELAST" wM_MOUSELAST :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCMOUSEMOVE" wM_NCMOUSEMOVE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCLBUTTONDOWN" wM_NCLBUTTONDOWN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCLBUTTONUP" wM_NCLBUTTONUP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCLBUTTONDBLCLK" wM_NCLBUTTONDBLCLK :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCRBUTTONDOWN" wM_NCRBUTTONDOWN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCRBUTTONUP" wM_NCRBUTTONUP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCRBUTTONDBLCLK" wM_NCRBUTTONDBLCLK :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCMBUTTONDOWN" wM_NCMBUTTONDOWN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCMBUTTONUP" wM_NCMBUTTONUP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NCMBUTTONDBLCLK" wM_NCMBUTTONDBLCLK :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MOUSEACTIVATE" wM_MOUSEACTIVATE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_CANCELMODE" wM_CANCELMODE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_TIMER" wM_TIMER :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_INITMENU" wM_INITMENU :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_INITMENUPOPUP" wM_INITMENUPOPUP :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MENUSELECT" wM_MENUSELECT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MENUCHAR" wM_MENUCHAR :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_COMMAND" wM_COMMAND :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_HSCROLL" wM_HSCROLL :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_VSCROLL" wM_VSCROLL :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_CUT" wM_CUT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_COPY" wM_COPY :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_PASTE" wM_PASTE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_CLEAR" wM_CLEAR :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_UNDO" wM_UNDO :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_RENDERFORMAT" wM_RENDERFORMAT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_RENDERALLFORMATS" wM_RENDERALLFORMATS :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_DESTROYCLIPBOARD" wM_DESTROYCLIPBOARD :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_DRAWCLIPBOARD" wM_DRAWCLIPBOARD :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_PAINTCLIPBOARD" wM_PAINTCLIPBOARD :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SIZECLIPBOARD" wM_SIZECLIPBOARD :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_VSCROLLCLIPBOARD" wM_VSCROLLCLIPBOARD :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_HSCROLLCLIPBOARD" wM_HSCROLLCLIPBOARD :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_ASKCBFORMATNAME" wM_ASKCBFORMATNAME :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_CHANGECBCHAIN" wM_CHANGECBCHAIN :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SETCURSOR" wM_SETCURSOR :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SYSCOMMAND" wM_SYSCOMMAND :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDICREATE" wM_MDICREATE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDIDESTROY" wM_MDIDESTROY :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDIACTIVATE" wM_MDIACTIVATE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDIRESTORE" wM_MDIRESTORE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDINEXT" wM_MDINEXT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDIMAXIMIZE" wM_MDIMAXIMIZE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDITILE" wM_MDITILE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDICASCADE" wM_MDICASCADE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDIICONARRANGE" wM_MDIICONARRANGE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDIGETACTIVE" wM_MDIGETACTIVE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MDISETMENU" wM_MDISETMENU :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_CHILDACTIVATE" wM_CHILDACTIVATE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_INITDIALOG" wM_INITDIALOG :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_NEXTDLGCTL" wM_NEXTDLGCTL :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_PARENTNOTIFY" wM_PARENTNOTIFY :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_ENTERIDLE" wM_ENTERIDLE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_GETDLGCODE" wM_GETDLGCODE :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_SETFONT" wM_SETFONT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_GETFONT" wM_GETFONT :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_DRAWITEM" wM_DRAWITEM :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_MEASUREITEM" wM_MEASUREITEM :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_DELETEITEM" wM_DELETEITEM :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_COMPAREITEM" wM_COMPAREITEM :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_VKEYTOITEM" wM_VKEYTOITEM :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_CHARTOITEM" wM_CHARTOITEM :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_QUEUESYNC" wM_QUEUESYNC :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_USER" wM_USER :: WindowMessage
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_wM_APP" wM_APP :: WindowMessage


registerWindowMessage :: String -> IO WindowMessage
registerWindowMessage gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_registerWindowMessage arg1
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_registerWindowMessage" prim_registerWindowMessage :: Addr -> IO (Word32)

-- These are WM_SIZE specific
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_sIZE_RESTORED" sIZE_RESTORED :: WPARAM
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_sIZE_MINIMIZED" sIZE_MINIMIZED :: WPARAM
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_sIZE_MAXIMIZED" sIZE_MAXIMIZED :: WPARAM
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_sIZE_MAXSHOW" sIZE_MAXSHOW :: WPARAM
foreign import  ccall unsafe "Win32WinMessage_stub_ffi.h prim_sIZE_MAXHIDE" sIZE_MAXHIDE :: WPARAM


----------------------------------------------------------------
-- Phew!
----------------------------------------------------------------
