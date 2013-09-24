module Win32Key where

import Win32Types
import GDITypes
import StdDIS


type VKey   = DWORD

foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_LBUTTON" vK_LBUTTON :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_RBUTTON" vK_RBUTTON :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_CANCEL"  vK_CANCEL :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_MBUTTON" vK_MBUTTON :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_BACK"    vK_BACK :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_TAB"     vK_TAB :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_CLEAR"   vK_CLEAR :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_RETURN"  vK_RETURN :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_SHIFT"   vK_SHIFT :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_CONTROL" vK_CONTROL :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_MENU"    vK_MENU :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_PAUSE"   vK_PAUSE :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_CAPITAL" vK_CAPITAL :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_ESCAPE"  vK_ESCAPE :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_SPACE"   vK_SPACE :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_PRIOR"   vK_PRIOR :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NEXT"    vK_NEXT :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_END"     vK_END :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_HOME"    vK_HOME :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_LEFT"    vK_LEFT :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_UP"      vK_UP :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_RIGHT"   vK_RIGHT :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_DOWN"    vK_DOWN :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_SELECT"  vK_SELECT :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_EXECUTE" vK_EXECUTE :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_SNAPSHOT" vK_SNAPSHOT :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_INSERT"   vK_INSERT :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_DELETE"   vK_DELETE :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_HELP"     vK_HELP :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD0"  vK_NUMPAD0 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD1"  vK_NUMPAD1 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD2"  vK_NUMPAD2 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD3"  vK_NUMPAD3 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD4"  vK_NUMPAD4 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD5"  vK_NUMPAD5 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD6"  vK_NUMPAD6 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD7"  vK_NUMPAD7 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD8"  vK_NUMPAD8 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMPAD9"  vK_NUMPAD9 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_MULTIPLY" vK_MULTIPLY :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_ADD"      vK_ADD :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_SEPARATOR" vK_SEPARATOR :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_SUBTRACT" vK_SUBTRACT :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_DECIMAL" vK_DECIMAL :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_DIVIDE" vK_DIVIDE :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F1" vK_F1 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F2" vK_F2 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F3" vK_F3 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F4" vK_F4 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F5" vK_F5 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F6" vK_F6 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F7" vK_F7 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F8" vK_F8 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F9" vK_F9 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F10" vK_F10 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F11" vK_F11 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F12" vK_F12 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F13" vK_F13 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F14" vK_F14 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F15" vK_F15 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F16" vK_F16 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F17" vK_F17 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F18" vK_F18 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F19" vK_F19 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F20" vK_F20 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F21" vK_F21 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F22" vK_F22 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F23" vK_F23 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_F24" vK_F24 :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_NUMLOCK" vK_NUMLOCK :: VKey
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_vK_SCROLL"  vK_SCROLL :: VKey

enableWindow :: HWND -> Bool -> IO Bool
enableWindow arg1 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg2) ->
  prim_enableWindow arg1 arg2
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_enableWindow" prim_enableWindow :: Addr -> Int -> IO (Int)

getActiveWindow :: IO MbHWND
getActiveWindow =
  prim_getActiveWindow
  >>= \  res1  ->
  (if nullHANDLE == (res1)
   then return Nothing
   else (return ((Just res1)))) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_getActiveWindow" prim_getActiveWindow :: IO (Addr)

getAsyncKeyState :: Int -> IO WORD
getAsyncKeyState arg1 =
  prim_getAsyncKeyState arg1
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_getAsyncKeyState" prim_getAsyncKeyState :: Int -> IO (Word16)

getFocus :: IO MbHWND
getFocus =
  prim_getFocus
  >>= \  res1  ->
  (if nullHANDLE == (res1)
   then return Nothing
   else (return ((Just res1)))) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_getFocus" prim_getFocus :: IO (Addr)

getKBCodePage :: IO UINT
getKBCodePage =
  prim_getKBCodePage
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_getKBCodePage" prim_getKBCodePage :: IO (Word32)

isWindowEnabled :: HWND -> IO Bool
isWindowEnabled arg1 =
  prim_isWindowEnabled arg1
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_isWindowEnabled" prim_isWindowEnabled :: Addr -> IO (Int)

foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_lOWORD" lOWORD :: DWORD -> WORD
foreign import  ccall unsafe "Win32Key_stub_ffi.h prim_hIWORD" hIWORD :: DWORD -> WORD

