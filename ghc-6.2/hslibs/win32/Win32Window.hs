module Win32Window where

import StdDIS
import Win32Types
import GDITypes
import Win32WinMessage
import Addr
import Foreign.ForeignPtr
import Foreign.Ptr ( castFunPtrToPtr )

----------------------------------------------------------------
-- Window Class
----------------------------------------------------------------

-- The classname must not be deallocated until the corresponding class
-- is deallocated.  For this reason, we represent classnames by pointers
-- and explicitly allocate the className.

type ClassName   = LPCTSTR

-- Note: this is one of those rare functions which doesnt free all 
-- its String arguments.
mkClassName :: String -> ClassName
mkClassName gc_arg1 =
  unsafePerformIO(
    (marshall_string_ gc_arg1) >>= \ (arg1) ->
    prim_mkClassName arg1
    >>= \  gc_res1  ->
    (return (gc_res1)))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_mkClassName" prim_mkClassName :: Addr -> IO (Addr)

type ClassStyle   = UINT

foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_VREDRAW" cS_VREDRAW :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_HREDRAW" cS_HREDRAW :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_OWNDC"   cS_OWNDC   :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_CLASSDC" cS_CLASSDC :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_PARENTDC" cS_PARENTDC :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_SAVEBITS" cS_SAVEBITS :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_DBLCLKS"  cS_DBLCLKS :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_BYTEALIGNCLIENT" cS_BYTEALIGNCLIENT :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_BYTEALIGNWINDOW" cS_BYTEALIGNWINDOW :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_NOCLOSE" cS_NOCLOSE :: ClassStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cS_GLOBALCLASS" cS_GLOBALCLASS :: ClassStyle

type WNDCLASS =
 (ClassStyle,  -- style
  HINSTANCE,   -- hInstance
  MbHICON,     -- hIcon
  MbHCURSOR,   -- hCursor
  MbHBRUSH,    -- hbrBackground
  MbLPCSTR,    -- lpszMenuName
  ClassName)   -- lpszClassName

--ToDo!
--To avoid confusion with NULL, WNDCLASS requires you to add 1 to a SystemColor
--(which can be NULL)
-- %fun mkMbHBRUSH :: SystemColor -> MbHBRUSH
-- %code
-- %result ((HBRUSH)($0+1));

marshall_wndClass_ :: WNDCLASS -> IO Addr
marshall_wndClass_ gc_arg1 =
  case gc_arg1 of { (style,hInstance,hIcon,hCursor,hbrBackground,lpszMenuName,lpszClassName) ->
  (case hIcon of {
      Nothing -> (return (nullHANDLE));
      (Just hIcon) -> (return ((hIcon)))
   }) >>= \ (hIcon) ->
  (case hCursor of {
      Nothing -> (return (nullHANDLE));
      (Just hCursor) -> (return ((hCursor)))
   }) >>= \ (hCursor) ->
  (case hbrBackground of {
      Nothing -> (return (nullHANDLE));
      (Just hbrBackground) -> (return ((hbrBackground)))
   }) >>= \ (hbrBackground) ->
  (case lpszMenuName of {
      Nothing -> (return (nullAddr));
      (Just lpszMenuName) -> (return ((lpszMenuName)))
   }) >>= \ (lpszMenuName) ->
  prim_marshall_wndClass_ style hInstance hIcon hCursor hbrBackground lpszMenuName lpszClassName
  >>= \ gc_result ->
  access_prim_marshall_wndClass__c (gc_result :: Addr) >>= \ c ->
  access_prim_marshall_wndClass__gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_marshall_wndClass__gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (c))}
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_marshall_wndClass_" prim_marshall_wndClass_ :: Word32 -> Addr -> Addr -> Addr -> Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_marshall_wndClass__c :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_marshall_wndClass__gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_marshall_wndClass__gc_failstring :: Addr -> IO (Addr)


registerClass :: WNDCLASS -> IO MbATOM
registerClass gc_arg1 =
  (marshall_wndClass_ gc_arg1) >>= \ (arg1) ->
  prim_registerClass arg1
  >>= \  res1  ->
  (if 0 == (res1)
   then return Nothing
   else (return ((Just res1)))) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_registerClass" prim_registerClass :: Addr -> IO (Word32)

unregisterClass :: ClassName -> HINSTANCE -> IO ()
unregisterClass arg1 arg2 =
  prim_unregisterClass arg1 arg2
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_unregisterClass" prim_unregisterClass :: Addr -> Addr -> IO ()

----------------------------------------------------------------
-- Window Style
----------------------------------------------------------------

type WindowStyle   = DWORD

foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_OVERLAPPED" wS_OVERLAPPED :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_POPUP" wS_POPUP :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_CHILD" wS_CHILD :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_CLIPSIBLINGS" wS_CLIPSIBLINGS :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_CLIPCHILDREN" wS_CLIPCHILDREN :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_VISIBLE" wS_VISIBLE :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_DISABLED" wS_DISABLED :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_MINIMIZE" wS_MINIMIZE :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_MAXIMIZE" wS_MAXIMIZE :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_CAPTION"  wS_CAPTION :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_BORDER"   wS_BORDER :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_DLGFRAME" wS_DLGFRAME :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_VSCROLL"  wS_VSCROLL :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_HSCROLL"  wS_HSCROLL :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_SYSMENU"  wS_SYSMENU :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_THICKFRAME" wS_THICKFRAME :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_MINIMIZEBOX" wS_MINIMIZEBOX :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_MAXIMIZEBOX" wS_MAXIMIZEBOX :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_GROUP" wS_GROUP :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_TABSTOP" wS_TABSTOP :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_OVERLAPPEDWINDOW" wS_OVERLAPPEDWINDOW :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_POPUPWINDOW" wS_POPUPWINDOW :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_CHILDWINDOW" wS_CHILDWINDOW :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_TILED" wS_TILED :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_ICONIC" wS_ICONIC :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_SIZEBOX" wS_SIZEBOX :: WindowStyle
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_TILEDWINDOW" wS_TILEDWINDOW :: WindowStyle

type WindowStyleEx   = DWORD

foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_DLGMODALFRAME" wS_EX_DLGMODALFRAME :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_NOPARENTNOTIFY" wS_EX_NOPARENTNOTIFY :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_TOPMOST" wS_EX_TOPMOST :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_ACCEPTFILES" wS_EX_ACCEPTFILES :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_TRANSPARENT" wS_EX_TRANSPARENT :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_MDICHILD" wS_EX_MDICHILD :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_TOOLWINDOW" wS_EX_TOOLWINDOW :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_WINDOWEDGE" wS_EX_WINDOWEDGE :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_CLIENTEDGE" wS_EX_CLIENTEDGE :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_CONTEXTHELP" wS_EX_CONTEXTHELP :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_RIGHT" wS_EX_RIGHT :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_LEFT"  wS_EX_LEFT :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_RTLREADING" wS_EX_RTLREADING :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_LTRREADING" wS_EX_LTRREADING :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_LEFTSCROLLBAR" wS_EX_LEFTSCROLLBAR :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_RIGHTSCROLLBAR" wS_EX_RIGHTSCROLLBAR :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_CONTROLPARENT" wS_EX_CONTROLPARENT :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_STATICEDGE" wS_EX_STATICEDGE :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_APPWINDOW" wS_EX_APPWINDOW :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_OVERLAPPEDWINDOW" wS_EX_OVERLAPPEDWINDOW :: WindowStyleEx
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_wS_EX_PALETTEWINDOW" wS_EX_PALETTEWINDOW :: WindowStyleEx

type Pos = Int
type MbPos = Maybe Pos
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_cW_USEDEFAULT" cW_USEDEFAULT :: Pos

type WindowClosure = HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT

type WindowClosure_FFI = Ptr () -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT

 
marshall_windowClosure_ :: WindowClosure -> IO (FunPtr WindowClosure)
marshall_windowClosure_ cl = mkWindowClosure (\ x -> cl (ptrToAddr x))

foreign import ccall "wrapper" mkWindowClosure :: WindowClosure_FFI -> IO (FunPtr WindowClosure)


setWindowClosure :: HWND -> WindowClosure -> IO ()
setWindowClosure hwnd gc_arg1 =
  (marshall_windowClosure_ gc_arg1) >>= \ (closure) ->
  prim_setWindowClosure hwnd closure
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_setWindowClosure" prim_setWindowClosure :: Addr -> FunPtr WindowClosure -> IO ()

createWindow :: ClassName -> String -> WindowStyle -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos -> MbHWND -> MbHMENU -> HINSTANCE -> WindowClosure -> IO HWND
createWindow name gc_arg1 style x y width height hwndParent hmenu hinst gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (windowName) ->
  (case x of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just x) -> (return ((x)))
   }) >>= \ (x) ->
  (case y of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just y) -> (return ((y)))
   }) >>= \ (y) ->
  (case width of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just width) -> (return ((width)))
   }) >>= \ (width) ->
  (case height of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just height) -> (return ((height)))
   }) >>= \ (height) ->
  (case hwndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hwndParent) -> (return ((hwndParent)))
   }) >>= \ (hwndParent) ->
  (case hmenu of {
      Nothing -> (return (nullHANDLE));
      (Just hmenu) -> (return ((hmenu)))
   }) >>= \ (hmenu) ->
  (marshall_windowClosure_ gc_arg2) >>= \ (closure) ->
  prim_createWindow name windowName style x y width height hwndParent hmenu hinst closure
  >>= \ gc_result ->
  access_prim_createWindow_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createWindow_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createWindow_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_createWindow" prim_createWindow :: Addr -> Addr -> Word32 -> Int -> Int -> Int -> Int -> Addr -> Addr -> Addr -> FunPtr WindowClosure -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_createWindow_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_createWindow_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_createWindow_gc_failstring :: Addr -> IO (Addr)

-- Freeing the title/window name has been reported
-- to cause a crash, so let's not do it.
-- %end free(windowName)  /* I think this is safe... */

createWindowEx :: WindowStyle -> ClassName -> String -> WindowStyle -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos -> MbHWND -> MbHMENU -> HINSTANCE -> WindowClosure -> IO HWND
createWindowEx estyle cls gc_arg1 wstyle x y nWidth nHeight hwndParent hmenu hinstance gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (wname) ->
  (case x of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just x) -> (return ((x)))
   }) >>= \ (x) ->
  (case y of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just y) -> (return ((y)))
   }) >>= \ (y) ->
  (case nWidth of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just nWidth) -> (return ((nWidth)))
   }) >>= \ (nWidth) ->
  (case nHeight of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just nHeight) -> (return ((nHeight)))
   }) >>= \ (nHeight) ->
  (case hwndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hwndParent) -> (return ((hwndParent)))
   }) >>= \ (hwndParent) ->
  (case hmenu of {
      Nothing -> (return (nullHANDLE));
      (Just hmenu) -> (return ((hmenu)))
   }) >>= \ (hmenu) ->
  (marshall_windowClosure_ gc_arg2) >>= \ (closure) ->
  prim_createWindowEx estyle cls wname wstyle x y nWidth nHeight hwndParent hmenu hinstance closure
  >>= \ gc_result ->
  access_prim_createWindowEx_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createWindowEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createWindowEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_createWindowEx" prim_createWindowEx :: Word32 -> Addr -> Addr -> Word32 -> Int -> Int -> Int -> Int -> Addr -> Addr -> Addr -> FunPtr WindowClosure -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_createWindowEx_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_createWindowEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_createWindowEx_gc_failstring :: Addr -> IO (Addr)

-- see CreateWindow comment.
-- %end free(wname)  /* I think this is safe... */

----------------------------------------------------------------

defWindowProc :: MbHWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
defWindowProc arg1 arg2 arg3 arg4 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_defWindowProc arg1 arg2 arg3 arg4
  >>= \  res1  ->
  (return (res1))
foreign import  ccall "Win32Window_stub_ffi.h prim_defWindowProc" prim_defWindowProc :: Addr -> Word32 -> Word32 -> Int32 -> IO (Int32)

----------------------------------------------------------------

getClientRect :: HWND -> IO RECT
getClientRect arg1 =
  prim_getClientRect arg1
  >>= \ gc_result ->
  access_prim_getClientRect_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getClientRect_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getClientRect_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_getClientRect_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_getClientRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getClientRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2,gc_res3,gc_res4)))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getClientRect" prim_getClientRect :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getClientRect_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getClientRect_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getClientRect_gc_res3 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getClientRect_gc_res4 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getClientRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getClientRect_gc_failstring :: Addr -> IO (Addr)

getWindowRect :: HWND -> IO RECT
getWindowRect arg1 =
  prim_getWindowRect arg1
  >>= \ gc_result ->
  access_prim_getWindowRect_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getWindowRect_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getWindowRect_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_getWindowRect_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_getWindowRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getWindowRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2,gc_res3,gc_res4)))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getWindowRect" prim_getWindowRect :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getWindowRect_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getWindowRect_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getWindowRect_gc_res3 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getWindowRect_gc_res4 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getWindowRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getWindowRect_gc_failstring :: Addr -> IO (Addr)

-- Should it be MbRECT instead?
invalidateRect :: MbHWND -> MbLPRECT -> Bool -> IO ()
invalidateRect arg1 arg2 gc_arg1 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  (case arg2 of {
      Nothing -> (return (nullAddr));
      (Just arg2) -> (return ((arg2)))
   }) >>= \ (arg2) ->
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_invalidateRect arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_invalidateRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_invalidateRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall "Win32Window_stub_ffi.h prim_invalidateRect" prim_invalidateRect :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_invalidateRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_invalidateRect_gc_failstring :: Addr -> IO (Addr)

screenToClient :: HWND -> POINT -> IO POINT
screenToClient arg1 gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  prim_screenToClient arg1 gc_arg2 gc_arg3
  >>= \ gc_result ->
  access_prim_screenToClient_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_screenToClient_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_screenToClient_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_screenToClient_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))}
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_screenToClient" prim_screenToClient :: Addr -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_screenToClient_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_screenToClient_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_screenToClient_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_screenToClient_gc_failstring :: Addr -> IO (Addr)

clientToScreen :: HWND -> POINT -> IO POINT
clientToScreen hwnd gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  prim_clientToScreen hwnd gc_arg2 gc_arg3
  >>= \ gc_result ->
  access_prim_clientToScreen_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_clientToScreen_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_clientToScreen_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_clientToScreen_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))}
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_clientToScreen" prim_clientToScreen :: Addr -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_clientToScreen_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_clientToScreen_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_clientToScreen_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_clientToScreen_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Setting window text/label
----------------------------------------------------------------
-- For setting the title bar text.  But inconvenient to make the LPCTSTR
setWindowText :: HWND -> String -> IO ()
setWindowText arg1 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  prim_setWindowText arg1 arg2
  >>= \ gc_result ->
  access_prim_setWindowText_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setWindowText_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall "Win32Window_stub_ffi.h prim_setWindowText" prim_setWindowText :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_setWindowText_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_setWindowText_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Paint struct
----------------------------------------------------------------

type PAINTSTRUCT =
 ( HDC   -- hdc
 , Bool  -- fErase
 , RECT  -- rcPaint
 )

type LPPAINTSTRUCT   = Addr

foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sizeofPAINTSTRUCT" sizeofPAINTSTRUCT :: DWORD

beginPaint :: HWND -> LPPAINTSTRUCT -> IO HDC
beginPaint arg1 arg2 =
  prim_beginPaint arg1 arg2
  >>= \ gc_result ->
  access_prim_beginPaint_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_beginPaint_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_beginPaint_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Window_stub_ffi.h prim_beginPaint" prim_beginPaint :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_beginPaint_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_beginPaint_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_beginPaint_gc_failstring :: Addr -> IO (Addr)

endPaint :: HWND -> LPPAINTSTRUCT -> IO ()
endPaint arg1 arg2 =
  prim_endPaint arg1 arg2
foreign import  ccall "Win32Window_stub_ffi.h prim_endPaint" prim_endPaint :: Addr -> Addr -> IO ()
-- Apparently always succeeds (return non-zero)

----------------------------------------------------------------
-- ShowWindow
----------------------------------------------------------------

type ShowWindowControl   = DWORD

foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_HIDE" sW_HIDE :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_SHOWNORMAL" sW_SHOWNORMAL :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_SHOWMINIMIZED" sW_SHOWMINIMIZED :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_SHOWMAXIMIZED" sW_SHOWMAXIMIZED :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_MAXIMIZE" sW_MAXIMIZE :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_SHOWNOACTIVATE" sW_SHOWNOACTIVATE :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_SHOW" sW_SHOW :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_MINIMIZE" sW_MINIMIZE :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_SHOWMINNOACTIVE" sW_SHOWMINNOACTIVE :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_SHOWNA" sW_SHOWNA :: ShowWindowControl
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sW_RESTORE" sW_RESTORE :: ShowWindowControl

showWindow :: HWND -> ShowWindowControl -> IO Bool
showWindow arg1 arg2 =
  prim_showWindow arg1 arg2
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall "Win32Window_stub_ffi.h prim_showWindow" prim_showWindow :: Addr -> Word32 -> IO (Int)

----------------------------------------------------------------
-- Misc
----------------------------------------------------------------

adjustWindowRect :: RECT -> WindowStyle -> Bool -> IO RECT
adjustWindowRect gc_arg1 arg2 gc_arg7 =
  case gc_arg1 of { (gc_arg2,gc_arg3,gc_arg4,gc_arg5) ->
  (marshall_bool_ gc_arg7) >>= \ (arg3) ->
  prim_adjustWindowRect gc_arg2 gc_arg3 gc_arg4 gc_arg5 arg2 arg3
  >>= \ gc_result ->
  access_prim_adjustWindowRect_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_adjustWindowRect_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_adjustWindowRect_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_adjustWindowRect_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_adjustWindowRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_adjustWindowRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2,gc_res3,gc_res4)))}
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_adjustWindowRect" prim_adjustWindowRect :: Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRect_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRect_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRect_gc_res3 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRect_gc_res4 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRect_gc_failstring :: Addr -> IO (Addr)

adjustWindowRectEx :: RECT -> WindowStyle -> Bool -> WindowStyleEx -> IO RECT
adjustWindowRectEx gc_arg1 arg2 gc_arg7 arg4 =
  case gc_arg1 of { (gc_arg2,gc_arg3,gc_arg4,gc_arg5) ->
  (marshall_bool_ gc_arg7) >>= \ (arg3) ->
  prim_adjustWindowRectEx gc_arg2 gc_arg3 gc_arg4 gc_arg5 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_adjustWindowRectEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_adjustWindowRectEx_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_adjustWindowRectEx_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_adjustWindowRectEx_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_adjustWindowRectEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_adjustWindowRectEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2,gc_res3,gc_res4)))}
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_adjustWindowRectEx" prim_adjustWindowRectEx :: Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Int -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRectEx_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRectEx_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRectEx_gc_res3 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRectEx_gc_res4 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRectEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_adjustWindowRectEx_gc_failstring :: Addr -> IO (Addr)

-- Win2K and later:
-- %fun AllowSetForegroundWindow :: DWORD -> IO ()

-- %
-- %dis animateWindowType x = dWORD x
-- type AnimateWindowType   = DWORD

-- %const AnimateWindowType
--        [ AW_SLIDE
--        , AW_ACTIVATE
--        , AW_BLEND
--        , AW_HIDE
--        , AW_CENTER
--        , AW_HOR_POSITIVE
--        , AW_HOR_NEGATIVE
--        , AW_VER_POSITIVE
--        , AW_VER_NEGATIVE
--        ]

-- Win98 or Win2K:
-- %fun AnimateWindow :: HWND -> DWORD -> AnimateWindowType -> IO ()
-- %code BOOL success = AnimateWindow(arg1,arg2,arg3)
-- %fail { !success } { ErrorWin("AnimateWindow") }

anyPopup :: IO Bool
anyPopup =
  prim_anyPopup
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_anyPopup" prim_anyPopup :: IO (Int)

arrangeIconicWindows :: HWND -> IO ()
arrangeIconicWindows arg1 =
  prim_arrangeIconicWindows arg1
  >>= \ gc_result ->
  access_prim_arrangeIconicWindows_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_arrangeIconicWindows_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_arrangeIconicWindows" prim_arrangeIconicWindows :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_arrangeIconicWindows_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_arrangeIconicWindows_gc_failstring :: Addr -> IO (Addr)

beginDeferWindowPos :: Int -> IO HDWP
beginDeferWindowPos arg1 =
  prim_beginDeferWindowPos arg1
  >>= \ gc_result ->
  access_prim_beginDeferWindowPos_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_beginDeferWindowPos_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_beginDeferWindowPos_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_beginDeferWindowPos" prim_beginDeferWindowPos :: Int -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_beginDeferWindowPos_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_beginDeferWindowPos_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_beginDeferWindowPos_gc_failstring :: Addr -> IO (Addr)

bringWindowToTop :: HWND -> IO ()
bringWindowToTop arg1 =
  prim_bringWindowToTop arg1
  >>= \ gc_result ->
  access_prim_bringWindowToTop_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_bringWindowToTop_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall "Win32Window_stub_ffi.h prim_bringWindowToTop" prim_bringWindowToTop :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_bringWindowToTop_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_bringWindowToTop_gc_failstring :: Addr -> IO (Addr)

childWindowFromPoint :: HWND -> POINT -> IO MbHWND
childWindowFromPoint hwnd gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  prim_childWindowFromPoint hwnd gc_arg2 gc_arg3
  >>= \ gc_result ->
  access_prim_childWindowFromPoint_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_childWindowFromPoint_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_childWindowFromPoint_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (if nullHANDLE == (res1)
	then return Nothing
	else (return ((Just res1)))) >>= \ gc_res1 ->
       (return (gc_res1))}
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_childWindowFromPoint" prim_childWindowFromPoint :: Addr -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_childWindowFromPoint_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_childWindowFromPoint_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_childWindowFromPoint_gc_failstring :: Addr -> IO (Addr)

childWindowFromPointEx :: HWND -> POINT -> DWORD -> IO MbHWND
childWindowFromPointEx hwnd gc_arg1 arg2 =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  prim_childWindowFromPointEx hwnd gc_arg2 gc_arg3 arg2
  >>= \ gc_result ->
  access_prim_childWindowFromPointEx_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_childWindowFromPointEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_childWindowFromPointEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (if nullHANDLE == (res1)
	then return Nothing
	else (return ((Just res1)))) >>= \ gc_res1 ->
       (return (gc_res1))}
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_childWindowFromPointEx" prim_childWindowFromPointEx :: Addr -> Int32 -> Int32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_childWindowFromPointEx_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_childWindowFromPointEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_childWindowFromPointEx_gc_failstring :: Addr -> IO (Addr)

closeWindow :: HWND -> IO ()
closeWindow arg1 =
  prim_closeWindow arg1
  >>= \ gc_result ->
  access_prim_closeWindow_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_closeWindow_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall "Win32Window_stub_ffi.h prim_closeWindow" prim_closeWindow :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_closeWindow_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_closeWindow_gc_failstring :: Addr -> IO (Addr)

deferWindowPos :: HDWP -> HWND -> HWND -> Int -> Int -> Int -> Int -> SetWindowPosFlags -> IO HDWP
deferWindowPos arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 =
  prim_deferWindowPos arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8
  >>= \ gc_result ->
  access_prim_deferWindowPos_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_deferWindowPos_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_deferWindowPos_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_deferWindowPos" prim_deferWindowPos :: Addr -> Addr -> Addr -> Int -> Int -> Int -> Int -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_deferWindowPos_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_deferWindowPos_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_deferWindowPos_gc_failstring :: Addr -> IO (Addr)

destroyWindow :: HWND -> IO ()
destroyWindow arg1 =
  prim_destroyWindow arg1
  >>= \ gc_result ->
  access_prim_destroyWindow_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_destroyWindow_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall "Win32Window_stub_ffi.h prim_destroyWindow" prim_destroyWindow :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_destroyWindow_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_destroyWindow_gc_failstring :: Addr -> IO (Addr)

endDeferWindowPos :: HDWP -> IO ()
endDeferWindowPos arg1 =
  prim_endDeferWindowPos arg1
  >>= \ gc_result ->
  access_prim_endDeferWindowPos_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_endDeferWindowPos_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_endDeferWindowPos" prim_endDeferWindowPos :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_endDeferWindowPos_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_endDeferWindowPos_gc_failstring :: Addr -> IO (Addr)

findWindow :: String -> String -> IO MbHWND
findWindow gc_arg1 gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (marshall_string_ gc_arg2) >>= \ (arg2) ->
  prim_findWindow arg1 arg2
  >>= \  res1  ->
  (if nullHANDLE == (res1)
   then return Nothing
   else (return ((Just res1)))) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_findWindow" prim_findWindow :: Addr -> Addr -> IO (Addr)

findWindowEx :: HWND -> HWND -> String -> String -> IO MbHWND
findWindowEx arg1 arg2 gc_arg1 gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg3) ->
  (marshall_string_ gc_arg2) >>= \ (arg4) ->
  prim_findWindowEx arg1 arg2 arg3 arg4
  >>= \  res1  ->
  (if nullHANDLE == (res1)
   then return Nothing
   else (return ((Just res1)))) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_findWindowEx" prim_findWindowEx :: Addr -> Addr -> Addr -> Addr -> IO (Addr)

flashWindow :: HWND -> Bool -> IO Bool
flashWindow arg1 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg2) ->
  prim_flashWindow arg1 arg2
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_flashWindow" prim_flashWindow :: Addr -> Int -> IO (Int)
-- No error code

moveWindow :: HWND -> Int -> Int -> Int -> Int -> Bool -> IO ()
moveWindow arg1 arg2 arg3 arg4 arg5 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg6) ->
  prim_moveWindow arg1 arg2 arg3 arg4 arg5 arg6
  >>= \ gc_result ->
  access_prim_moveWindow_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_moveWindow_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall "Win32Window_stub_ffi.h prim_moveWindow" prim_moveWindow :: Addr -> Int -> Int -> Int -> Int -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_moveWindow_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_moveWindow_gc_failstring :: Addr -> IO (Addr)

getDesktopWindow :: IO HWND
getDesktopWindow =
  prim_getDesktopWindow
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getDesktopWindow" prim_getDesktopWindow :: IO (Addr)

getForegroundWindow :: IO HWND
getForegroundWindow =
  prim_getForegroundWindow
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getForegroundWindow" prim_getForegroundWindow :: IO (Addr)

getParent :: HWND -> IO HWND
getParent arg1 =
  prim_getParent arg1
  >>= \ gc_result ->
  access_prim_getParent_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getParent_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getParent_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getParent" prim_getParent :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getParent_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getParent_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getParent_gc_failstring :: Addr -> IO (Addr)

getTopWindow :: HWND -> IO HWND
getTopWindow arg1 =
  prim_getTopWindow arg1
  >>= \ gc_result ->
  access_prim_getTopWindow_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getTopWindow_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getTopWindow_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getTopWindow" prim_getTopWindow :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getTopWindow_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getTopWindow_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getTopWindow_gc_failstring :: Addr -> IO (Addr)


type SetWindowPosFlags = UINT

foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_NOSIZE" sWP_NOSIZE :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_NOMOVE" sWP_NOMOVE :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_NOZORDER" sWP_NOZORDER :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_NOREDRAW" sWP_NOREDRAW :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_NOACTIVATE" sWP_NOACTIVATE :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_FRAMECHANGED" sWP_FRAMECHANGED :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_SHOWWINDOW" sWP_SHOWWINDOW :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_HIDEWINDOW" sWP_HIDEWINDOW :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_NOCOPYBITS" sWP_NOCOPYBITS :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_NOOWNERZORDER" sWP_NOOWNERZORDER :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_NOSENDCHANGING" sWP_NOSENDCHANGING :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_DRAWFRAME" sWP_DRAWFRAME :: SetWindowPosFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_sWP_NOREPOSITION" sWP_NOREPOSITION :: SetWindowPosFlags

----------------------------------------------------------------
-- HDCs
----------------------------------------------------------------

type GetDCExFlags   = DWORD

foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_dCX_WINDOW" dCX_WINDOW :: GetDCExFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_dCX_CACHE"  dCX_CACHE :: GetDCExFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_dCX_CLIPCHILDREN" dCX_CLIPCHILDREN :: GetDCExFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_dCX_CLIPSIBLINGS" dCX_CLIPSIBLINGS :: GetDCExFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_dCX_PARENTCLIP" dCX_PARENTCLIP :: GetDCExFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_dCX_EXCLUDERGN" dCX_EXCLUDERGN :: GetDCExFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_dCX_INTERSECTRGN" dCX_INTERSECTRGN :: GetDCExFlags
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_dCX_LOCKWINDOWUPDATE" dCX_LOCKWINDOWUPDATE :: GetDCExFlags

-- apparently mostly fails if you use invalid hwnds
getDCEx :: HWND -> HRGN -> GetDCExFlags -> IO HDC
getDCEx arg1 arg2 arg3 =
  prim_getDCEx arg1 (unsafeForeignPtrToPtr arg2) arg3
  >>= \ gc_result ->
  access_prim_getDCEx_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getDCEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getDCEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getDCEx" prim_getDCEx :: Addr -> Ptr () -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDCEx_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDCEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDCEx_gc_failstring :: Addr -> IO (Addr)

getDC :: MbHWND -> IO HDC
getDC arg1 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_getDC arg1
  >>= \ gc_result ->
  access_prim_getDC_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getDC_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getDC_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getDC" prim_getDC :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDC_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDC_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDC_gc_failstring :: Addr -> IO (Addr)

getWindowDC :: MbHWND -> IO HDC
getWindowDC arg1 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_getWindowDC arg1
  >>= \ gc_result ->
  access_prim_getWindowDC_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getWindowDC_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getWindowDC_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getWindowDC" prim_getWindowDC :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getWindowDC_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getWindowDC_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getWindowDC_gc_failstring :: Addr -> IO (Addr)

releaseDC :: MbHWND -> HDC -> IO ()
releaseDC arg1 arg2 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_releaseDC arg1 arg2
  >>= \ gc_result ->
  access_prim_releaseDC_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_releaseDC_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_releaseDC" prim_releaseDC :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_releaseDC_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_releaseDC_gc_failstring :: Addr -> IO (Addr)

getDCOrgEx :: HDC -> IO POINT
getDCOrgEx arg1 =
  prim_getDCOrgEx arg1
  >>= \ gc_result ->
  access_prim_getDCOrgEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getDCOrgEx_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getDCOrgEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getDCOrgEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getDCOrgEx" prim_getDCOrgEx :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDCOrgEx_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDCOrgEx_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDCOrgEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getDCOrgEx_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Caret
----------------------------------------------------------------

hideCaret :: HWND -> IO ()
hideCaret arg1 =
  prim_hideCaret arg1
  >>= \ gc_result ->
  access_prim_hideCaret_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_hideCaret_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_hideCaret" prim_hideCaret :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_hideCaret_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_hideCaret_gc_failstring :: Addr -> IO (Addr)

showCaret :: HWND -> IO ()
showCaret arg1 =
  prim_showCaret arg1
  >>= \ gc_result ->
  access_prim_showCaret_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_showCaret_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_showCaret" prim_showCaret :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_showCaret_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_showCaret_gc_failstring :: Addr -> IO (Addr)

-- ToDo: allow arg2 to be NULL or {(HBITMAP)1}
createCaret :: HWND -> HBITMAP -> MbINT -> MbINT -> IO ()
createCaret arg1 arg2 arg3 arg4 =
  (case arg3 of {
      Nothing -> (return (0));
      (Just arg3) -> (return ((arg3)))
   }) >>= \ (arg3) ->
  (case arg4 of {
      Nothing -> (return (0));
      (Just arg4) -> (return ((arg4)))
   }) >>= \ (arg4) ->
  prim_createCaret arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_createCaret_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createCaret_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_createCaret" prim_createCaret :: Addr -> Addr -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_createCaret_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_createCaret_gc_failstring :: Addr -> IO (Addr)

destroyCaret :: IO ()
destroyCaret =
  prim_destroyCaret
  >>= \ gc_result ->
  access_prim_destroyCaret_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_destroyCaret_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_destroyCaret" prim_destroyCaret :: IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_destroyCaret_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_destroyCaret_gc_failstring :: Addr -> IO (Addr)

getCaretPos :: IO POINT
getCaretPos =
  prim_getCaretPos
  >>= \ gc_result ->
  access_prim_getCaretPos_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getCaretPos_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getCaretPos_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getCaretPos_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_getCaretPos" prim_getCaretPos :: IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getCaretPos_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getCaretPos_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getCaretPos_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getCaretPos_gc_failstring :: Addr -> IO (Addr)

setCaretPos :: POINT -> IO ()
setCaretPos gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  prim_setCaretPos gc_arg2 gc_arg3
  >>= \ gc_result ->
  access_prim_setCaretPos_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setCaretPos_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Window_stub_ffi.h prim_setCaretPos" prim_setCaretPos :: Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_setCaretPos_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_setCaretPos_gc_failstring :: Addr -> IO (Addr)

-- The remarks on SetCaretBlinkTime are either highly risible or very sad -
-- depending on whether you plan to use this function.

----------------------------------------------------------------
-- MSGs and event loops
--
-- Note that the following functions have to be reentrant:
--
--   DispatchMessage
--   SendMessage
--   UpdateWindow   (I think)
--   RedrawWindow   (I think)
--
-- The following dont have to be reentrant (according to documentation)
--
--   GetMessage
--   PeekMessage
--   TranslateMessage
--
-- For Hugs (and possibly NHC too?) this is no big deal.
-- For GHC, you have to use casm_GC instead of casm.
-- (It might be simpler to just put all this code in another
-- file and build it with the appropriate command line option...)
----------------------------------------------------------------

-- type MSG = 
--   ( HWND   -- hwnd;	
--   , UINT   -- message;
--   , WPARAM -- wParam;
--   , LPARAM -- lParam;
--   , DWORD  -- time;
--   , POINT  -- pt;
--   )

type LPMSG   = Addr

-- A NULL window requests messages for any window belonging to this thread.
-- a "success" value of 0 indicates that WM_QUIT was received
getMessage :: MbHWND -> IO LPMSG
getMessage arg1 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_getMessage arg1
  >>= \ gc_result ->
  access_prim_getMessage_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getMessage_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getMessage_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (gc_res1))
foreign import  ccall "Win32Window_stub_ffi.h prim_getMessage" prim_getMessage :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getMessage_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getMessage_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getMessage_gc_failstring :: Addr -> IO (Addr)

getMessage2 :: MbHWND -> IO (LPMSG,Bool)
getMessage2 arg1 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_getMessage2 arg1
  >>= \ gc_result ->
  access_prim_getMessage2_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getMessage2_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_getMessage2_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getMessage2_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_bool_ gc_res3) >>= \ gc_res2 ->
       (return ((gc_res1,gc_res2)))
foreign import  ccall "Win32Window_stub_ffi.h prim_getMessage2" prim_getMessage2 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getMessage2_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getMessage2_gc_res3 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getMessage2_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_getMessage2_gc_failstring :: Addr -> IO (Addr)

-- A NULL window requests messages for any window belonging to this thread.
-- Arguably the code block shouldn't be a 'safe' one, but it shouldn't really
-- hurt.
peekMessage :: MbHWND -> UINT -> UINT -> UINT -> IO LPMSG
peekMessage arg1 arg2 arg3 arg4 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_peekMessage arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_peekMessage_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_peekMessage_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_peekMessage_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (gc_res1))
foreign import  ccall "Win32Window_stub_ffi.h prim_peekMessage" prim_peekMessage :: Addr -> Word32 -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_peekMessage_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_peekMessage_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_peekMessage_gc_failstring :: Addr -> IO (Addr)

-- Note: youre not supposed to call this if youre using accelerators
translateMessage :: LPMSG -> IO BOOL
translateMessage arg1 =
  prim_translateMessage arg1
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall "Win32Window_stub_ffi.h prim_translateMessage" prim_translateMessage :: Addr -> IO (Int)

updateWindow :: HWND -> IO ()
updateWindow arg1 =
  prim_updateWindow arg1
  >>= \ gc_result ->
  access_prim_updateWindow_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_updateWindow_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall "Win32Window_stub_ffi.h prim_updateWindow" prim_updateWindow :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_updateWindow_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Window_stub_ffi.h" access_prim_updateWindow_gc_failstring :: Addr -> IO (Addr)

-- Return value of DispatchMessage is usually ignored
dispatchMessage :: LPMSG -> IO LONG
dispatchMessage arg1 =
  prim_dispatchMessage arg1
  >>= \  res1  ->
  (return (res1))
foreign import  ccall "Win32Window_stub_ffi.h prim_dispatchMessage" prim_dispatchMessage :: Addr -> IO (Int32)

sendMessage :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
sendMessage arg1 arg2 arg3 arg4 =
  prim_sendMessage arg1 arg2 arg3 arg4
  >>= \  res1  ->
  (return (res1))
foreign import  ccall "Win32Window_stub_ffi.h prim_sendMessage" prim_sendMessage :: Addr -> Word32 -> Word32 -> Int32 -> IO (Int32)

----------------------------------------------------------------

-- ToDo: figure out reentrancy stuff
-- ToDo: catch error codes
--
-- ToDo: how to send HWND_BROADCAST to PostMessage
-- %fun PostMessage       :: MbHWND -> WindowMessage -> WPARAM -> LPARAM -> IO ()
-- %fun PostQuitMessage   :: Int -> IO ()
-- %fun PostThreadMessage :: DWORD -> WindowMessage -> WPARAM -> LPARAM -> IO ()
-- %fun InSendMessage     :: IO Bool

----------------------------------------------------------------
-- End
----------------------------------------------------------------
