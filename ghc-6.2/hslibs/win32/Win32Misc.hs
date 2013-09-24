module Win32Misc where

import StdDIS
import GDITypes
import Win32Types


----------------------------------------------------------------
-- Resources 
-- (should probably be distributed between 
--  Win32{Icon,Cursor,Accelerator,Menu,...}.gc)
----------------------------------------------------------------

type Accelerator = LPCSTR
-- intToAccelerator :: Int -> Accelerator
-- intToAccelerator i = makeIntResource (toWord i)

-- cursor and icon should not be const pointer; GSL ???
type Cursor = LPSTR
-- intToCursor :: Int -> Cursor
-- intToCursor i = makeIntResource (toWord i)

type Icon = LPSTR
-- intToIcon :: Int -> Icon
-- intToIcon i = makeIntResource (toWord i)

loadAccelerators :: MbHINSTANCE -> Accelerator -> IO HACCEL
loadAccelerators arg1 arg2 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_loadAccelerators arg1 arg2
  >>= \ gc_result ->
  access_prim_loadAccelerators_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_loadAccelerators_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_loadAccelerators_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_loadAccelerators" prim_loadAccelerators :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_loadAccelerators_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_loadAccelerators_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_loadAccelerators_gc_failstring :: Addr -> IO (Addr)

loadCursor :: MbHINSTANCE -> Cursor -> IO HCURSOR
loadCursor arg1 arg2 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_loadCursor arg1 arg2
  >>= \ gc_result ->
  access_prim_loadCursor_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_loadCursor_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_loadCursor_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_loadCursor" prim_loadCursor :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_loadCursor_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_loadCursor_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_loadCursor_gc_failstring :: Addr -> IO (Addr)

loadIcon :: MbHINSTANCE -> Icon -> IO HICON
loadIcon arg1 arg2 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_loadIcon arg1 arg2
  >>= \ gc_result ->
  access_prim_loadIcon_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_loadIcon_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_loadIcon_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_loadIcon" prim_loadIcon :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_loadIcon_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_loadIcon_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_loadIcon_gc_failstring :: Addr -> IO (Addr)

foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDC_ARROW" iDC_ARROW :: Cursor
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDC_IBEAM" iDC_IBEAM :: Cursor
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDC_WAIT"  iDC_WAIT :: Cursor
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDC_CROSS" iDC_CROSS :: Cursor
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDC_UPARROW" iDC_UPARROW :: Cursor
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDC_SIZENWSE" iDC_SIZENWSE :: Cursor
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDC_SIZENESW" iDC_SIZENESW :: Cursor
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDC_SIZEWE" iDC_SIZEWE :: Cursor
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDC_SIZENS" iDC_SIZENS :: Cursor

foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDI_APPLICATION" iDI_APPLICATION :: Icon
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDI_HAND" iDI_HAND :: Icon
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDI_QUESTION" iDI_QUESTION :: Icon
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDI_EXCLAMATION" iDI_EXCLAMATION :: Icon
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDI_ASTERISK" iDI_ASTERISK :: Icon

----------------------------------------------------------------
-- Message Boxes
----------------------------------------------------------------

type MBStyle = UINT

foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_OK" mB_OK :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_OKCANCEL" mB_OKCANCEL :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_ABORTRETRYIGNORE" mB_ABORTRETRYIGNORE :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_YESNOCANCEL" mB_YESNOCANCEL :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_YESNO" mB_YESNO :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_RETRYCANCEL" mB_RETRYCANCEL :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_ICONHAND" mB_ICONHAND :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_ICONQUESTION" mB_ICONQUESTION :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_ICONEXCLAMATION" mB_ICONEXCLAMATION :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_ICONASTERISK" mB_ICONASTERISK :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_ICONINFORMATION" mB_ICONINFORMATION :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_ICONSTOP" mB_ICONSTOP :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_DEFBUTTON1" mB_DEFBUTTON1 :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_DEFBUTTON2" mB_DEFBUTTON2 :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_DEFBUTTON3" mB_DEFBUTTON3 :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_APPLMODAL"  mB_APPLMODAL :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_SYSTEMMODAL" mB_SYSTEMMODAL :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_TASKMODAL"  mB_TASKMODAL :: MBStyle
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_mB_SETFOREGROUND" mB_SETFOREGROUND :: MBStyle

type MBStatus = UINT

foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDABORT" prim_iDABORT :: MBStatus
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDCANCEL" prim_iDCANCEL :: MBStatus
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDIGNORE" prim_iDIGNORE :: MBStatus
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDNO" prim_iDNO :: MBStatus
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDOK" prim_iDOK :: MBStatus
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDRETRY" prim_iDRETRY :: MBStatus
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_iDYES" prim_iDYES :: MBStatus

-- Note: if the error is ever raised, we're in a very sad way!
messageBox :: HWND -> String -> String -> MBStyle -> IO MBStatus
messageBox arg1 gc_arg1 gc_arg2 arg4 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  (marshall_string_ gc_arg2) >>= \ (arg3) ->
  prim_messageBox arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_messageBox_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_messageBox_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_messageBox_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_messageBox" prim_messageBox :: Addr -> Addr -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_messageBox_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_messageBox_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_messageBox_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
--
----------------------------------------------------------------

-- %fun GetModuleHandle :: MbString -> IO HMODULE
-- %fail { res1 == 0 } { ErrorWin("GetModuleHandle") }

type StdHandleId   = DWORD

foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_sTD_INPUT_HANDLE" sTD_INPUT_HANDLE :: StdHandleId
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_sTD_OUTPUT_HANDLE" sTD_OUTPUT_HANDLE :: StdHandleId
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_sTD_ERROR_HANDLE" sTD_ERROR_HANDLE :: StdHandleId

getStdHandle :: StdHandleId -> IO HANDLE
getStdHandle arg1 =
  prim_getStdHandle arg1
  >>= \ gc_result ->
  access_prim_getStdHandle_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getStdHandle_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getStdHandle_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_getStdHandle" prim_getStdHandle :: Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_getStdHandle_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_getStdHandle_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_getStdHandle_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Rotatable Ellipse hack
--
-- Win95 (Win32?) doesn't support rotating ellipses - so we 
-- implement them with polygons.
--
-- We use a fixed number of edges rather than varying the number 
-- according to the radius of the ellipse.  
-- If anyone feels like improving the code (to vary the number),
-- they should place a fixed upper bound on the number of edges
-- since it takes a relatively long time to draw 1000 edges.
----------------------------------------------------------------


-- #define SIN_Cache_Size 20

transformedEllipse :: HDC -> POINT -> POINT -> POINT -> IO ()
transformedEllipse hdc gc_arg1 gc_arg5 gc_arg9 =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  case gc_arg5 of { (gc_arg6,gc_arg7) ->
  case gc_arg9 of { (gc_arg10,gc_arg11) ->
  prim_transformedEllipse hdc gc_arg2 gc_arg3 gc_arg6 gc_arg7 gc_arg10 gc_arg11
  >>= \ gc_result ->
  access_prim_transformedEllipse_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_transformedEllipse_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}}}
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_transformedEllipse" prim_transformedEllipse :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_transformedEllipse_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_transformedEllipse_gc_failstring :: Addr -> IO (Addr)


----------------------------------------------------------------
-- Cursor
----------------------------------------------------------------

-- %fun getCursorPos :: IO POINT
-- %code BOOL success = GetCursorPos(p);
-- %fail { !success } { ErrorString/Win("GetCursorPos") }
-- %result (point p)
-- 
-- %fun SetCursorPos :: POINT -> IO ()
-- %code BOOL success = SetCursorPos(arg1->x,arg1->y);
-- %fail { !success } { ErrorString/Win("SetCursorPos") }
--
-- %fun ClipCursor :: RECT -> IO ()
-- %call (declare {RECT} arg1 in (rect {arg1}))
-- %code BOOL success = ClipCursor(&arg1);
-- %fail { !success } { ErrorString/Win("ClipCursor") }
--
-- %fun getClipCursor :: IO RECT
-- %code BOOL success = GetClipCursor(&res1) } )
-- %fail { !success } { ErrorString/Win("GetClipCursor") }
-- %result (declare {RECT} res1 in (rect {res1}))

----------------------------------------------------------------
-- Exit/shutdown
----------------------------------------------------------------

type ExitOption = UINT

foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_eWX_FORCE" eWX_FORCE :: ExitOption
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_eWX_LOGOFF" eWX_LOGOFF :: ExitOption
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_eWX_POWEROFF" eWX_POWEROFF :: ExitOption
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_eWX_REBOOT" eWX_REBOOT :: ExitOption
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_eWX_SHUTDOWN" eWX_SHUTDOWN :: ExitOption

-- %fun exitWindowsEx :: ExitOption -> IO ()
-- %code ExitWindowEx(arg1,0);
-- %fail { !success } { ErrorWin("ExitWindowsEx") }
--
-- %fun exitWindows :: IO ()
-- %code ExitWindows(0,0)
-- %fail { !success } { ErrorWin("ExitWindows") }

----------------------------------------------------------------
-- Beeping
----------------------------------------------------------------

type MbBeep = Maybe UINT

type Duration   = Int

type MbDuration   = Maybe Duration

messageBeep :: MbBeep -> IO ()
messageBeep arg1 =
  (case arg1 of {
      Nothing -> (return (0xFFFFFFFF));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_messageBeep arg1
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_messageBeep" prim_messageBeep :: Word32 -> IO ()

beep :: WORD -> MbDuration -> IO ()
beep arg1 arg2 =
  (case arg2 of {
      Nothing -> (return (-1));
      (Just arg2) -> (return ((arg2)))
   }) >>= \ (arg2) ->
  prim_beep arg1 arg2
  >>= \ gc_result ->
  access_prim_beep_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_beep_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_beep" prim_beep :: Word16 -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_beep_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_beep_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Timers
----------------------------------------------------------------

type TimerId   = UINT

-- ToDo: support the other two forms of timer initialisation

-- Cause WM_TIMER events to be sent to window callback
setWinTimer :: HWND -> TimerId -> UINT -> IO TimerId
setWinTimer arg1 arg2 arg3 =
  prim_setWinTimer arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_setWinTimer_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setWinTimer_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setWinTimer_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_setWinTimer" prim_setWinTimer :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_setWinTimer_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_setWinTimer_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_setWinTimer_gc_failstring :: Addr -> IO (Addr)

killTimer :: MbHWND -> TimerId -> IO ()
killTimer arg1 arg2 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_killTimer arg1 arg2
  >>= \ gc_result ->
  access_prim_killTimer_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_killTimer_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_killTimer" prim_killTimer :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_killTimer_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Misc_stub_ffi.h" access_prim_killTimer_gc_failstring :: Addr -> IO (Addr)

-- For documentation purposes:
type MilliSeconds = DWORD

timeGetTime :: IO MilliSeconds
timeGetTime =
  prim_timeGetTime
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Misc_stub_ffi.h prim_timeGetTime" prim_timeGetTime :: IO (Word32)

----------------------------------------------------------------

-- %fun ezCreateFont :: Unknown
-- %result BITMAP({ getBitmapInfo(x) })

----------------------------------------------------------------
-- End
----------------------------------------------------------------
