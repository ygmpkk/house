module Win32Clip where

import StdDIS
import Win32Types
import GDITypes
import Win32WinMessage


type ClipboardFormat = UINT

foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_BITMAP" cF_BITMAP :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_DIB" cF_DIB :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_DIF" cF_DIF :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_DSPBITMAP" cF_DSPBITMAP :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_DSPENHMETAFILE" cF_DSPENHMETAFILE :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_DSPMETAFILEPICT" cF_DSPMETAFILEPICT :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_DSPTEXT" cF_DSPTEXT :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_ENHMETAFILE" cF_ENHMETAFILE :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_GDIOBJFIRST" cF_GDIOBJFIRST :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_HDROP" cF_HDROP :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_LOCALE" cF_LOCALE :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_METAFILEPICT" cF_METAFILEPICT :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_OEMTEXT" cF_OEMTEXT :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_OWNERDISPLAY" cF_OWNERDISPLAY :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_PALETTE" cF_PALETTE :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_PENDATA" cF_PENDATA :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_PRIVATEFIRST" cF_PRIVATEFIRST :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_PRIVATELAST" cF_PRIVATELAST :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_RIFF" cF_RIFF :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_SYLK" cF_SYLK :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_TEXT" cF_TEXT :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_WAVE" cF_WAVE :: ClipboardFormat
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_cF_TIFF" cF_TIFF :: ClipboardFormat

-- % , CF_UNICODETEXT  -- WinNT only

changeClipboardChain :: HWND -> HWND -> IO Bool
changeClipboardChain arg1 arg2 =
  prim_changeClipboardChain arg1 arg2
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_changeClipboardChain" prim_changeClipboardChain :: Addr -> Addr -> IO (Int)

closeClipboard :: IO ()
closeClipboard =
  prim_closeClipboard
  >>= \ gc_result ->
  access_prim_closeClipboard_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_closeClipboard_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_closeClipboard" prim_closeClipboard :: IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_closeClipboard_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_closeClipboard_gc_failstring :: Addr -> IO (Addr)

countClipboardFormats :: IO Int
countClipboardFormats =
  prim_countClipboardFormats
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_countClipboardFormats" prim_countClipboardFormats :: IO (Int)

emptyClipboard :: IO ()
emptyClipboard =
  prim_emptyClipboard
  >>= \ gc_result ->
  access_prim_emptyClipboard_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_emptyClipboard_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_emptyClipboard" prim_emptyClipboard :: IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_emptyClipboard_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_emptyClipboard_gc_failstring :: Addr -> IO (Addr)

enumClipboardFormats :: UINT -> IO UINT
enumClipboardFormats arg1 =
  prim_enumClipboardFormats arg1
  >>= \ gc_result ->
  access_prim_enumClipboardFormats_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_enumClipboardFormats_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_enumClipboardFormats_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_enumClipboardFormats" prim_enumClipboardFormats :: Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_enumClipboardFormats_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_enumClipboardFormats_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_enumClipboardFormats_gc_failstring :: Addr -> IO (Addr)

getClipboardData :: ClipboardFormat -> IO HANDLE
getClipboardData arg1 =
  prim_getClipboardData arg1
  >>= \ gc_result ->
  access_prim_getClipboardData_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getClipboardData_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getClipboardData_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_getClipboardData" prim_getClipboardData :: Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardData_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardData_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardData_gc_failstring :: Addr -> IO (Addr)

getClipboardFormatName :: ClipboardFormat -> Addr -> Int -> IO Int
getClipboardFormatName arg1 arg2 arg3 =
  prim_getClipboardFormatName arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_getClipboardFormatName_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getClipboardFormatName_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getClipboardFormatName_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_getClipboardFormatName" prim_getClipboardFormatName :: Word32 -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardFormatName_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardFormatName_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardFormatName_gc_failstring :: Addr -> IO (Addr)

getClipboardOwner :: IO HWND
getClipboardOwner =
  prim_getClipboardOwner
  >>= \ gc_result ->
  access_prim_getClipboardOwner_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getClipboardOwner_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getClipboardOwner_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_getClipboardOwner" prim_getClipboardOwner :: IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardOwner_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardOwner_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardOwner_gc_failstring :: Addr -> IO (Addr)

getClipboardViewer :: IO HWND
getClipboardViewer =
  prim_getClipboardViewer
  >>= \ gc_result ->
  access_prim_getClipboardViewer_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getClipboardViewer_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getClipboardViewer_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_getClipboardViewer" prim_getClipboardViewer :: IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardViewer_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardViewer_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getClipboardViewer_gc_failstring :: Addr -> IO (Addr)

getOpenClipboardWindow :: IO HWND
getOpenClipboardWindow =
  prim_getOpenClipboardWindow
  >>= \ gc_result ->
  access_prim_getOpenClipboardWindow_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getOpenClipboardWindow_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getOpenClipboardWindow_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_getOpenClipboardWindow" prim_getOpenClipboardWindow :: IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getOpenClipboardWindow_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getOpenClipboardWindow_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getOpenClipboardWindow_gc_failstring :: Addr -> IO (Addr)

getPriorityClipboardFormat :: Addr -> Int -> IO Int
getPriorityClipboardFormat arg1 arg2 =
  prim_getPriorityClipboardFormat arg1 arg2
  >>= \ gc_result ->
  access_prim_getPriorityClipboardFormat_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getPriorityClipboardFormat_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getPriorityClipboardFormat_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_getPriorityClipboardFormat" prim_getPriorityClipboardFormat :: Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getPriorityClipboardFormat_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getPriorityClipboardFormat_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_getPriorityClipboardFormat_gc_failstring :: Addr -> IO (Addr)

isClipboardFormatAvailable :: ClipboardFormat -> IO BOOL
isClipboardFormatAvailable arg1 =
  prim_isClipboardFormatAvailable arg1
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_isClipboardFormatAvailable" prim_isClipboardFormatAvailable :: Word32 -> IO (Int)

openClipboard :: HWND -> IO ()
openClipboard arg1 =
  prim_openClipboard arg1
  >>= \ gc_result ->
  access_prim_openClipboard_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_openClipboard_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_openClipboard" prim_openClipboard :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_openClipboard_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_openClipboard_gc_failstring :: Addr -> IO (Addr)

registerClipboardFormat :: String -> IO ClipboardFormat
registerClipboardFormat gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_registerClipboardFormat arg1
  >>= \ gc_result ->
  access_prim_registerClipboardFormat_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_registerClipboardFormat_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_registerClipboardFormat_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_registerClipboardFormat" prim_registerClipboardFormat :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_registerClipboardFormat_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_registerClipboardFormat_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_registerClipboardFormat_gc_failstring :: Addr -> IO (Addr)

setClipboardData :: ClipboardFormat -> HANDLE -> IO HANDLE
setClipboardData arg1 arg2 =
  prim_setClipboardData arg1 arg2
  >>= \ gc_result ->
  access_prim_setClipboardData_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setClipboardData_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setClipboardData_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_setClipboardData" prim_setClipboardData :: Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_setClipboardData_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_setClipboardData_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_setClipboardData_gc_failstring :: Addr -> IO (Addr)

setClipboardViewer :: HWND -> IO HWND
setClipboardViewer arg1 =
  prim_setClipboardViewer arg1
  >>= \ gc_result ->
  access_prim_setClipboardViewer_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setClipboardViewer_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setClipboardViewer_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Clip_stub_ffi.h prim_setClipboardViewer" prim_setClipboardViewer :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_setClipboardViewer_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_setClipboardViewer_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Clip_stub_ffi.h" access_prim_setClipboardViewer_gc_failstring :: Addr -> IO (Addr)







