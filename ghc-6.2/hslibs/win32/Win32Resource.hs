module Win32Resource where

import StdDIS
import Addr
import Word
import Win32Types


beginUpdateResource :: String -> Bool -> IO HANDLE
beginUpdateResource gc_arg1 gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (marshall_bool_ gc_arg2) >>= \ (arg2) ->
  prim_beginUpdateResource arg1 arg2
  >>= \ gc_result ->
  access_prim_beginUpdateResource_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_beginUpdateResource_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_beginUpdateResource_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_beginUpdateResource" prim_beginUpdateResource :: Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_beginUpdateResource_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_beginUpdateResource_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_beginUpdateResource_gc_failstring :: Addr -> IO (Addr)

type ResourceImageType = UINT

type   HRSRC      = Addr

type   HGLOBAL    = Addr

foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_iMAGE_BITMAP" iMAGE_BITMAP :: ResourceImageType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_iMAGE_ICON" iMAGE_ICON :: ResourceImageType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_iMAGE_CURSOR" iMAGE_CURSOR :: ResourceImageType

copyImage :: HANDLE -> ResourceImageType -> Int -> Int -> UINT -> IO HANDLE
copyImage arg1 arg2 arg3 arg4 arg5 =
  prim_copyImage arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_copyImage_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_copyImage_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_copyImage_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_copyImage" prim_copyImage :: Addr -> Word32 -> Int -> Int -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_copyImage_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_copyImage_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_copyImage_gc_failstring :: Addr -> IO (Addr)

endUpdateResource :: HANDLE -> BOOL -> IO ()
endUpdateResource arg1 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg2) ->
  prim_endUpdateResource arg1 arg2
  >>= \ gc_result ->
  access_prim_endUpdateResource_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_endUpdateResource_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_endUpdateResource" prim_endUpdateResource :: Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_endUpdateResource_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_endUpdateResource_gc_failstring :: Addr -> IO (Addr)

type ResourceType = Addr

--lPCTSTR_ x

foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_ACCELERATOR" rT_ACCELERATOR :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_ANICURSOR" rT_ANICURSOR :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_ANIICON" rT_ANIICON :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_BITMAP" rT_BITMAP :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_CURSOR" rT_CURSOR :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_DIALOG" rT_DIALOG :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_FONT"   rT_FONT :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_FONTDIR" rT_FONTDIR :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_GROUP_CURSOR" rT_GROUP_CURSOR :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_GROUP_ICON" rT_GROUP_ICON :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_HTML" rT_HTML :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_ICON" rT_ICON :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_MENU" rT_MENU :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_MESSAGETABLE" rT_MESSAGETABLE :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_RCDATA" rT_RCDATA :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_STRING" rT_STRING :: ResourceType
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_rT_VERSION" rT_VERSION :: ResourceType

findResource :: HMODULE -> String -> ResourceType -> IO HRSRC
findResource arg1 gc_arg1 arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  prim_findResource arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_findResource_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_findResource_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_findResource_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_findResource" prim_findResource :: Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_findResource_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_findResource_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_findResource_gc_failstring :: Addr -> IO (Addr)

findResourceEx :: HMODULE -> LPCTSTR_ -> ResourceType -> WORD -> IO HRSRC
findResourceEx arg1 gc_arg1 arg3 arg4 =
  (marshall_lpctstr_ gc_arg1) >>= \ (arg2) ->
  prim_findResourceEx arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_findResourceEx_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_findResourceEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_findResourceEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_findResourceEx" prim_findResourceEx :: Addr -> Addr -> Addr -> Word16 -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_findResourceEx_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_findResourceEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_findResourceEx_gc_failstring :: Addr -> IO (Addr)

type ResourceSize = Int

foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_lR_DEFAULTSIZE" lR_DEFAULTSIZE :: ResourceSize

type LoadImageFlags = UINT

foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_lR_DEFAULTCOLOR" lR_DEFAULTCOLOR :: LoadImageFlags
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_lR_CREATEDIBSECTION" lR_CREATEDIBSECTION :: LoadImageFlags
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_lR_LOADFROMFILE" lR_LOADFROMFILE :: LoadImageFlags
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_lR_LOADMAP3DCOLORS" lR_LOADMAP3DCOLORS :: LoadImageFlags
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_lR_LOADTRANSPARENT" lR_LOADTRANSPARENT :: LoadImageFlags
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_lR_MONOCHROME" lR_MONOCHROME :: LoadImageFlags
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_lR_SHARED" lR_SHARED :: LoadImageFlags

-- % , LR_VGACOLOR (Not in mingw-20001111 headers)


loadImage :: HINSTANCE -> LPCTSTR_ -> ResourceImageType -> ResourceSize -> ResourceSize -> LoadImageFlags -> IO HANDLE
loadImage arg1 gc_arg1 arg3 arg4 arg5 arg6 =
  (marshall_lpctstr_ gc_arg1) >>= \ (arg2) ->
  prim_loadImage arg1 arg2 arg3 arg4 arg5 arg6
  >>= \ gc_result ->
  access_prim_loadImage_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_loadImage_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_loadImage_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_loadImage" prim_loadImage :: Addr -> Addr -> Word32 -> Int -> Int -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_loadImage_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_loadImage_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_loadImage_gc_failstring :: Addr -> IO (Addr)

loadResource :: HMODULE -> HRSRC -> IO HGLOBAL
loadResource arg1 arg2 =
  prim_loadResource arg1 arg2
  >>= \ gc_result ->
  access_prim_loadResource_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_loadResource_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_loadResource_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_loadResource" prim_loadResource :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_loadResource_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_loadResource_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_loadResource_gc_failstring :: Addr -> IO (Addr)

lockResource :: HGLOBAL -> IO Addr
lockResource arg1 =
  prim_lockResource arg1
  >>= \ gc_result ->
  access_prim_lockResource_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_lockResource_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_lockResource_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_lockResource" prim_lockResource :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_lockResource_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_lockResource_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_lockResource_gc_failstring :: Addr -> IO (Addr)

sizeofResource :: HMODULE -> HRSRC -> IO DWORD
sizeofResource arg1 arg2 =
  prim_sizeofResource arg1 arg2
  >>= \ gc_result ->
  access_prim_sizeofResource_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_sizeofResource_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_sizeofResource_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_sizeofResource" prim_sizeofResource :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_sizeofResource_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_sizeofResource_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_sizeofResource_gc_failstring :: Addr -> IO (Addr)

updateResource :: HANDLE -> LPCTSTR_ -> ResourceType -> WORD -> Addr -> DWORD -> IO ()
updateResource arg1 gc_arg1 arg3 arg4 arg5 arg6 =
  (marshall_lpctstr_ gc_arg1) >>= \ (arg2) ->
  prim_updateResource arg1 arg2 arg3 arg4 arg5 arg6
  >>= \ gc_result ->
  access_prim_updateResource_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_updateResource_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Resource_stub_ffi.h prim_updateResource" prim_updateResource :: Addr -> Addr -> Addr -> Word16 -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_updateResource_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Resource_stub_ffi.h" access_prim_updateResource_gc_failstring :: Addr -> IO (Addr)


