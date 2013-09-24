module Win32DLL where

import StdDIS
import Addr
import Word
import Win32Types


disableThreadLibraryCalls :: HMODULE -> IO ()
disableThreadLibraryCalls arg1 =
  prim_disableThreadLibraryCalls arg1
  >>= \ gc_result ->
  access_prim_disableThreadLibraryCalls_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_disableThreadLibraryCalls_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32DLL_stub_ffi.h prim_disableThreadLibraryCalls" prim_disableThreadLibraryCalls :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_disableThreadLibraryCalls_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_disableThreadLibraryCalls_gc_failstring :: Addr -> IO (Addr)

freeLibrary :: HMODULE -> IO ()
freeLibrary arg1 =
  prim_freeLibrary arg1
  >>= \ gc_result ->
  access_prim_freeLibrary_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_freeLibrary_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32DLL_stub_ffi.h prim_freeLibrary" prim_freeLibrary :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_freeLibrary_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_freeLibrary_gc_failstring :: Addr -> IO (Addr)

getModuleFileName :: HMODULE -> IO String
getModuleFileName arg1 =
  prim_getModuleFileName arg1
  >>= \ gc_result ->
  access_prim_getModuleFileName_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getModuleFileName_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getModuleFileName_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ res1) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall unsafe "Win32DLL_stub_ffi.h prim_getModuleFileName" prim_getModuleFileName :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_getModuleFileName_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_getModuleFileName_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_getModuleFileName_gc_failstring :: Addr -> IO (Addr)

getModuleHandle :: MbString -> IO HMODULE
getModuleHandle gc_arg1 =
  (case gc_arg1 of {
      Nothing -> (return (nullAddr));
      (Just gc_arg1) -> (marshall_string_ gc_arg1) >>= \ (arg1) ->
			(return ((arg1)))
   }) >>= \ (arg1) ->
  prim_getModuleHandle arg1
  >>= \ gc_result ->
  access_prim_getModuleHandle_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getModuleHandle_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getModuleHandle_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32DLL_stub_ffi.h prim_getModuleHandle" prim_getModuleHandle :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_getModuleHandle_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_getModuleHandle_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_getModuleHandle_gc_failstring :: Addr -> IO (Addr)

getProcAddress :: HMODULE -> String -> IO Addr
getProcAddress arg1 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  prim_getProcAddress arg1 arg2
  >>= \ gc_result ->
  access_prim_getProcAddress_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getProcAddress_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getProcAddress_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32DLL_stub_ffi.h prim_getProcAddress" prim_getProcAddress :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_getProcAddress_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_getProcAddress_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_getProcAddress_gc_failstring :: Addr -> IO (Addr)

loadLibrary :: String -> IO HINSTANCE
loadLibrary gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_loadLibrary arg1
  >>= \ gc_result ->
  access_prim_loadLibrary_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_loadLibrary_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_loadLibrary_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32DLL_stub_ffi.h prim_loadLibrary" prim_loadLibrary :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_loadLibrary_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_loadLibrary_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_loadLibrary_gc_failstring :: Addr -> IO (Addr)

type LoadLibraryFlags = DWORD

foreign import  ccall unsafe "Win32DLL_stub_ffi.h prim_lOAD_LIBRARY_AS_DATAFILE" lOAD_LIBRARY_AS_DATAFILE :: LoadLibraryFlags
foreign import  ccall unsafe "Win32DLL_stub_ffi.h prim_lOAD_WITH_ALTERED_SEARCH_PATH" lOAD_WITH_ALTERED_SEARCH_PATH :: LoadLibraryFlags

loadLibraryEx :: String -> HANDLE -> LoadLibraryFlags -> IO HINSTANCE
loadLibraryEx gc_arg1 arg2 arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_loadLibraryEx arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_loadLibraryEx_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_loadLibraryEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_loadLibraryEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32DLL_stub_ffi.h prim_loadLibraryEx" prim_loadLibraryEx :: Addr -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_loadLibraryEx_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_loadLibraryEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32DLL_stub_ffi.h" access_prim_loadLibraryEx_gc_failstring :: Addr -> IO (Addr)

