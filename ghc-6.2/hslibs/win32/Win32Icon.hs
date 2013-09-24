module Win32Icon where

import StdDIS
import Win32Types
import GDITypes


----------------------------------------------------------------
-- Icons
----------------------------------------------------------------

copyIcon :: HICON -> IO HICON
copyIcon arg1 =
  prim_copyIcon arg1
  >>= \ gc_result ->
  access_prim_copyIcon_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_copyIcon_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_copyIcon_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Icon_stub_ffi.h prim_copyIcon" prim_copyIcon :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Icon_stub_ffi.h" access_prim_copyIcon_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Icon_stub_ffi.h" access_prim_copyIcon_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Icon_stub_ffi.h" access_prim_copyIcon_gc_failstring :: Addr -> IO (Addr)

drawIcon :: HDC -> Int -> Int -> HICON -> IO ()
drawIcon arg1 arg2 arg3 arg4 =
  prim_drawIcon arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_drawIcon_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_drawIcon_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Icon_stub_ffi.h prim_drawIcon" prim_drawIcon :: Addr -> Int -> Int -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Icon_stub_ffi.h" access_prim_drawIcon_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Icon_stub_ffi.h" access_prim_drawIcon_gc_failstring :: Addr -> IO (Addr)

destroyIcon :: HICON -> IO ()
destroyIcon arg1 =
  prim_destroyIcon arg1
  >>= \ gc_result ->
  access_prim_destroyIcon_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_destroyIcon_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Icon_stub_ffi.h prim_destroyIcon" prim_destroyIcon :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Icon_stub_ffi.h" access_prim_destroyIcon_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Icon_stub_ffi.h" access_prim_destroyIcon_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------

