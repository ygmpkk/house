module Win32Path
	( beginPath, closeFigure, endPath, fillPath, flattenPath
	, pathToRegion, strokeAndFillPath, strokePath, widenPath
	) where

import StdDIS
import GDITypes
import Win32Types


----------------------------------------------------------------
-- Paths
----------------------------------------------------------------

-- %fun AbortPath       :: HDC -> IO ()

beginPath :: HDC -> IO ()
beginPath arg1 =
  prim_beginPath arg1
  >>= \ gc_result ->
  access_prim_beginPath_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_beginPath_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Path_stub_ffi.h prim_beginPath" prim_beginPath :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_beginPath_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_beginPath_gc_failstring :: Addr -> IO (Addr)

closeFigure :: HDC -> IO ()
closeFigure arg1 =
  prim_closeFigure arg1
  >>= \ gc_result ->
  access_prim_closeFigure_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_closeFigure_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Path_stub_ffi.h prim_closeFigure" prim_closeFigure :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_closeFigure_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_closeFigure_gc_failstring :: Addr -> IO (Addr)

endPath :: HDC -> IO ()
endPath arg1 =
  prim_endPath arg1
  >>= \ gc_result ->
  access_prim_endPath_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_endPath_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Path_stub_ffi.h prim_endPath" prim_endPath :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_endPath_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_endPath_gc_failstring :: Addr -> IO (Addr)

fillPath :: HDC -> IO ()
fillPath arg1 =
  prim_fillPath arg1
  >>= \ gc_result ->
  access_prim_fillPath_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_fillPath_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Path_stub_ffi.h prim_fillPath" prim_fillPath :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_fillPath_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_fillPath_gc_failstring :: Addr -> IO (Addr)

flattenPath :: HDC -> IO ()
flattenPath arg1 =
  prim_flattenPath arg1
  >>= \ gc_result ->
  access_prim_flattenPath_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_flattenPath_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Path_stub_ffi.h prim_flattenPath" prim_flattenPath :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_flattenPath_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_flattenPath_gc_failstring :: Addr -> IO (Addr)

pathToRegion :: HDC -> IO HRGN
pathToRegion arg1 =
  prim_pathToRegion arg1
  >>= \ gc_result ->
  access_prim_pathToRegion_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_pathToRegion_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_pathToRegion_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_pathToRegion_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Path_stub_ffi.h prim_pathToRegion" prim_pathToRegion :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_pathToRegion_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_pathToRegion_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_pathToRegion_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_pathToRegion_gc_failstring :: Addr -> IO (Addr)

strokeAndFillPath :: HDC -> IO ()
strokeAndFillPath arg1 =
  prim_strokeAndFillPath arg1
  >>= \ gc_result ->
  access_prim_strokeAndFillPath_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_strokeAndFillPath_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Path_stub_ffi.h prim_strokeAndFillPath" prim_strokeAndFillPath :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_strokeAndFillPath_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_strokeAndFillPath_gc_failstring :: Addr -> IO (Addr)

strokePath :: HDC -> IO ()
strokePath arg1 =
  prim_strokePath arg1
  >>= \ gc_result ->
  access_prim_strokePath_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_strokePath_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Path_stub_ffi.h prim_strokePath" prim_strokePath :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_strokePath_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_strokePath_gc_failstring :: Addr -> IO (Addr)

widenPath :: HDC -> IO ()
widenPath arg1 =
  prim_widenPath arg1
  >>= \ gc_result ->
  access_prim_widenPath_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_widenPath_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Path_stub_ffi.h prim_widenPath" prim_widenPath :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_widenPath_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Path_stub_ffi.h" access_prim_widenPath_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
