module Win32Region where

import StdDIS
import Win32Types
import GDITypes


----------------------------------------------------------------
-- Regions
----------------------------------------------------------------

combineRgn :: HRGN -> HRGN -> HRGN -> ClippingMode -> IO RegionType
combineRgn arg1 arg2 arg3 arg4 =
  withForeignPtr arg1 $ \ arg1 ->
  withForeignPtr arg2 $ \ arg2 ->
  withForeignPtr arg3 $ \ arg3 ->
  prim_combineRgn arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_combineRgn_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_combineRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_combineRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_combineRgn" prim_combineRgn :: Ptr () -> Ptr () -> Ptr () -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_combineRgn_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_combineRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_combineRgn_gc_failstring :: Addr -> IO (Addr)

offsetRgn :: HRGN -> INT -> INT -> IO RegionType
offsetRgn arg1 arg2 arg3 =
  withForeignPtr arg1 $ \ arg1 ->
  prim_offsetRgn arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_offsetRgn_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_offsetRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_offsetRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_offsetRgn" prim_offsetRgn :: Ptr () -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_offsetRgn_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_offsetRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_offsetRgn_gc_failstring :: Addr -> IO (Addr)

getRgnBox :: HRGN -> LPRECT -> IO RegionType
getRgnBox arg1 arg2 =
  withForeignPtr arg1 $ \ arg1 ->
  prim_getRgnBox arg1 arg2
  >>= \ gc_result ->
  access_prim_getRgnBox_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getRgnBox_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getRgnBox_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_getRgnBox" prim_getRgnBox :: Ptr () -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_getRgnBox_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_getRgnBox_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_getRgnBox_gc_failstring :: Addr -> IO (Addr)

createEllipticRgn :: INT -> INT -> INT -> INT -> IO HRGN
createEllipticRgn arg1 arg2 arg3 arg4 =
  prim_createEllipticRgn arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_createEllipticRgn_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_createEllipticRgn_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_createEllipticRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createEllipticRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_createEllipticRgn" prim_createEllipticRgn :: Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createEllipticRgn_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createEllipticRgn_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createEllipticRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createEllipticRgn_gc_failstring :: Addr -> IO (Addr)

createEllipticRgnIndirect :: LPRECT -> IO HRGN
createEllipticRgnIndirect arg1 =
  prim_createEllipticRgnIndirect arg1
  >>= \ gc_result ->
  access_prim_createEllipticRgnIndirect_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_createEllipticRgnIndirect_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_createEllipticRgnIndirect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createEllipticRgnIndirect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_createEllipticRgnIndirect" prim_createEllipticRgnIndirect :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createEllipticRgnIndirect_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createEllipticRgnIndirect_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createEllipticRgnIndirect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createEllipticRgnIndirect_gc_failstring :: Addr -> IO (Addr)

createRectRgn :: INT -> INT -> INT -> INT -> IO HRGN
createRectRgn arg1 arg2 arg3 arg4 =
  prim_createRectRgn arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_createRectRgn_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_createRectRgn_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_createRectRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createRectRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_createRectRgn" prim_createRectRgn :: Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRectRgn_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRectRgn_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRectRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRectRgn_gc_failstring :: Addr -> IO (Addr)

createRectRgnIndirect :: LPRECT -> IO HRGN
createRectRgnIndirect arg1 =
  prim_createRectRgnIndirect arg1
  >>= \ gc_result ->
  access_prim_createRectRgnIndirect_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_createRectRgnIndirect_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_createRectRgnIndirect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createRectRgnIndirect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_createRectRgnIndirect" prim_createRectRgnIndirect :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRectRgnIndirect_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRectRgnIndirect_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRectRgnIndirect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRectRgnIndirect_gc_failstring :: Addr -> IO (Addr)

createRoundRectRgn :: INT -> INT -> INT -> INT -> INT -> INT -> IO HRGN
createRoundRectRgn arg1 arg2 arg3 arg4 arg5 arg6 =
  prim_createRoundRectRgn arg1 arg2 arg3 arg4 arg5 arg6
  >>= \ gc_result ->
  access_prim_createRoundRectRgn_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_createRoundRectRgn_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_createRoundRectRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createRoundRectRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_createRoundRectRgn" prim_createRoundRectRgn :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRoundRectRgn_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRoundRectRgn_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRoundRectRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createRoundRectRgn_gc_failstring :: Addr -> IO (Addr)

createPolygonRgn :: [POINT] -> PolyFillMode -> IO HRGN
createPolygonRgn gc_arg1 mode =
  (marshall_listLenPOINT_ gc_arg1) >>= \ (gc_arg2) ->
  case gc_arg2 of { (ps,num_ps) ->
  prim_createPolygonRgn ps num_ps mode
  >>= \ gc_result ->
  access_prim_createPolygonRgn_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_createPolygonRgn_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_createPolygonRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createPolygonRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))}
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_createPolygonRgn" prim_createPolygonRgn :: Addr -> Int -> Word16 -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createPolygonRgn_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createPolygonRgn_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createPolygonRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_createPolygonRgn_gc_failstring :: Addr -> IO (Addr)

-- Needs to do proper error test for EqualRgn; GSL ???
equalRgn :: HRGN -> HRGN -> IO Bool
equalRgn arg1 arg2 =
  withForeignPtr arg1 $ \ arg1 ->
  withForeignPtr arg2 $ \ arg2 ->
  prim_equalRgn arg1 arg2
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_equalRgn" prim_equalRgn :: Ptr () -> Ptr () -> IO (Int)

fillRgn :: HDC -> HRGN -> HBRUSH -> IO ()
fillRgn arg1 arg2 arg3 =
  withForeignPtr arg2 $ \ arg2 ->
  prim_fillRgn arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_fillRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_fillRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_fillRgn" prim_fillRgn :: Addr -> Ptr () -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_fillRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_fillRgn_gc_failstring :: Addr -> IO (Addr)

invertRgn :: HDC -> HRGN -> IO ()
invertRgn arg1 arg2 =
  withForeignPtr arg2 $ \ arg2 ->
  prim_invertRgn arg1 arg2
  >>= \ gc_result ->
  access_prim_invertRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_invertRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_invertRgn" prim_invertRgn :: Addr -> Ptr () -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_invertRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_invertRgn_gc_failstring :: Addr -> IO (Addr)

paintRgn :: HDC -> HRGN -> IO ()
paintRgn arg1 arg2 =
  withForeignPtr arg2 $ \ arg2 ->
  prim_paintRgn arg1 arg2
  >>= \ gc_result ->
  access_prim_paintRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_paintRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_paintRgn" prim_paintRgn :: Addr -> Ptr () -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_paintRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_paintRgn_gc_failstring :: Addr -> IO (Addr)

frameRgn :: HDC -> HRGN -> HBRUSH -> Int -> Int -> IO ()
frameRgn arg1 arg2 arg3 arg4 arg5 =
  withForeignPtr arg2 $ \ arg2 ->
  prim_frameRgn arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_frameRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_frameRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_frameRgn" prim_frameRgn :: Addr -> Ptr () -> Addr -> Int -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_frameRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_frameRgn_gc_failstring :: Addr -> IO (Addr)

ptInRegion :: HRGN -> Int -> Int -> IO Bool
ptInRegion arg1 arg2 arg3 =
  withForeignPtr arg1 $ \ arg1 ->
  prim_ptInRegion arg1 arg2 arg3
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_ptInRegion" prim_ptInRegion :: Ptr () -> Int -> Int -> IO (Int)

rectInRegion :: HRGN -> RECT -> IO Bool
rectInRegion arg1 gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3,gc_arg4,gc_arg5) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_rectInRegion arg1 gc_arg2 gc_arg3 gc_arg4 gc_arg5
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))}
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_rectInRegion" prim_rectInRegion :: Ptr () -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Int)

deleteRegion :: HRGN -> IO ()
deleteRegion arg1 =
  withForeignPtr arg1 $ \ arg1 ->
  prim_deleteRegion arg1
  >>= \ gc_result ->
  access_prim_deleteRegion_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_deleteRegion_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Region_stub_ffi.h prim_deleteRegion" prim_deleteRegion :: Ptr () -> IO (Addr)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_deleteRegion_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Region_stub_ffi.h" access_prim_deleteRegion_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
