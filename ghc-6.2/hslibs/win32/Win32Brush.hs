module Win32Brush where

import StdDIS
import Win32Types
import GDITypes


----------------------------------------------------------------
-- Brush
----------------------------------------------------------------

createSolidBrush :: COLORREF -> IO HBRUSH
createSolidBrush arg1 =
  prim_createSolidBrush arg1
  >>= \ gc_result ->
  access_prim_createSolidBrush_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createSolidBrush_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createSolidBrush_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_createSolidBrush" prim_createSolidBrush :: Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_createSolidBrush_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_createSolidBrush_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_createSolidBrush_gc_failstring :: Addr -> IO (Addr)

createHatchBrush :: HatchStyle -> COLORREF -> IO HBRUSH
createHatchBrush arg1 arg2 =
  prim_createHatchBrush arg1 arg2
  >>= \ gc_result ->
  access_prim_createHatchBrush_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createHatchBrush_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createHatchBrush_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_createHatchBrush" prim_createHatchBrush :: Word16 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_createHatchBrush_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_createHatchBrush_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_createHatchBrush_gc_failstring :: Addr -> IO (Addr)

createPatternBrush :: HBITMAP -> IO HBRUSH
createPatternBrush arg1 =
  prim_createPatternBrush arg1
  >>= \ gc_result ->
  access_prim_createPatternBrush_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createPatternBrush_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createPatternBrush_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_createPatternBrush" prim_createPatternBrush :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_createPatternBrush_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_createPatternBrush_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_createPatternBrush_gc_failstring :: Addr -> IO (Addr)

deleteBrush :: HBRUSH -> IO ()
deleteBrush arg1 =
  prim_deleteBrush arg1
  >>= \ gc_result ->
  access_prim_deleteBrush_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_deleteBrush_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_deleteBrush" prim_deleteBrush :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_deleteBrush_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_deleteBrush_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------

type StockBrush   = WORD

foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_wHITE_BRUSH" wHITE_BRUSH :: StockBrush
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_lTGRAY_BRUSH" lTGRAY_BRUSH :: StockBrush
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_gRAY_BRUSH" gRAY_BRUSH :: StockBrush
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_dKGRAY_BRUSH" dKGRAY_BRUSH :: StockBrush
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_bLACK_BRUSH" bLACK_BRUSH :: StockBrush
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_nULL_BRUSH" nULL_BRUSH :: StockBrush
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_hOLLOW_BRUSH" hOLLOW_BRUSH :: StockBrush

getStockBrush :: StockBrush -> IO HBRUSH
getStockBrush arg1 =
  prim_getStockBrush arg1
  >>= \ gc_result ->
  access_prim_getStockBrush_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getStockBrush_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getStockBrush_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Brush_stub_ffi.h prim_getStockBrush" prim_getStockBrush :: Word16 -> IO (Addr)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_getStockBrush_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_getStockBrush_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Brush_stub_ffi.h" access_prim_getStockBrush_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------

