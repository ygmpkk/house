module Win32Pen where

import StdDIS
import Win32Types
import GDITypes


----------------------------------------------------------------
-- Stock Objects
----------------------------------------------------------------

type StockPen   = WORD

foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_wHITE_PEN" wHITE_PEN :: StockPen
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_bLACK_PEN" bLACK_PEN :: StockPen
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_nULL_PEN"  nULL_PEN :: StockPen

getStockPen :: StockPen -> IO HPEN
getStockPen arg1 =
  prim_getStockPen arg1
  >>= \ gc_result ->
  access_prim_getStockPen_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getStockPen_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getStockPen_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_getStockPen" prim_getStockPen :: Word16 -> IO (Addr)
foreign import ccall unsafe "Win32Pen_stub_ffi.h" access_prim_getStockPen_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Pen_stub_ffi.h" access_prim_getStockPen_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Pen_stub_ffi.h" access_prim_getStockPen_gc_failstring :: Addr -> IO (Addr)

deletePen :: HPEN -> IO ()
deletePen arg1 =
  prim_deletePen arg1
  >>= \ gc_result ->
  access_prim_deletePen_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_deletePen_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_deletePen" prim_deletePen :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Pen_stub_ffi.h" access_prim_deletePen_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Pen_stub_ffi.h" access_prim_deletePen_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Creating pens
----------------------------------------------------------------

type PenStyle   = Int32

foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_SOLID" pS_SOLID :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_DASH"  pS_DASH  :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_DOT"   pS_DOT   :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_DASHDOT" pS_DASHDOT :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_DASHDOTDOT" pS_DASHDOTDOT :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_NULL" pS_NULL :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_INSIDEFRAME" pS_INSIDEFRAME :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_USERSTYLE" pS_USERSTYLE :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_ALTERNATE" pS_ALTERNATE :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_STYLE_MASK" pS_STYLE_MASK :: PenStyle

foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_ENDCAP_ROUND" pS_ENDCAP_ROUND :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_ENDCAP_SQUARE" pS_ENDCAP_SQUARE :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_ENDCAP_FLAT" pS_ENDCAP_FLAT :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_ENDCAP_MASK" pS_ENDCAP_MASK :: PenStyle

{-
If PS_JOIN_MASK is not defined with your GNU Windows32 header files,
you'll have to define it.
-}
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_JOIN_ROUND" pS_JOIN_ROUND :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_JOIN_BEVEL" pS_JOIN_BEVEL :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_JOIN_MITER" pS_JOIN_MITER :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_COSMETIC"   pS_COSMETIC   :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_GEOMETRIC"  pS_GEOMETRIC  :: PenStyle
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_pS_TYPE_MASK"  pS_TYPE_MASK  :: PenStyle

createPen :: PenStyle -> INT -> COLORREF -> IO HPEN
createPen arg1 arg2 arg3 =
  prim_createPen arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_createPen_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createPen_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createPen_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Pen_stub_ffi.h prim_createPen" prim_createPen :: Int32 -> Int32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Pen_stub_ffi.h" access_prim_createPen_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Pen_stub_ffi.h" access_prim_createPen_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Pen_stub_ffi.h" access_prim_createPen_gc_failstring :: Addr -> IO (Addr)

-- Not very well supported on Win'95
-- %fun NullHANDLE ExtCreatePen :: PenStyle -> INT -> LOGBRUSH -> [StyleBit] -> IO HPEN

-- ToDo: CreatePenIndirect

----------------------------------------------------------------
-- End
----------------------------------------------------------------
