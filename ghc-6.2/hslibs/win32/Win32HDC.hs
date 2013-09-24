module Win32HDC
	( module Win32HDC
	) where

import Win32Types
import StdDIS
import GDITypes


---------------- Macros to set/get fields of HDC ----------------









----------------------------------------------------------------

setArcDirection :: HDC -> ArcDirection -> IO ArcDirection
setArcDirection arg1 arg2 =
  prim_setArcDirection arg1 arg2
  >>= \ gc_result ->
  access_prim_setArcDirection_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setArcDirection_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setArcDirection_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setArcDirection" prim_setArcDirection :: Addr -> Word16 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setArcDirection_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setArcDirection_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setArcDirection_gc_failstring :: Addr -> IO (Addr)
getArcDirection :: HDC -> IO ArcDirection
getArcDirection arg1 =
  prim_getArcDirection arg1
  >>= \ gc_result ->
  access_prim_getArcDirection_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getArcDirection_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getArcDirection_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getArcDirection" prim_getArcDirection :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getArcDirection_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getArcDirection_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getArcDirection_gc_failstring :: Addr -> IO (Addr)
       
setPolyFillMode :: HDC -> PolyFillMode -> IO PolyFillMode
setPolyFillMode arg1 arg2 =
  prim_setPolyFillMode arg1 arg2
  >>= \ gc_result ->
  access_prim_setPolyFillMode_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setPolyFillMode_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setPolyFillMode_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setPolyFillMode" prim_setPolyFillMode :: Addr -> Word16 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setPolyFillMode_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setPolyFillMode_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setPolyFillMode_gc_failstring :: Addr -> IO (Addr)
getPolyFillMode :: HDC -> IO PolyFillMode
getPolyFillMode arg1 =
  prim_getPolyFillMode arg1
  >>= \ gc_result ->
  access_prim_getPolyFillMode_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getPolyFillMode_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getPolyFillMode_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getPolyFillMode" prim_getPolyFillMode :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getPolyFillMode_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getPolyFillMode_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getPolyFillMode_gc_failstring :: Addr -> IO (Addr)
       
setGraphicsMode :: HDC -> GraphicsMode -> IO GraphicsMode
setGraphicsMode arg1 arg2 =
  prim_setGraphicsMode arg1 arg2
  >>= \ gc_result ->
  access_prim_setGraphicsMode_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setGraphicsMode_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setGraphicsMode_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setGraphicsMode" prim_setGraphicsMode :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setGraphicsMode_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setGraphicsMode_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setGraphicsMode_gc_failstring :: Addr -> IO (Addr)
getGraphicsMode :: HDC -> IO GraphicsMode
getGraphicsMode arg1 =
  prim_getGraphicsMode arg1
  >>= \ gc_result ->
  access_prim_getGraphicsMode_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getGraphicsMode_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getGraphicsMode_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getGraphicsMode" prim_getGraphicsMode :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getGraphicsMode_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getGraphicsMode_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getGraphicsMode_gc_failstring :: Addr -> IO (Addr)
       
setStretchBltMode :: HDC -> StretchBltMode -> IO StretchBltMode
setStretchBltMode arg1 arg2 =
  prim_setStretchBltMode arg1 arg2
  >>= \ gc_result ->
  access_prim_setStretchBltMode_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setStretchBltMode_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setStretchBltMode_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setStretchBltMode" prim_setStretchBltMode :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setStretchBltMode_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setStretchBltMode_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setStretchBltMode_gc_failstring :: Addr -> IO (Addr)
getStretchBltMode :: HDC -> IO StretchBltMode
getStretchBltMode arg1 =
  prim_getStretchBltMode arg1
  >>= \ gc_result ->
  access_prim_getStretchBltMode_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getStretchBltMode_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getStretchBltMode_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getStretchBltMode" prim_getStretchBltMode :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getStretchBltMode_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getStretchBltMode_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getStretchBltMode_gc_failstring :: Addr -> IO (Addr)
       
setBkColor :: HDC -> COLORREF -> IO COLORREF
setBkColor arg1 arg2 =
  prim_setBkColor arg1 arg2
  >>= \ gc_result ->
  access_prim_setBkColor_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setBkColor_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setBkColor_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setBkColor" prim_setBkColor :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBkColor_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBkColor_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBkColor_gc_failstring :: Addr -> IO (Addr)
getBkColor :: HDC -> IO COLORREF
getBkColor arg1 =
  prim_getBkColor arg1
  >>= \ gc_result ->
  access_prim_getBkColor_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getBkColor_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getBkColor_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getBkColor" prim_getBkColor :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBkColor_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBkColor_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBkColor_gc_failstring :: Addr -> IO (Addr)

setTextColor :: HDC -> COLORREF -> IO COLORREF
setTextColor arg1 arg2 =
  prim_setTextColor arg1 arg2
  >>= \ gc_result ->
  access_prim_setTextColor_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setTextColor_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setTextColor_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setTextColor" prim_setTextColor :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setTextColor_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setTextColor_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setTextColor_gc_failstring :: Addr -> IO (Addr)
getTextColor :: HDC -> IO COLORREF
getTextColor arg1 =
  prim_getTextColor arg1
  >>= \ gc_result ->
  access_prim_getTextColor_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getTextColor_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getTextColor_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getTextColor" prim_getTextColor :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getTextColor_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getTextColor_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getTextColor_gc_failstring :: Addr -> IO (Addr)
       
setBkMode :: HDC -> BackgroundMode -> IO BackgroundMode
setBkMode arg1 arg2 =
  prim_setBkMode arg1 arg2
  >>= \ gc_result ->
  access_prim_setBkMode_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setBkMode_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setBkMode_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setBkMode" prim_setBkMode :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBkMode_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBkMode_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBkMode_gc_failstring :: Addr -> IO (Addr)
getBkMode :: HDC -> IO BackgroundMode
getBkMode arg1 =
  prim_getBkMode arg1
  >>= \ gc_result ->
  access_prim_getBkMode_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getBkMode_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getBkMode_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getBkMode" prim_getBkMode :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBkMode_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBkMode_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBkMode_gc_failstring :: Addr -> IO (Addr)

setBrushOrgEx :: HDC -> Int -> Int -> IO POINT
setBrushOrgEx arg1 arg2 arg3 =
  prim_setBrushOrgEx arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_setBrushOrgEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_setBrushOrgEx_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_setBrushOrgEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setBrushOrgEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setBrushOrgEx" prim_setBrushOrgEx :: Addr -> Int -> Int -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBrushOrgEx_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBrushOrgEx_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBrushOrgEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setBrushOrgEx_gc_failstring :: Addr -> IO (Addr)

getBrushOrgEx :: HDC -> IO POINT
getBrushOrgEx arg1 =
  prim_getBrushOrgEx arg1
  >>= \ gc_result ->
  access_prim_getBrushOrgEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getBrushOrgEx_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getBrushOrgEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getBrushOrgEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getBrushOrgEx" prim_getBrushOrgEx :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBrushOrgEx_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBrushOrgEx_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBrushOrgEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getBrushOrgEx_gc_failstring :: Addr -> IO (Addr)

setTextAlign :: HDC -> TextAlignment -> IO TextAlignment
setTextAlign arg1 arg2 =
  prim_setTextAlign arg1 arg2
  >>= \ gc_result ->
  access_prim_setTextAlign_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setTextAlign_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setTextAlign_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setTextAlign" prim_setTextAlign :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setTextAlign_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setTextAlign_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setTextAlign_gc_failstring :: Addr -> IO (Addr)
getTextAlign :: HDC -> IO TextAlignment
getTextAlign arg1 =
  prim_getTextAlign arg1
  >>= \ gc_result ->
  access_prim_getTextAlign_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getTextAlign_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getTextAlign_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getTextAlign" prim_getTextAlign :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getTextAlign_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getTextAlign_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getTextAlign_gc_failstring :: Addr -> IO (Addr)

setTextCharacterExtra :: HDC -> Int -> IO Int
setTextCharacterExtra arg1 arg2 =
  prim_setTextCharacterExtra arg1 arg2
  >>= \ gc_result ->
  access_prim_setTextCharacterExtra_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setTextCharacterExtra_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setTextCharacterExtra_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setTextCharacterExtra" prim_setTextCharacterExtra :: Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setTextCharacterExtra_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setTextCharacterExtra_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setTextCharacterExtra_gc_failstring :: Addr -> IO (Addr)
getTextCharacterExtra :: HDC -> IO Int
getTextCharacterExtra arg1 =
  prim_getTextCharacterExtra arg1
  >>= \ gc_result ->
  access_prim_getTextCharacterExtra_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getTextCharacterExtra_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getTextCharacterExtra_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getTextCharacterExtra" prim_getTextCharacterExtra :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getTextCharacterExtra_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getTextCharacterExtra_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getTextCharacterExtra_gc_failstring :: Addr -> IO (Addr)

getMiterLimit :: HDC -> IO Float
getMiterLimit arg1 =
  prim_getMiterLimit arg1
  >>= \ gc_result ->
  access_prim_getMiterLimit_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getMiterLimit_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getMiterLimit_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getMiterLimit" prim_getMiterLimit :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getMiterLimit_res1 :: Addr -> IO (Float)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getMiterLimit_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getMiterLimit_gc_failstring :: Addr -> IO (Addr)

setMiterLimit :: HDC -> Float -> IO Float
setMiterLimit arg1 arg2 =
  prim_setMiterLimit arg1 arg2
  >>= \ gc_result ->
  access_prim_setMiterLimit_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setMiterLimit_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setMiterLimit_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_setMiterLimit" prim_setMiterLimit :: Addr -> Float -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setMiterLimit_res1 :: Addr -> IO (Float)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setMiterLimit_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_setMiterLimit_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------

saveDC :: HDC -> IO Int
saveDC arg1 =
  prim_saveDC arg1
  >>= \ gc_result ->
  access_prim_saveDC_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_saveDC_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_saveDC_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_saveDC" prim_saveDC :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_saveDC_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_saveDC_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_saveDC_gc_failstring :: Addr -> IO (Addr)

restoreDC :: HDC -> Int -> IO ()
restoreDC arg1 arg2 =
  prim_restoreDC arg1 arg2
  >>= \ gc_result ->
  access_prim_restoreDC_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_restoreDC_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_restoreDC" prim_restoreDC :: Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_restoreDC_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_restoreDC_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Macros for getting/selecting objects in HDCs
----------------------------------------------------------------





----------------------------------------------------------------

getCurrentBitmap :: HDC -> IO HBITMAP
getCurrentBitmap arg1 =
  prim_getCurrentBitmap arg1
  >>= \ gc_result ->
  access_prim_getCurrentBitmap_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getCurrentBitmap_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getCurrentBitmap_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getCurrentBitmap" prim_getCurrentBitmap :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentBitmap_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentBitmap_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentBitmap_gc_failstring :: Addr -> IO (Addr)
getCurrentBrush :: HDC -> IO HBRUSH
getCurrentBrush arg1 =
  prim_getCurrentBrush arg1
  >>= \ gc_result ->
  access_prim_getCurrentBrush_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getCurrentBrush_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getCurrentBrush_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getCurrentBrush" prim_getCurrentBrush :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentBrush_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentBrush_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentBrush_gc_failstring :: Addr -> IO (Addr)
getCurrentFont :: HDC -> IO HFONT
getCurrentFont arg1 =
  prim_getCurrentFont arg1
  >>= \ gc_result ->
  access_prim_getCurrentFont_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getCurrentFont_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getCurrentFont_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getCurrentFont" prim_getCurrentFont :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentFont_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentFont_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentFont_gc_failstring :: Addr -> IO (Addr)
getCurrentPalette :: HDC -> IO HPALETTE
getCurrentPalette arg1 =
  prim_getCurrentPalette arg1
  >>= \ gc_result ->
  access_prim_getCurrentPalette_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getCurrentPalette_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getCurrentPalette_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getCurrentPalette" prim_getCurrentPalette :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentPalette_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentPalette_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentPalette_gc_failstring :: Addr -> IO (Addr)
getCurrentPen :: HDC -> IO HPEN
getCurrentPen arg1 =
  prim_getCurrentPen arg1
  >>= \ gc_result ->
  access_prim_getCurrentPen_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getCurrentPen_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getCurrentPen_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_getCurrentPen" prim_getCurrentPen :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentPen_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentPen_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_getCurrentPen_gc_failstring :: Addr -> IO (Addr)

selectBitmap :: HDC -> HBITMAP -> IO HBITMAP
selectBitmap arg1 arg2 =
  prim_selectBitmap arg1 arg2
  >>= \ gc_result ->
  access_prim_selectBitmap_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_selectBitmap_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_selectBitmap_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_selectBitmap" prim_selectBitmap :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectBitmap_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectBitmap_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectBitmap_gc_failstring :: Addr -> IO (Addr)
selectBrush :: HDC -> HBRUSH -> IO HBRUSH
selectBrush arg1 arg2 =
  prim_selectBrush arg1 arg2
  >>= \ gc_result ->
  access_prim_selectBrush_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_selectBrush_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_selectBrush_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_selectBrush" prim_selectBrush :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectBrush_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectBrush_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectBrush_gc_failstring :: Addr -> IO (Addr)
selectFont :: HDC -> HFONT -> IO HFONT
selectFont arg1 arg2 =
  prim_selectFont arg1 arg2
  >>= \ gc_result ->
  access_prim_selectFont_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_selectFont_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_selectFont_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_selectFont" prim_selectFont :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectFont_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectFont_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectFont_gc_failstring :: Addr -> IO (Addr)
selectPen :: HDC -> HPEN -> IO HPEN
selectPen arg1 arg2 =
  prim_selectPen arg1 arg2
  >>= \ gc_result ->
  access_prim_selectPen_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_selectPen_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_selectPen_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_selectPen" prim_selectPen :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectPen_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectPen_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectPen_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
--
----------------------------------------------------------------

selectPalette :: HDC -> HPALETTE -> Bool -> IO HPALETTE
selectPalette arg1 arg2 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_selectPalette arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_selectPalette_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_selectPalette_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_selectPalette_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_selectPalette" prim_selectPalette :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectPalette_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectPalette_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectPalette_gc_failstring :: Addr -> IO (Addr)

selectRgn :: HDC -> HRGN -> IO RegionType
selectRgn arg1 arg2 =
  withForeignPtr arg2 $ \ arg2 ->
  prim_selectRgn arg1 arg2
  >>= \ gc_result ->
  access_prim_selectRgn_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_selectRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_selectRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_selectRgn" prim_selectRgn :: Addr -> Ptr () -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectRgn_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectRgn_gc_failstring :: Addr -> IO (Addr)

selectClipRgn :: HDC -> MbHRGN -> IO RegionType
selectClipRgn arg1 arg2 =
  (case arg2 of {
      Nothing -> (return (nullFinalHANDLE));
      (Just arg2) -> (return ((arg2)))
   }) >>= \ (arg2) ->
  withForeignPtr arg2 $ \ arg2 ->
  prim_selectClipRgn arg1 arg2
  >>= \ gc_result ->
  access_prim_selectClipRgn_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_selectClipRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_selectClipRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_selectClipRgn" prim_selectClipRgn :: Addr -> Ptr () -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectClipRgn_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectClipRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectClipRgn_gc_failstring :: Addr -> IO (Addr)

extSelectClipRgn :: HDC -> MbHRGN -> ClippingMode -> IO RegionType
extSelectClipRgn arg1 arg2 arg3 =
  (case arg2 of {
      Nothing -> (return (nullFinalHANDLE));
      (Just arg2) -> (return ((arg2)))
   }) >>= \ (arg2) ->
  withForeignPtr arg2 $ \ arg2 ->
  prim_extSelectClipRgn arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_extSelectClipRgn_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_extSelectClipRgn_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_extSelectClipRgn_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_extSelectClipRgn" prim_extSelectClipRgn :: Addr -> Ptr () -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_extSelectClipRgn_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_extSelectClipRgn_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_extSelectClipRgn_gc_failstring :: Addr -> IO (Addr)

selectClipPath :: HDC -> ClippingMode -> IO RegionType
selectClipPath arg1 arg2 =
  prim_selectClipPath arg1 arg2
  >>= \ gc_result ->
  access_prim_selectClipPath_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_selectClipPath_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_selectClipPath_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_selectClipPath" prim_selectClipPath :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectClipPath_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectClipPath_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_selectClipPath_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Misc
----------------------------------------------------------------

cancelDC :: HDC -> IO ()
cancelDC arg1 =
  prim_cancelDC arg1
  >>= \ gc_result ->
  access_prim_cancelDC_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_cancelDC_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_cancelDC" prim_cancelDC :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_cancelDC_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_cancelDC_gc_failstring :: Addr -> IO (Addr)

createCompatibleDC :: MbHDC -> IO HDC
createCompatibleDC arg1 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_createCompatibleDC arg1
  >>= \ gc_result ->
  access_prim_createCompatibleDC_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createCompatibleDC_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createCompatibleDC_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_createCompatibleDC" prim_createCompatibleDC :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_createCompatibleDC_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_createCompatibleDC_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_createCompatibleDC_gc_failstring :: Addr -> IO (Addr)

deleteDC :: HDC -> IO ()
deleteDC arg1 =
  prim_deleteDC arg1
  >>= \ gc_result ->
  access_prim_deleteDC_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_deleteDC_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32HDC_stub_ffi.h prim_deleteDC" prim_deleteDC :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_deleteDC_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32HDC_stub_ffi.h" access_prim_deleteDC_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
