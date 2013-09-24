module Win32Graphics2D
	where

import StdDIS
import Win32Types
import GDITypes
import Win32Bitmap


----------------------------------------------------------------
-- 2D graphics operations
----------------------------------------------------------------

moveToEx :: HDC -> Int32 -> Int32 -> IO POINT
moveToEx arg1 arg2 arg3 =
  prim_moveToEx arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_moveToEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_moveToEx_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_moveToEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_moveToEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_moveToEx" prim_moveToEx :: Addr -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_moveToEx_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_moveToEx_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_moveToEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_moveToEx_gc_failstring :: Addr -> IO (Addr)

lineTo :: HDC -> Int32 -> Int32 -> IO ()
lineTo arg1 arg2 arg3 =
  prim_lineTo arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_lineTo_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_lineTo_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_lineTo" prim_lineTo :: Addr -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_lineTo_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_lineTo_gc_failstring :: Addr -> IO (Addr)

polyline :: HDC -> [POINT] -> IO ()
polyline hdc gc_arg1 =
  (marshall_listLenPOINT_ gc_arg1) >>= \ (gc_arg2) ->
  case gc_arg2 of { (ps,num_ps) ->
  prim_polyline hdc ps num_ps
  >>= \ gc_result ->
  access_prim_polyline_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_polyline_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_polyline" prim_polyline :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polyline_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polyline_gc_failstring :: Addr -> IO (Addr)

polylineTo :: HDC -> [POINT] -> IO ()
polylineTo hdc gc_arg1 =
  (marshall_listLenPOINT_ gc_arg1) >>= \ (gc_arg2) ->
  case gc_arg2 of { (ps,num_ps) ->
  prim_polylineTo hdc ps num_ps
  >>= \ gc_result ->
  access_prim_polylineTo_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_polylineTo_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_polylineTo" prim_polylineTo :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polylineTo_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polylineTo_gc_failstring :: Addr -> IO (Addr)

polygon :: HDC -> [POINT] -> IO ()
polygon hdc gc_arg1 =
  (marshall_listLenPOINT_ gc_arg1) >>= \ (gc_arg2) ->
  case gc_arg2 of { (ps,num_ps) ->
  prim_polygon hdc ps num_ps
  >>= \ gc_result ->
  access_prim_polygon_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_polygon_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_polygon" prim_polygon :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polygon_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polygon_gc_failstring :: Addr -> IO (Addr)

polyBezier :: HDC -> [POINT] -> IO ()
polyBezier hdc gc_arg1 =
  (marshall_listLenPOINT_ gc_arg1) >>= \ (gc_arg2) ->
  case gc_arg2 of { (ps,num_ps) ->
  prim_polyBezier hdc ps num_ps
  >>= \ gc_result ->
  access_prim_polyBezier_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_polyBezier_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_polyBezier" prim_polyBezier :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polyBezier_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polyBezier_gc_failstring :: Addr -> IO (Addr)

polyBezierTo :: HDC -> [POINT] -> IO ()
polyBezierTo hdc gc_arg1 =
  (marshall_listLenPOINT_ gc_arg1) >>= \ (gc_arg2) ->
  case gc_arg2 of { (ps,num_ps) ->
  prim_polyBezierTo hdc ps num_ps
  >>= \ gc_result ->
  access_prim_polyBezierTo_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_polyBezierTo_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_polyBezierTo" prim_polyBezierTo :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polyBezierTo_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_polyBezierTo_gc_failstring :: Addr -> IO (Addr)

-- ToDo: We ought to be able to specify a colour instead of the
-- Brush by adding 1 to colour number.

fillRect :: HDC -> RECT -> HBRUSH -> IO ()
fillRect arg1 gc_arg1 arg3 =
  case gc_arg1 of { (gc_arg2,gc_arg3,gc_arg4,gc_arg5) ->
  prim_fillRect arg1 gc_arg2 gc_arg3 gc_arg4 gc_arg5 arg3
  >>= \ gc_result ->
  access_prim_fillRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_fillRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_fillRect" prim_fillRect :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_fillRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_fillRect_gc_failstring :: Addr -> IO (Addr)

frameRect :: HDC -> RECT -> HBRUSH -> IO ()
frameRect arg1 gc_arg1 arg3 =
  case gc_arg1 of { (gc_arg2,gc_arg3,gc_arg4,gc_arg5) ->
  prim_frameRect arg1 gc_arg2 gc_arg3 gc_arg4 gc_arg5 arg3
  >>= \ gc_result ->
  access_prim_frameRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_frameRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_frameRect" prim_frameRect :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_frameRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_frameRect_gc_failstring :: Addr -> IO (Addr)

invertRect :: HDC -> RECT -> IO ()
invertRect arg1 gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3,gc_arg4,gc_arg5) ->
  prim_invertRect arg1 gc_arg2 gc_arg3 gc_arg4 gc_arg5
  >>= \ gc_result ->
  access_prim_invertRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_invertRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_invertRect" prim_invertRect :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_invertRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_invertRect_gc_failstring :: Addr -> IO (Addr)

rectangle :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
rectangle arg1 arg2 arg3 arg4 arg5 =
  prim_rectangle arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_rectangle_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_rectangle_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_rectangle" prim_rectangle :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_rectangle_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_rectangle_gc_failstring :: Addr -> IO (Addr)

roundRect :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
roundRect arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
  prim_roundRect arg1 arg2 arg3 arg4 arg5 arg6 arg7
  >>= \ gc_result ->
  access_prim_roundRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_roundRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_roundRect" prim_roundRect :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_roundRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_roundRect_gc_failstring :: Addr -> IO (Addr)

ellipse :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
ellipse arg1 arg2 arg3 arg4 arg5 =
  prim_ellipse arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_ellipse_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_ellipse_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_ellipse" prim_ellipse :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_ellipse_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_ellipse_gc_failstring :: Addr -> IO (Addr)

arc :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
arc arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 =
  prim_arc arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
  >>= \ gc_result ->
  access_prim_arc_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_arc_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_arc" prim_arc :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_arc_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_arc_gc_failstring :: Addr -> IO (Addr)

arcTo :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
arcTo arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 =
  prim_arcTo arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
  >>= \ gc_result ->
  access_prim_arcTo_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_arcTo_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_arcTo" prim_arcTo :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_arcTo_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_arcTo_gc_failstring :: Addr -> IO (Addr)

angleArc :: HDC -> Int32 -> Int32 -> WORD -> Double -> Double -> IO ()
angleArc arg1 arg2 arg3 arg4 arg5 arg6 =
  prim_angleArc arg1 arg2 arg3 arg4 arg5 arg6
  >>= \ gc_result ->
  access_prim_angleArc_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_angleArc_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_angleArc" prim_angleArc :: Addr -> Int32 -> Int32 -> Word16 -> Double -> Double -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_angleArc_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_angleArc_gc_failstring :: Addr -> IO (Addr)

chord :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
chord arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 =
  prim_chord arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
  >>= \ gc_result ->
  access_prim_chord_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_chord_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_chord" prim_chord :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_chord_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_chord_gc_failstring :: Addr -> IO (Addr)

pie :: HDC -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
pie arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 =
  prim_pie arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
  >>= \ gc_result ->
  access_prim_pie_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_pie_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_pie" prim_pie :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_pie_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_pie_gc_failstring :: Addr -> IO (Addr)

bitBlt :: HDC -> INT -> INT -> INT -> INT -> HDC -> INT -> INT -> RasterOp3 -> IO ()
bitBlt arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 =
  prim_bitBlt arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
  >>= \ gc_result ->
  access_prim_bitBlt_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_bitBlt_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_bitBlt" prim_bitBlt :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Addr -> Int32 -> Int32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_bitBlt_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_bitBlt_gc_failstring :: Addr -> IO (Addr)

maskBlt :: HDC -> INT -> INT -> INT -> INT -> HDC -> INT -> INT -> HBITMAP -> INT -> INT -> RasterOp4 -> IO ()
maskBlt arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 =
  prim_maskBlt arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12
  >>= \ gc_result ->
  access_prim_maskBlt_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_maskBlt_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_maskBlt" prim_maskBlt :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Addr -> Int32 -> Int32 -> Addr -> Int32 -> Int32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_maskBlt_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_maskBlt_gc_failstring :: Addr -> IO (Addr)

stretchBlt :: HDC -> INT -> INT -> INT -> INT -> HDC -> INT -> INT -> INT -> INT -> RasterOp3 -> IO ()
stretchBlt arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 =
  prim_stretchBlt arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11
  >>= \ gc_result ->
  access_prim_stretchBlt_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_stretchBlt_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_stretchBlt" prim_stretchBlt :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_stretchBlt_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_stretchBlt_gc_failstring :: Addr -> IO (Addr)

-- We deviate slightly from the Win32 interface

-- %C typedef POINT ThreePts[3];

-- Old 2nd line: 
-- %start POINT vertices[3];

plgBlt :: HDC -> POINT -> POINT -> POINT -> HDC -> INT -> INT -> INT -> INT -> MbHBITMAP -> INT -> INT -> IO ()
plgBlt arg1 gc_arg1 gc_arg5 gc_arg8 arg2 x y w h bm sx sy =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  case gc_arg5 of { (gc_arg6,gc_arg7) ->
  case gc_arg8 of { (gc_arg9,gc_arg10) ->
  (case bm of {
      Nothing -> (return (nullHANDLE));
      (Just bm) -> (return ((bm)))
   }) >>= \ (bm) ->
  prim_plgBlt arg1 gc_arg2 gc_arg3 gc_arg6 gc_arg7 gc_arg9 gc_arg10 arg2 x y w h bm sx sy
  >>= \ gc_result ->
  access_prim_plgBlt_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_plgBlt_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))}}}
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_plgBlt" prim_plgBlt :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Addr -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_plgBlt_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_plgBlt_gc_failstring :: Addr -> IO (Addr)

textOut :: HDC -> INT -> INT -> String -> IO ()
textOut arg1 arg2 arg3 gc_arg1 =
  (marshall_stringLen_ gc_arg1) >>= \ (arg4,arg5) ->
  prim_textOut arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_textOut_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_textOut_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_textOut" prim_textOut :: Addr -> Int32 -> Int32 -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_textOut_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_textOut_gc_failstring :: Addr -> IO (Addr)

-- missing TabbedTextOut from WinFonts.ss; GSL ???

getTextExtentPoint32 :: HDC -> String -> IO SIZE
getTextExtentPoint32 hdc gc_arg1 =
  (marshall_stringLen_ gc_arg1) >>= \ (str,l) ->
  prim_getTextExtentPoint32 hdc str l
  >>= \ gc_result ->
  access_prim_getTextExtentPoint32_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getTextExtentPoint32_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getTextExtentPoint32_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getTextExtentPoint32_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))
foreign import  ccall unsafe "Win32Graphics2D_stub_ffi.h prim_getTextExtentPoint32" prim_getTextExtentPoint32 :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_getTextExtentPoint32_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_getTextExtentPoint32_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_getTextExtentPoint32_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Graphics2D_stub_ffi.h" access_prim_getTextExtentPoint32_gc_failstring :: Addr -> IO (Addr)

-- missing getTabbedTextExtent from WinFonts.ss; GSL ???
-- missing SetTextJustification from WinFonts.ss; GSL ???
-- missing a whole family of techandfamily functionality; GSL ???
-- missing DrawText and DrawTextFormat in WinFonts.ss; GSL ???

----------------------------------------------------------------
-- End
----------------------------------------------------------------
