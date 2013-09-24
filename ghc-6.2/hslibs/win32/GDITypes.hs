module GDITypes
{-  -- still incomplete
	( POINT,
	, RECT,
	, SIZE,
	, nullAddr
	, HBITMAP	, MbHBITMAP
	, HFONT		, MbHFONT
	, HCURSOR	, MbHCURSOR
	, HICON		, MbHICON
	, HRGN		, MbHRGN
	, HPALETTE	, MbHPALETTE
	, HBRUSH	, MbHBRUSH
	, HPEN		, MbHPEN
	, HACCEL	--, MbHACCEL
	, HDC		, MbHDC
	, HDWP          , MbHDWP
	, HWND		, MbHWND
	, HMENU		, MbHMENU
	, PolyFillMode
	, ArcDirection
	, MbArcDirection
	, GraphicsMode
	, MbGraphicsMode
	, BackgroundMode
	, HatchStyle
	, StretchBltMode
	, COLORREF
	, TextAlignment
	, ClippingMode
	, RegionType
	)
-}
	where

import StdDIS ( unmarshall_string_ )
import Win32Types
import Monad( zipWithM_ )
import IOExts
import Foreign
import Int
import Word
import Addr ( Addr )
import Foreign.ForeignPtr

----------------------------------------------------------------
--
----------------------------------------------------------------

type POINT =
  ( LONG  -- x
  , LONG  -- y
  )
type RECT =
  ( LONG  -- left
  , LONG  -- top
  , LONG  -- right
  , LONG  -- bottom
  )
type SIZE =
  ( LONG  -- cx
  , LONG  -- cy
  )


----------------------------------------------------------------
marshall_listLenPOINT_ :: [POINT] -> IO (Addr, Int)
marshall_listLenPOINT_ cs = do
  let l = length cs
  ps <- mallocPOINTs l
  zipWithM_ (setPOINT ps) [0..] cs
  return (ps,l)

apiError :: String -> IO a
apiError loc = ioError (userError (loc ++ ": failed"))

mallocPOINTs :: Int -> IO Addr
mallocPOINTs arg1 =
  prim_mallocPOINTs arg1
  >>= \ gc_result ->
  access_prim_mallocPOINTs_ps (gc_result :: Addr) >>= \ ps ->
  access_prim_mallocPOINTs_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_mallocPOINTs_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (ps))

foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_mallocPOINTs" prim_mallocPOINTs :: Int -> IO (Addr)
foreign import ccall unsafe "GDITypes_stub_ffi.h" access_prim_mallocPOINTs_ps :: Addr -> IO (Addr)
foreign import ccall unsafe "GDITypes_stub_ffi.h" access_prim_mallocPOINTs_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "GDITypes_stub_ffi.h" access_prim_mallocPOINTs_gc_failstring :: Addr -> IO (Addr)

setPOINT :: Addr -> Int -> POINT -> IO ()
setPOINT ps i gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  prim_setPOINT ps i gc_arg2 gc_arg3}
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_setPOINT" prim_setPOINT :: Addr -> Int -> Int32 -> Int32 -> IO ()

type   LPRECT   = Addr
type MbLPRECT   = Maybe LPRECT

getRECT :: LPRECT -> IO RECT
getRECT r =
  prim_getRECT r
  >>= \ gc_result ->
  access_prim_getRECT_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getRECT_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getRECT_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_getRECT_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  (return ((gc_res1,gc_res2,gc_res3,gc_res4)))
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_getRECT" prim_getRECT :: Addr -> IO (Addr)
foreign import ccall unsafe "GDITypes_stub_ffi.h" access_prim_getRECT_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "GDITypes_stub_ffi.h" access_prim_getRECT_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "GDITypes_stub_ffi.h" access_prim_getRECT_gc_res3 :: Addr -> IO (Int32)
foreign import ccall unsafe "GDITypes_stub_ffi.h" access_prim_getRECT_gc_res4 :: Addr -> IO (Int32)

----------------------------------------------------------------
-- (GDI related) Handles
----------------------------------------------------------------

type   HBITMAP    = Addr
type MbHBITMAP    = Maybe HBITMAP

type   HFONT      = Addr
type MbHFONT      = Maybe HFONT

type   HCURSOR    = Addr
type MbHCURSOR    = Maybe HCURSOR

type   HICON      = Addr
type MbHICON      = Maybe HICON


-- This is not the only handle / resource that should be
-- finalised for you, but it's a start.
-- ToDo.

type   HRGN       = ForeignPtr ()
type MbHRGN       = Maybe HRGN

type   HPALETTE   = Addr
type MbHPALETTE   = Maybe HPALETTE

type   HBRUSH     = Addr
type MbHBRUSH     = Maybe HBRUSH

type   HPEN       = Addr
type MbHPEN       = Maybe HPEN

type   HACCEL     = Addr

type   HDC        = Addr
type MbHDC        = Maybe HDC

type   HDWP        = Addr
type MbHDWP        = Maybe HDWP

type   HWND       = Addr
type MbHWND       = Maybe HWND

foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hWND_BOTTOM" hWND_BOTTOM :: HWND
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hWND_NOTOPMOST" hWND_NOTOPMOST :: HWND
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hWND_TOP" hWND_TOP :: HWND
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hWND_TOPMOST" hWND_TOPMOST :: HWND

type   HMENU      = Addr
type MbHMENU      = Maybe HMENU

----------------------------------------------------------------
-- COLORREF
----------------------------------------------------------------

type COLORREF   = DWORD

foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_rgb" rgb :: BYTE -> BYTE -> BYTE -> COLORREF
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_getRValue" getRValue :: COLORREF -> BYTE
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_getGValue" getGValue :: COLORREF -> BYTE
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_getBValue" getBValue :: COLORREF -> BYTE

----------------------------------------------------------------
-- Miscellaneous enumerations
----------------------------------------------------------------

type PolyFillMode   = WORD
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_aLTERNATE" aLTERNATE :: PolyFillMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_wINDING"   wINDING   :: PolyFillMode

----------------------------------------------------------------

type ArcDirection = WORD
type MbArcDirection = Maybe ArcDirection
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_aD_COUNTERCLOCKWISE" aD_COUNTERCLOCKWISE :: ArcDirection
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_aD_CLOCKWISE" aD_CLOCKWISE :: ArcDirection

----------------------------------------------------------------

type GraphicsMode   = DWORD
type MbGraphicsMode = Maybe GraphicsMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_gM_COMPATIBLE" gM_COMPATIBLE :: GraphicsMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_gM_ADVANCED" gM_ADVANCED :: GraphicsMode

----------------------------------------------------------------

type BackgroundMode   = UINT
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_tRANSPARENT" tRANSPARENT :: BackgroundMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_oPAQUE" oPAQUE :: BackgroundMode

----------------------------------------------------------------

type HatchStyle   = WORD

foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hS_HORIZONTAL" hS_HORIZONTAL :: HatchStyle
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hS_VERTICAL"  hS_VERTICAL :: HatchStyle
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hS_FDIAGONAL" hS_FDIAGONAL :: HatchStyle
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hS_BDIAGONAL" hS_BDIAGONAL :: HatchStyle
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hS_CROSS"     hS_CROSS :: HatchStyle
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hS_DIAGCROSS" hS_DIAGCROSS :: HatchStyle

----------------------------------------------------------------

type StretchBltMode   = UINT

foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_bLACKONWHITE" bLACKONWHITE :: StretchBltMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_wHITEONBLACK" wHITEONBLACK :: StretchBltMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_cOLORONCOLOR" cOLORONCOLOR :: StretchBltMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_hALFTONE" hALFTONE :: StretchBltMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_sTRETCH_ANDSCANS" sTRETCH_ANDSCANS :: StretchBltMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_sTRETCH_ORSCANS" sTRETCH_ORSCANS :: StretchBltMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_sTRETCH_DELETESCANS" sTRETCH_DELETESCANS :: StretchBltMode

----------------------------------------------------------------

type TextAlignment   = UINT

foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_tA_NOUPDATECP" tA_NOUPDATECP :: TextAlignment
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_tA_UPDATECP" tA_UPDATECP :: TextAlignment
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_tA_LEFT"     tA_LEFT     :: TextAlignment
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_tA_RIGHT"    tA_RIGHT    :: TextAlignment
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_tA_CENTER"   tA_CENTER   :: TextAlignment
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_tA_TOP"      tA_TOP      :: TextAlignment
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_tA_BOTTOM"   tA_BOTTOM   :: TextAlignment
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_tA_BASELINE" tA_BASELINE :: TextAlignment

----------------------------------------------------------------

type ClippingMode   = UINT
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_rGN_AND"  rGN_AND  :: ClippingMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_rGN_OR"   rGN_OR   :: ClippingMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_rGN_XOR"  rGN_XOR  :: ClippingMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_rGN_DIFF" rGN_DIFF :: ClippingMode
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_rGN_COPY" rGN_COPY :: ClippingMode

----------------------------------------------------------------

type RegionType   = WORD
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_eRROR"         eRROR :: RegionType
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_nULLREGION"    nULLREGION  :: RegionType
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_sIMPLEREGION"  sIMPLEREGION  :: RegionType
foreign import  ccall unsafe "GDITypes_stub_ffi.h prim_cOMPLEXREGION" cOMPLEXREGION :: RegionType

----------------------------------------------------------------
-- End
----------------------------------------------------------------
