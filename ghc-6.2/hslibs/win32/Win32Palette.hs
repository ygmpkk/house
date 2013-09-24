module Win32Palette where

import StdDIS
import Win32Types
import GDITypes


----------------------------------------------------------------
-- Palettes
----------------------------------------------------------------

type StockPalette   = WORD

foreign import  ccall unsafe "Win32Palette_stub_ffi.h prim_dEFAULT_PALETTE" dEFAULT_PALETTE :: StockPalette

getStockPalette :: StockPalette -> IO HPALETTE
getStockPalette arg1 =
  prim_getStockPalette arg1
  >>= \ gc_result ->
  access_prim_getStockPalette_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getStockPalette_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getStockPalette_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Palette_stub_ffi.h prim_getStockPalette" prim_getStockPalette :: Word16 -> IO (Addr)
foreign import ccall unsafe "Win32Palette_stub_ffi.h" access_prim_getStockPalette_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Palette_stub_ffi.h" access_prim_getStockPalette_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Palette_stub_ffi.h" access_prim_getStockPalette_gc_failstring :: Addr -> IO (Addr)

deletePalette :: HPALETTE -> IO ()
deletePalette arg1 =
  prim_deletePalette arg1
  >>= \ gc_result ->
  access_prim_deletePalette_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_deletePalette_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Palette_stub_ffi.h prim_deletePalette" prim_deletePalette :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Palette_stub_ffi.h" access_prim_deletePalette_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Palette_stub_ffi.h" access_prim_deletePalette_gc_failstring :: Addr -> IO (Addr)

foreign import  ccall unsafe "Win32Palette_stub_ffi.h prim_pALETTERGB" pALETTERGB :: BYTE -> BYTE -> BYTE -> COLORREF
foreign import  ccall unsafe "Win32Palette_stub_ffi.h prim_pALETTEINDEX" pALETTEINDEX :: WORD -> COLORREF

----------------------------------------------------------------
-- End
----------------------------------------------------------------
