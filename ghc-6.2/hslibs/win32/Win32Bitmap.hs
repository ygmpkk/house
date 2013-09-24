module Win32Bitmap where

import StdDIS
import Win32Types
import GDITypes


----------------------------------------------------------------
-- Resources
----------------------------------------------------------------

-- Yoiks - name clash
-- %dis bitmap x = addr ({LPTSTR} x)
-- 
-- type Bitmap = LPCTSTR
-- 
-- intToBitmap :: Int -> Bitmap
-- intToBitmap i = makeIntResource (toWord i)
-- 
-- %fun LoadBitmap :: MbHINSTANCE -> Bitmap -> IO HBITMAP
-- %fail { res1 == 0 } { ErrorString("LoadBitmap") }
--  
-- %const Bitmap  
-- % [ OBM_CLOSE        = { MAKEINTRESOURCE(OBM_CLOSE)       }
-- % , OBM_UPARROW      = { MAKEINTRESOURCE(OBM_UPARROW)     }
-- % , OBM_DNARROW      = { MAKEINTRESOURCE(OBM_DNARROW)     }
-- % , OBM_RGARROW      = { MAKEINTRESOURCE(OBM_RGARROW)     }
-- % , OBM_LFARROW      = { MAKEINTRESOURCE(OBM_LFARROW)     }
-- % , OBM_REDUCE       = { MAKEINTRESOURCE(OBM_REDUCE)      }
-- % , OBM_ZOOM         = { MAKEINTRESOURCE(OBM_ZOOM)        }
-- % , OBM_RESTORE      = { MAKEINTRESOURCE(OBM_RESTORE)     }
-- % , OBM_REDUCED      = { MAKEINTRESOURCE(OBM_REDUCED)     }
-- % , OBM_ZOOMD        = { MAKEINTRESOURCE(OBM_ZOOMD)       }
-- % , OBM_RESTORED     = { MAKEINTRESOURCE(OBM_RESTORED)    }
-- % , OBM_UPARROWD     = { MAKEINTRESOURCE(OBM_UPARROWD)    }
-- % , OBM_DNARROWD     = { MAKEINTRESOURCE(OBM_DNARROWD)    }
-- % , OBM_RGARROWD     = { MAKEINTRESOURCE(OBM_RGARROWD)    }
-- % , OBM_LFARROWD     = { MAKEINTRESOURCE(OBM_LFARROWD)    }
-- % , OBM_MNARROW      = { MAKEINTRESOURCE(OBM_MNARROW)     }
-- % , OBM_COMBO        = { MAKEINTRESOURCE(OBM_COMBO)       }
-- % , OBM_UPARROWI     = { MAKEINTRESOURCE(OBM_UPARROWI)    }
-- % , OBM_DNARROWI     = { MAKEINTRESOURCE(OBM_DNARROWI)    }
-- % , OBM_RGARROWI     = { MAKEINTRESOURCE(OBM_RGARROWI)    }
-- % , OBM_LFARROWI     = { MAKEINTRESOURCE(OBM_LFARROWI)    }
-- % , OBM_OLD_CLOSE    = { MAKEINTRESOURCE(OBM_OLD_CLOSE)   }   
-- % , OBM_SIZE         = { MAKEINTRESOURCE(OBM_SIZE)        }
-- % , OBM_OLD_UPARROW  = { MAKEINTRESOURCE(OBM_OLD_UPARROW) }   
-- % , OBM_OLD_DNARROW  = { MAKEINTRESOURCE(OBM_OLD_DNARROW) }   
-- % , OBM_OLD_RGARROW  = { MAKEINTRESOURCE(OBM_OLD_RGARROW) }   
-- % , OBM_OLD_LFARROW  = { MAKEINTRESOURCE(OBM_OLD_LFARROW) }   
-- % , OBM_BTSIZE       = { MAKEINTRESOURCE(OBM_BTSIZE)      }
-- % , OBM_CHECK        = { MAKEINTRESOURCE(OBM_CHECK)       }
-- % , OBM_CHECKBOXES   = { MAKEINTRESOURCE(OBM_CHECKBOXES)  }   
-- % , OBM_BTNCORNERS   = { MAKEINTRESOURCE(OBM_BTNCORNERS)  }   
-- % , OBM_OLD_REDUCE   = { MAKEINTRESOURCE(OBM_OLD_REDUCE)  }   
-- % , OBM_OLD_ZOOM     = { MAKEINTRESOURCE(OBM_OLD_ZOOM)    }
-- % , OBM_OLD_RESTORE  = { MAKEINTRESOURCE(OBM_OLD_RESTORE) }   
-- % ]

----------------------------------------------------------------
-- Raster Ops
----------------------------------------------------------------


type RasterOp3 = Word32
type RasterOp4 = Word32

foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sRCCOPY"     sRCCOPY :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sRCPAINT"    sRCPAINT :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sRCAND"      sRCAND :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sRCINVERT"   sRCINVERT :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sRCERASE"    sRCERASE :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_nOTSRCCOPY"  nOTSRCCOPY :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_nOTSRCERASE" nOTSRCERASE :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_mERGECOPY"  mERGECOPY :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_mERGEPAINT" mERGEPAINT :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_pATCOPY"   pATCOPY   :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_pATPAINT"  pATPAINT  :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_pATINVERT" pATINVERT :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_dSTINVERT" dSTINVERT :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_bLACKNESS" bLACKNESS :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_wHITENESS" wHITENESS :: RasterOp3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_mAKEROP4" mAKEROP4 :: RasterOp3 -> RasterOp3 -> RasterOp4

----------------------------------------------------------------
-- BITMAP
----------------------------------------------------------------

type BITMAP =
  ( LONG    -- bmType
  , LONG    -- bmWidth
  , LONG    -- bmHeight
  , LONG    -- bmWidthBytes
  , WORD    -- bmPlanes   
  , WORD    -- bmBitsPixel
  , LPVOID  -- bmBits
  )


type LPBITMAP = Addr

setBITMAP :: LPBITMAP -> BITMAP -> IO ()
setBITMAP arg1 gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3,gc_arg4,gc_arg5,gc_arg6,gc_arg7,gc_arg8) ->
  prim_setBITMAP arg1 gc_arg2 gc_arg3 gc_arg4 gc_arg5 gc_arg6 gc_arg7 gc_arg8}
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_setBITMAP" prim_setBITMAP :: Addr -> Int32 -> Int32 -> Int32 -> Int32 -> Word16 -> Word16 -> Addr -> IO ()

marshall_bITMAP_ :: BITMAP -> IO LPBITMAP
marshall_bITMAP_ bmp = do
  lpbmp <- malloc sizeofBITMAP
  setBITMAP lpbmp bmp
  return lpbmp

----------------------------------------------------------------
-- Misc
----------------------------------------------------------------

deleteBitmap :: HBITMAP -> IO ()
deleteBitmap arg1 =
  prim_deleteBitmap arg1
  >>= \ gc_result ->
  access_prim_deleteBitmap_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_deleteBitmap_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_deleteBitmap" prim_deleteBitmap :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_deleteBitmap_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_deleteBitmap_gc_failstring :: Addr -> IO (Addr)

createCompatibleBitmap :: HDC -> Int32 -> Int32 -> IO HBITMAP
createCompatibleBitmap arg1 arg2 arg3 =
  prim_createCompatibleBitmap arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_createCompatibleBitmap_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createCompatibleBitmap_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createCompatibleBitmap_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_createCompatibleBitmap" prim_createCompatibleBitmap :: Addr -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createCompatibleBitmap_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createCompatibleBitmap_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createCompatibleBitmap_gc_failstring :: Addr -> IO (Addr)

createBitmap :: INT -> INT -> UINT -> UINT -> MbLPVOID -> IO HBITMAP
createBitmap arg1 arg2 arg3 arg4 arg5 =
  (case arg5 of {
      Nothing -> (return (nullAddr));
      (Just arg5) -> (return ((arg5)))
   }) >>= \ (arg5) ->
  prim_createBitmap arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_createBitmap_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createBitmap_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createBitmap_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_createBitmap" prim_createBitmap :: Int32 -> Int32 -> Word32 -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createBitmap_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createBitmap_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createBitmap_gc_failstring :: Addr -> IO (Addr)

createBitmapIndirect :: LPBITMAP -> IO HBITMAP
createBitmapIndirect arg1 =
  prim_createBitmapIndirect arg1
  >>= \ gc_result ->
  access_prim_createBitmapIndirect_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createBitmapIndirect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createBitmapIndirect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_createBitmapIndirect" prim_createBitmapIndirect :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createBitmapIndirect_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createBitmapIndirect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createBitmapIndirect_gc_failstring :: Addr -> IO (Addr)

createDIBPatternBrushPt :: LPVOID -> ColorFormat -> IO HBRUSH
createDIBPatternBrushPt arg1 arg2 =
  prim_createDIBPatternBrushPt arg1 arg2
  >>= \ gc_result ->
  access_prim_createDIBPatternBrushPt_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createDIBPatternBrushPt_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createDIBPatternBrushPt_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_createDIBPatternBrushPt" prim_createDIBPatternBrushPt :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createDIBPatternBrushPt_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createDIBPatternBrushPt_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createDIBPatternBrushPt_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Querying
----------------------------------------------------------------

getBitmapDimensionEx :: HBITMAP -> IO SIZE
getBitmapDimensionEx h =
  prim_getBitmapDimensionEx h
  >>= \ gc_result ->
  access_prim_getBitmapDimensionEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getBitmapDimensionEx_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getBitmapDimensionEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getBitmapDimensionEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_getBitmapDimensionEx" prim_getBitmapDimensionEx :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapDimensionEx_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapDimensionEx_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapDimensionEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapDimensionEx_gc_failstring :: Addr -> IO (Addr)

setBitmapDimensionEx :: HBITMAP -> SIZE -> IO SIZE
setBitmapDimensionEx h gc_arg1 =
  case gc_arg1 of { (gc_arg2,gc_arg3) ->
  prim_setBitmapDimensionEx h gc_arg2 gc_arg3
  >>= \ gc_result ->
  access_prim_setBitmapDimensionEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_setBitmapDimensionEx_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_setBitmapDimensionEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setBitmapDimensionEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2)))}
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_setBitmapDimensionEx" prim_setBitmapDimensionEx :: Addr -> Int32 -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_setBitmapDimensionEx_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_setBitmapDimensionEx_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_setBitmapDimensionEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_setBitmapDimensionEx_gc_failstring :: Addr -> IO (Addr)

getBitmapInfo :: HBITMAP -> IO BITMAP
getBitmapInfo x =
  prim_getBitmapInfo x
  >>= \ gc_result ->
  access_prim_getBitmapInfo_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getBitmapInfo_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getBitmapInfo_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_getBitmapInfo_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_getBitmapInfo_gc_res5 (gc_result :: Addr) >>= \ gc_res5 ->
  access_prim_getBitmapInfo_gc_res6 (gc_result :: Addr) >>= \ gc_res6 ->
  access_prim_getBitmapInfo_gc_res7 (gc_result :: Addr) >>= \ gc_res7 ->
  access_prim_getBitmapInfo_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getBitmapInfo_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((gc_res1,gc_res2,gc_res3,gc_res4,gc_res5,gc_res6,gc_res7)))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_getBitmapInfo" prim_getBitmapInfo :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapInfo_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapInfo_gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapInfo_gc_res3 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapInfo_gc_res4 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapInfo_gc_res5 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapInfo_gc_res6 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapInfo_gc_res7 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapInfo_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBitmapInfo_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
--
----------------------------------------------------------------

type BitmapCompression = WORD

foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_bI_RGB"  bI_RGB  :: BitmapCompression
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_bI_RLE8" bI_RLE8 :: BitmapCompression
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_bI_RLE4" bI_RLE4 :: BitmapCompression
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_bI_BITFIELDS" bI_BITFIELDS :: BitmapCompression

type ColorFormat = DWORD

foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_dIB_PAL_COLORS" dIB_PAL_COLORS :: ColorFormat
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_dIB_RGB_COLORS" dIB_RGB_COLORS :: ColorFormat

----------------------------------------------------------------
-- BITMAPINFO
----------------------------------------------------------------

type LPBITMAPINFO = Addr

----------------------------------------------------------------
-- BITMAPINFOHEADER
----------------------------------------------------------------

type BITMAPINFOHEADER =
 ( DWORD              -- biSize      -- sizeof(BITMAPINFOHEADER)
 , LONG               -- biWidth
 , LONG               -- biHeight
 , WORD               -- biPlanes
 , WORD               -- biBitCount  -- 1, 4, 8, 16, 24 or 32
 , BitmapCompression  -- biCompression
 , DWORD              -- biSizeImage
 , LONG               -- biXPelsPerMeter
 , LONG               -- biYPelsPerMeter
 , Maybe DWORD        -- biClrUsed
 , Maybe DWORD        -- biClrImportant
 )

type LPBITMAPINFOHEADER   = Addr

getBITMAPINFOHEADER_ :: LPBITMAPINFOHEADER -> IO BITMAPINFOHEADER
getBITMAPINFOHEADER_ arg1 =
  prim_getBITMAPINFOHEADER_ arg1
  >>= \ gc_result ->
  access_prim_getBITMAPINFOHEADER__gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getBITMAPINFOHEADER__gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getBITMAPINFOHEADER__gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_getBITMAPINFOHEADER__gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_getBITMAPINFOHEADER__gc_res5 (gc_result :: Addr) >>= \ gc_res5 ->
  access_prim_getBITMAPINFOHEADER__gc_res6 (gc_result :: Addr) >>= \ gc_res6 ->
  access_prim_getBITMAPINFOHEADER__gc_res7 (gc_result :: Addr) >>= \ gc_res7 ->
  access_prim_getBITMAPINFOHEADER__gc_res8 (gc_result :: Addr) >>= \ gc_res8 ->
  access_prim_getBITMAPINFOHEADER__gc_res9 (gc_result :: Addr) >>= \ gc_res9 ->
  access_prim_getBITMAPINFOHEADER__gc_res11 (gc_result :: Addr) >>= \ gc_res11 ->
  access_prim_getBITMAPINFOHEADER__gc_res13 (gc_result :: Addr) >>= \ gc_res13 ->
  (if 0 == (gc_res11)
   then return Nothing
   else (return ((Just gc_res11)))) >>= \ gc_res10 ->
  (if 0 == (gc_res13)
   then return Nothing
   else (return ((Just gc_res13)))) >>= \ gc_res12 ->
  (return ((gc_res1,gc_res2,gc_res3,gc_res4,gc_res5,gc_res6,gc_res7,gc_res8,gc_res9,gc_res10,gc_res12)))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_getBITMAPINFOHEADER_" prim_getBITMAPINFOHEADER_ :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res2 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res3 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res4 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res5 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res6 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res7 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res8 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res9 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res11 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPINFOHEADER__gc_res13 :: Addr -> IO (Word32)


----------------------------------------------------------------
-- BITMAPFILEHEADER
----------------------------------------------------------------

type BITMAPFILEHEADER =
 ( WORD   -- bfType      -- "BM" == 0x4d42
 , DWORD  -- bfSize      -- number of bytes in file
 , WORD   -- bfReserved1 -- == 0
 , WORD   -- bfReserved2 -- == 0
 , DWORD  -- bfOffBits   -- == (char*) bits - (char*) filehdr
 )

type LPBITMAPFILEHEADER = Addr

getBITMAPFILEHEADER :: LPBITMAPFILEHEADER -> IO BITMAPFILEHEADER
getBITMAPFILEHEADER arg1 =
  prim_getBITMAPFILEHEADER arg1
  >>= \ gc_result ->
  access_prim_getBITMAPFILEHEADER_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_getBITMAPFILEHEADER_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getBITMAPFILEHEADER_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_getBITMAPFILEHEADER_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_getBITMAPFILEHEADER_gc_res5 (gc_result :: Addr) >>= \ gc_res5 ->
  (return ((gc_res1,gc_res2,gc_res3,gc_res4,gc_res5)))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_getBITMAPFILEHEADER" prim_getBITMAPFILEHEADER :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPFILEHEADER_gc_res1 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPFILEHEADER_gc_res2 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPFILEHEADER_gc_res3 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPFILEHEADER_gc_res4 :: Addr -> IO (Word16)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getBITMAPFILEHEADER_gc_res5 :: Addr -> IO (Word32)

foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sizeofBITMAP" sizeofBITMAP :: Word32
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sizeofBITMAPINFO" sizeofBITMAPINFO :: Word32
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sizeofBITMAPINFOHEADER" sizeofBITMAPINFOHEADER :: Word32
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sizeofBITMAPFILEHEADER" sizeofBITMAPFILEHEADER :: Word32
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_sizeofLPBITMAPFILEHEADER" sizeofLPBITMAPFILEHEADER :: Word32

----------------------------------------------------------------
-- CreateBMPFile
----------------------------------------------------------------

-- A (large) helper function - courtesy of Microsoft

-- Includes "dumpBMP.c" for non-ghc backends.

createBMPFile :: String -> HBITMAP -> HDC -> IO ()
createBMPFile gc_arg1 arg2 arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_createBMPFile arg1 arg2 arg3
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_createBMPFile" prim_createBMPFile :: Addr -> Addr -> Addr -> IO ()

----------------------------------------------------------------
-- Device Independent Bitmaps
----------------------------------------------------------------

foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_cBM_INIT" cBM_INIT :: DWORD

getDIBits :: HDC -> HBITMAP -> INT -> INT -> MbLPVOID -> LPBITMAPINFO -> ColorFormat -> IO INT
getDIBits arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
  (case arg5 of {
      Nothing -> (return (nullAddr));
      (Just arg5) -> (return ((arg5)))
   }) >>= \ (arg5) ->
  prim_getDIBits arg1 arg2 arg3 arg4 arg5 arg6 arg7
  >>= \ gc_result ->
  access_prim_getDIBits_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getDIBits_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getDIBits_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_getDIBits" prim_getDIBits :: Addr -> Addr -> Int32 -> Int32 -> Addr -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getDIBits_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getDIBits_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_getDIBits_gc_failstring :: Addr -> IO (Addr)

setDIBits :: HDC -> HBITMAP -> INT -> INT -> LPVOID -> LPBITMAPINFO -> ColorFormat -> IO INT
setDIBits arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
  prim_setDIBits arg1 arg2 arg3 arg4 arg5 arg6 arg7
  >>= \ gc_result ->
  access_prim_setDIBits_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_setDIBits_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setDIBits_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_setDIBits" prim_setDIBits :: Addr -> Addr -> Int32 -> Int32 -> Addr -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_setDIBits_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_setDIBits_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_setDIBits_gc_failstring :: Addr -> IO (Addr)

createDIBitmap :: HDC -> LPBITMAPINFOHEADER -> DWORD -> LPVOID -> LPBITMAPINFO -> ColorFormat -> IO HBITMAP
createDIBitmap arg1 arg2 arg3 arg4 arg5 arg6 =
  prim_createDIBitmap arg1 arg2 arg3 arg4 arg5 arg6
  >>= \ gc_result ->
  access_prim_createDIBitmap_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createDIBitmap_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createDIBitmap_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Bitmap_stub_ffi.h prim_createDIBitmap" prim_createDIBitmap :: Addr -> Addr -> Word32 -> Addr -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createDIBitmap_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createDIBitmap_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Bitmap_stub_ffi.h" access_prim_createDIBitmap_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------


