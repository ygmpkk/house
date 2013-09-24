module Win32Font
{-
	( CharSet
	, PitchAndFamily
	, OutPrecision
	, ClipPrecision
	, FontQuality
	, FontWeight

	, createFont, deleteFont

	, StockFont, getStockFont
	, oEM_FIXED_FONT, aNSI_FIXED_FONT, aNSI_VAR_FONT, sYSTEM_FONT
	, dEVICE_DEFAULT_FONT, sYSTEM_FIXED_FONT
	) where
-}
	where

import StdDIS
import Win32Types
import GDITypes


----------------------------------------------------------------
-- Types
----------------------------------------------------------------

type CharSet        = UINT
type PitchAndFamily = UINT
type OutPrecision   = UINT
type ClipPrecision  = UINT
type FontQuality    = UINT
type FontWeight     = Word32
type FaceName       = String


-- # A FaceName is a string no more that LF_FACESIZE in length
-- # (including null terminator).
-- %const Int LF_FACESIZE         # == 32
-- %sentinel_array : FaceName : CHAR : char : $0 = '\0' : ('\0' == $0) : LF_FACESIZE

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_aNSI_CHARSET" aNSI_CHARSET :: CharSet
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_dEFAULT_CHARSET" dEFAULT_CHARSET :: CharSet
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_sYMBOL_CHARSET" sYMBOL_CHARSET :: CharSet
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_sHIFTJIS_CHARSET" sHIFTJIS_CHARSET :: CharSet
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_hANGEUL_CHARSET" hANGEUL_CHARSET :: CharSet
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_cHINESEBIG5_CHARSET" cHINESEBIG5_CHARSET :: CharSet
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oEM_CHARSET" oEM_CHARSET :: CharSet

foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_dEFAULT_PITCH" dEFAULT_PITCH :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fIXED_PITCH" fIXED_PITCH :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_vARIABLE_PITCH" vARIABLE_PITCH :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fF_DONTCARE" fF_DONTCARE :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fF_ROMAN" fF_ROMAN :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fF_SWISS" fF_SWISS :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fF_MODERN" fF_MODERN :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fF_SCRIPT" fF_SCRIPT :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fF_DECORATIVE" fF_DECORATIVE :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_familyMask" familyMask :: PitchAndFamily
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_pitchMask" pitchMask :: PitchAndFamily

foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oUT_DEFAULT_PRECIS" oUT_DEFAULT_PRECIS :: OutPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oUT_STRING_PRECIS" oUT_STRING_PRECIS :: OutPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oUT_CHARACTER_PRECIS" oUT_CHARACTER_PRECIS :: OutPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oUT_STROKE_PRECIS" oUT_STROKE_PRECIS :: OutPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oUT_TT_PRECIS" oUT_TT_PRECIS :: OutPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oUT_DEVICE_PRECIS" oUT_DEVICE_PRECIS :: OutPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oUT_RASTER_PRECIS" oUT_RASTER_PRECIS :: OutPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oUT_TT_ONLY_PRECIS" oUT_TT_ONLY_PRECIS :: OutPrecision

foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_cLIP_DEFAULT_PRECIS" cLIP_DEFAULT_PRECIS :: ClipPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_cLIP_CHARACTER_PRECIS" cLIP_CHARACTER_PRECIS :: ClipPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_cLIP_STROKE_PRECIS" cLIP_STROKE_PRECIS :: ClipPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_cLIP_MASK" cLIP_MASK :: ClipPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_cLIP_LH_ANGLES" cLIP_LH_ANGLES :: ClipPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_cLIP_TT_ALWAYS" cLIP_TT_ALWAYS :: ClipPrecision
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_cLIP_EMBEDDED" cLIP_EMBEDDED :: ClipPrecision

foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_dEFAULT_QUALITY" dEFAULT_QUALITY :: FontQuality
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_dRAFT_QUALITY" dRAFT_QUALITY :: FontQuality
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_pROOF_QUALITY" pROOF_QUALITY :: FontQuality

foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_DONTCARE" fW_DONTCARE :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_THIN" fW_THIN :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_EXTRALIGHT" fW_EXTRALIGHT :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_LIGHT" fW_LIGHT :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_NORMAL" fW_NORMAL :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_MEDIUM" fW_MEDIUM :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_SEMIBOLD" fW_SEMIBOLD :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_BOLD" fW_BOLD :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_EXTRABOLD" fW_EXTRABOLD :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_HEAVY" fW_HEAVY :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_REGULAR" fW_REGULAR :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_ULTRALIGHT" fW_ULTRALIGHT :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_DEMIBOLD" fW_DEMIBOLD :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_ULTRABOLD" fW_ULTRABOLD :: FontWeight
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_fW_BLACK" fW_BLACK :: FontWeight

----------------------------------------------------------------
-- Functions
----------------------------------------------------------------

createFont :: INT -> INT -> INT -> INT -> FontWeight -> Bool -> Bool -> Bool -> CharSet -> OutPrecision -> ClipPrecision -> FontQuality -> PitchAndFamily -> FaceName -> IO HFONT
createFont arg1 arg2 arg3 arg4 arg5 gc_arg1 gc_arg2 gc_arg3 arg9 arg10 arg11 arg12 arg13 gc_arg4 =
  (marshall_bool_ gc_arg1) >>= \ (arg6) ->
  (marshall_bool_ gc_arg2) >>= \ (arg7) ->
  (marshall_bool_ gc_arg3) >>= \ (arg8) ->
  (marshall_string_ gc_arg4) >>= \ (arg14) ->
  prim_createFont arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14
  >>= \ gc_result ->
  access_prim_createFont_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createFont_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createFont_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_createFont" prim_createFont :: Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Font_stub_ffi.h" access_prim_createFont_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Font_stub_ffi.h" access_prim_createFont_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Font_stub_ffi.h" access_prim_createFont_gc_failstring :: Addr -> IO (Addr)

-- test :: IO ()
-- test = do
--   f <- createFont_adr (100,100) 0 False False "Arial"
--   putStrLn "Created first font"
--   f <- createFont_adr (100,100) (-90) False False "Bogus"
--   putStrLn "Created second font"
-- 
-- createFont_adr (width, height) escapement bold italic family = 
--  createFont height width
-- 		     (round (escapement * 1800/pi))
-- 		     0                     -- orientation
-- 		     weight
-- 		     italic False False    -- italic, underline, strikeout
-- 		     aNSI_CHARSET
-- 		     oUT_DEFAULT_PRECIS
-- 		     cLIP_DEFAULT_PRECIS
-- 		     dEFAULT_QUALITY
-- 		     dEFAULT_PITCH
-- 		     family
--  where
--   weight | bold      = fW_BOLD
-- 	    | otherwise = fW_NORMAL


-- missing CreateFontIndirect from WinFonts.ss; GSL ???

deleteFont :: HFONT -> IO ()
deleteFont arg1 =
  prim_deleteFont arg1
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_deleteFont" prim_deleteFont :: Addr -> IO ()

----------------------------------------------------------------

type StockFont      = WORD

foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_oEM_FIXED_FONT" oEM_FIXED_FONT :: StockFont
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_aNSI_FIXED_FONT" aNSI_FIXED_FONT :: StockFont
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_aNSI_VAR_FONT" aNSI_VAR_FONT :: StockFont
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_sYSTEM_FONT" sYSTEM_FONT :: StockFont
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_dEVICE_DEFAULT_FONT" dEVICE_DEFAULT_FONT :: StockFont
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_sYSTEM_FIXED_FONT" sYSTEM_FIXED_FONT :: StockFont

getStockFont :: StockFont -> IO HFONT
getStockFont arg1 =
  prim_getStockFont arg1
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Font_stub_ffi.h prim_getStockFont" prim_getStockFont :: Word16 -> IO (Addr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
