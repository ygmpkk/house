module Win32NLS where

import StdDIS
import Addr
import Word
import Win32Types


type LCID = DWORD

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SYSTEM_DEFAULT" lOCALE_SYSTEM_DEFAULT :: LCID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_USER_DEFAULT" lOCALE_USER_DEFAULT :: LCID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_NEUTRAL" lOCALE_NEUTRAL :: LCID

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_convertDefaultLocale" convertDefaultLocale :: LCID -> IO LCID

-- ToDo: various enum functions.

type CodePage = UINT

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_cP_ACP" cP_ACP :: CodePage
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_cP_MACCP" cP_MACCP :: CodePage
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_cP_OEMCP" cP_OEMCP :: CodePage

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_getACP" getACP :: IO CodePage

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_setThreadLocale" setThreadLocale :: LCID -> IO ()

type LCTYPE = UINT

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_ICALENDARTYPE" lOCALE_ICALENDARTYPE :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SDATE" lOCALE_SDATE :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_ICURRDIGITS" lOCALE_ICURRDIGITS :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SDECIMAL" lOCALE_SDECIMAL :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_ICURRENCY" lOCALE_ICURRENCY :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SGROUPING" lOCALE_SGROUPING :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_IDIGITS" lOCALE_IDIGITS :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SLIST" lOCALE_SLIST :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_IFIRSTDAYOFWEEK" lOCALE_IFIRSTDAYOFWEEK :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SLONGDATE" lOCALE_SLONGDATE :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_IFIRSTWEEKOFYEAR" lOCALE_IFIRSTWEEKOFYEAR :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SMONDECIMALSEP" lOCALE_SMONDECIMALSEP :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_ILZERO" lOCALE_ILZERO :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SMONGROUPING" lOCALE_SMONGROUPING :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_IMEASURE" lOCALE_IMEASURE :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SMONTHOUSANDSEP" lOCALE_SMONTHOUSANDSEP :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_INEGCURR" lOCALE_INEGCURR :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SNEGATIVESIGN" lOCALE_SNEGATIVESIGN :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_INEGNUMBER" lOCALE_INEGNUMBER :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SPOSITIVESIGN" lOCALE_SPOSITIVESIGN :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SSHORTDATE" lOCALE_SSHORTDATE :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_ITIME" lOCALE_ITIME :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_STHOUSAND" lOCALE_STHOUSAND :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_S1159" lOCALE_S1159 :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_STIME" lOCALE_STIME :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_S2359" lOCALE_S2359 :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_STIMEFORMAT" lOCALE_STIMEFORMAT :: LCTYPE
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lOCALE_SCURRENCY" lOCALE_SCURRENCY :: LCTYPE

-- doesn't work too well with prim_Unicode strings coming back. ToDo: fix.

setLocaleInfo :: LCID -> LCTYPE -> IO String
setLocaleInfo arg1 arg2 =
  prim_setLocaleInfo arg1 arg2
  >>= \ gc_result ->
  access_setLocaleInfo_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_setLocaleInfo_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_setLocaleInfo_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ gc_res2) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_setLocaleInfo" prim_setLocaleInfo :: Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32NLS_stub_ffi.h access_prim_setLocaleInfo_gc_failstring" access_setLocaleInfo_gc_res2 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32NLS_stub_ffi.h access_prim_setLocaleInfo_gc_failed" access_setLocaleInfo_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32NLS_stub_ffi.h access_prim_setLocaleInfo_gc_failstring" access_setLocaleInfo_gc_failstring :: Addr -> IO (Addr)

type LCMapFlags = DWORD

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_BYTEREV" lCMAP_BYTEREV :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_FULLWIDTH" lCMAP_FULLWIDTH :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_HALFWIDTH" lCMAP_HALFWIDTH :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_HIRAGANA" lCMAP_HIRAGANA :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_KATAKANA" lCMAP_KATAKANA :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_LOWERCASE" lCMAP_LOWERCASE :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_SORTKEY" lCMAP_SORTKEY :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_UPPERCASE" lCMAP_UPPERCASE :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_nORM_IGNORECASE" nORM_IGNORECASE :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_nORM_IGNORENONSPACE" nORM_IGNORENONSPACE :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_nORM_IGNOREKANATYPE" nORM_IGNOREKANATYPE :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_nORM_IGNORESYMBOLS" nORM_IGNORESYMBOLS :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_nORM_IGNOREWIDTH" nORM_IGNOREWIDTH :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sORT_STRINGSORT" sORT_STRINGSORT :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_LINGUISTIC_CASING" lCMAP_LINGUISTIC_CASING :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_SIMPLIFIED_CHINESE" lCMAP_SIMPLIFIED_CHINESE :: LCMapFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMAP_TRADITIONAL_CHINESE" lCMAP_TRADITIONAL_CHINESE :: LCMapFlags

lCMapString :: LCID -> LCMapFlags -> String -> Int -> IO String
lCMapString arg1 arg2 gc_arg1 arg4 =
  (marshall_stringLen_ gc_arg1) >>= \ (arg3,arg3_len) ->
  prim_lCMapString arg1 arg2 arg3 arg3_len arg4
  >>= \ gc_result ->
  access_lCMapString_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_lCMapString_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_lCMapString_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ gc_res2) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCMapString" prim_lCMapString :: Word32 -> Word32 -> Addr -> Int -> Int -> IO (Addr)
foreign import ccall unsafe "Win32NLS_stub_ffi.h access_prim_lCMapString_gc_res2" access_lCMapString_gc_res2 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32NLS_stub_ffi.h access_prim_lCMapString_gc_failed" access_lCMapString_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32NLS_stub_ffi.h access_prim_lCMapString_gc_failstring" access_lCMapString_gc_failstring :: Addr -> IO (Addr)

type LocaleTestFlags = DWORD

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCID_INSTALLED" lCID_INSTALLED :: LocaleTestFlags
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lCID_SUPPORTED" lCID_SUPPORTED :: LocaleTestFlags

isValidLocale :: LCID -> LocaleTestFlags -> IO Bool
isValidLocale arg1 arg2 =
  prim_isValidLocale arg1 arg2
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_isValidLocale" prim_isValidLocale :: Word32 -> Word32 -> IO (Int)

isValidCodePage :: CodePage -> IO Bool
isValidCodePage arg1 =
  prim_isValidCodePage arg1
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_isValidCodePage" prim_isValidCodePage :: Word32 -> IO (Int)

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_getUserDefaultLCID" getUserDefaultLCID :: LCID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_getUserDefaultLangID" getUserDefaultLangID :: LANGID

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_getThreadLocale" getThreadLocale :: IO LCID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_getSystemDefaultLCID" getSystemDefaultLCID :: IO LCID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_getSystemDefaultLangID" getSystemDefaultLangID :: LANGID

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_getOEMCP" getOEMCP :: CodePage

type LANGID = WORD

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANGIDFROMLCID" lANGIDFROMLCID :: LCID -> LANGID

type SubLANGID = WORD

type PrimaryLANGID = WORD

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_NEUTRAL" lANG_NEUTRAL :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_BULGARIAN" lANG_BULGARIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_CHINESE" lANG_CHINESE :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_CZECH" lANG_CZECH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_DANISH" lANG_DANISH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_GERMAN" lANG_GERMAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_GREEK" lANG_GREEK :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ENGLISH" lANG_ENGLISH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_SPANISH" lANG_SPANISH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_FINNISH" lANG_FINNISH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_FRENCH" lANG_FRENCH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_HUNGARIAN" lANG_HUNGARIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ICELANDIC" lANG_ICELANDIC :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ITALIAN" lANG_ITALIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_JAPANESE" lANG_JAPANESE :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_KOREAN" lANG_KOREAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_DUTCH" lANG_DUTCH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_NORWEGIAN" lANG_NORWEGIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_POLISH" lANG_POLISH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_PORTUGUESE" lANG_PORTUGUESE :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ROMANIAN" lANG_ROMANIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_RUSSIAN" lANG_RUSSIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_CROATIAN" lANG_CROATIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_SLOVAK" lANG_SLOVAK :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_SWEDISH" lANG_SWEDISH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_TURKISH" lANG_TURKISH :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_SLOVENIAN" lANG_SLOVENIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ARABIC" lANG_ARABIC :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_CATALAN" lANG_CATALAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_HEBREW" lANG_HEBREW :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_SERBIAN" lANG_SERBIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ALBANIAN" lANG_ALBANIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_THAI" lANG_THAI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_URDU" lANG_URDU :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_INDONESIAN" lANG_INDONESIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_BELARUSIAN" lANG_BELARUSIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ESTONIAN" lANG_ESTONIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_LATVIAN" lANG_LATVIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_LITHUANIAN" lANG_LITHUANIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_FARSI" lANG_FARSI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_VIETNAMESE" lANG_VIETNAMESE :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ARMENIAN" lANG_ARMENIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_AZERI" lANG_AZERI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_BASQUE" lANG_BASQUE :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_MACEDONIAN" lANG_MACEDONIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_AFRIKAANS" lANG_AFRIKAANS :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_GEORGIAN" lANG_GEORGIAN :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_FAEROESE" lANG_FAEROESE :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_HINDI" lANG_HINDI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_MALAY" lANG_MALAY :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_KAZAK" lANG_KAZAK :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_SWAHILI" lANG_SWAHILI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_UZBEK" lANG_UZBEK :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_TATAR" lANG_TATAR :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_BENGALI" lANG_BENGALI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_PUNJABI" lANG_PUNJABI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_GUJARATI" lANG_GUJARATI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ORIYA" lANG_ORIYA :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_TAMIL" lANG_TAMIL :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_TELUGU" lANG_TELUGU :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_KANNADA" lANG_KANNADA :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_MALAYALAM" lANG_MALAYALAM :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_ASSAMESE" lANG_ASSAMESE :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_MARATHI" lANG_MARATHI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_SANSKRIT" lANG_SANSKRIT :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_KONKANI" lANG_KONKANI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_MANIPURI" lANG_MANIPURI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_SINDHI" lANG_SINDHI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_KASHMIRI" lANG_KASHMIRI :: PrimaryLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_lANG_NEPALI" lANG_NEPALI :: PrimaryLANGID

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_mAKELANGID" mAKELANGID :: PrimaryLANGID -> SubLANGID -> LANGID

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_mAKELCID" mAKELCID :: LANGID -> SortID -> LCID

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_pRIMARYLANGID" pRIMARYLANGID :: LANGID -> PrimaryLANGID

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANGID" sUBLANGID :: LANGID -> SubLANGID

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sORTIDFROMLCID" sORTIDFROMLCID :: LCID -> SortID

type SortID = WORD

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sORT_DEFAULT" sORT_DEFAULT :: SortID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sORT_JAPANESE_XJIS" sORT_JAPANESE_XJIS :: SortID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sORT_JAPANESE_UNICODE" sORT_JAPANESE_UNICODE :: SortID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sORT_CHINESE_BIG5" sORT_CHINESE_BIG5 :: SortID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sORT_CHINESE_UNICODE" sORT_CHINESE_UNICODE :: SortID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sORT_KOREAN_KSC" sORT_KOREAN_KSC :: SortID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sORT_KOREAN_UNICODE" sORT_KOREAN_UNICODE :: SortID

foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_NEUTRAL" sUBLANG_NEUTRAL :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_DEFAULT" sUBLANG_DEFAULT :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SYS_DEFAULT" sUBLANG_SYS_DEFAULT :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_CHINESE_TRADITIONAL" sUBLANG_CHINESE_TRADITIONAL :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_CHINESE_SIMPLIFIED" sUBLANG_CHINESE_SIMPLIFIED :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_CHINESE_HONGKONG" sUBLANG_CHINESE_HONGKONG :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_CHINESE_SINGAPORE" sUBLANG_CHINESE_SINGAPORE :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_DUTCH" sUBLANG_DUTCH :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_DUTCH_BELGIAN" sUBLANG_DUTCH_BELGIAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_US" sUBLANG_ENGLISH_US :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_UK" sUBLANG_ENGLISH_UK :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_AUS" sUBLANG_ENGLISH_AUS :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_CAN" sUBLANG_ENGLISH_CAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_NZ" sUBLANG_ENGLISH_NZ :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_EIRE" sUBLANG_ENGLISH_EIRE :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_FRENCH" sUBLANG_FRENCH :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_FRENCH_BELGIAN" sUBLANG_FRENCH_BELGIAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_FRENCH_CANADIAN" sUBLANG_FRENCH_CANADIAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_FRENCH_SWISS" sUBLANG_FRENCH_SWISS :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_GERMAN" sUBLANG_GERMAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_GERMAN_SWISS" sUBLANG_GERMAN_SWISS :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_GERMAN_AUSTRIAN" sUBLANG_GERMAN_AUSTRIAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ITALIAN" sUBLANG_ITALIAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ITALIAN_SWISS" sUBLANG_ITALIAN_SWISS :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_NORWEGIAN_BOKMAL" sUBLANG_NORWEGIAN_BOKMAL :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_NORWEGIAN_NYNORSK" sUBLANG_NORWEGIAN_NYNORSK :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_PORTUGUESE" sUBLANG_PORTUGUESE :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_PORTUGUESE_BRAZILIAN" sUBLANG_PORTUGUESE_BRAZILIAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH" sUBLANG_SPANISH :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_MEXICAN" sUBLANG_SPANISH_MEXICAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_MODERN" sUBLANG_SPANISH_MODERN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_SAUDI_ARABIA" sUBLANG_ARABIC_SAUDI_ARABIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_IRAQ" sUBLANG_ARABIC_IRAQ :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_EGYPT" sUBLANG_ARABIC_EGYPT :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_LIBYA" sUBLANG_ARABIC_LIBYA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_ALGERIA" sUBLANG_ARABIC_ALGERIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_MOROCCO" sUBLANG_ARABIC_MOROCCO :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_TUNISIA" sUBLANG_ARABIC_TUNISIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_OMAN" sUBLANG_ARABIC_OMAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_YEMEN" sUBLANG_ARABIC_YEMEN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_SYRIA" sUBLANG_ARABIC_SYRIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_JORDAN" sUBLANG_ARABIC_JORDAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_LEBANON" sUBLANG_ARABIC_LEBANON :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_KUWAIT" sUBLANG_ARABIC_KUWAIT :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_UAE" sUBLANG_ARABIC_UAE :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_BAHRAIN" sUBLANG_ARABIC_BAHRAIN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ARABIC_QATAR" sUBLANG_ARABIC_QATAR :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_AZERI_CYRILLIC" sUBLANG_AZERI_CYRILLIC :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_AZERI_LATIN" sUBLANG_AZERI_LATIN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_CHINESE_MACAU" sUBLANG_CHINESE_MACAU :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_SOUTH_AFRICA" sUBLANG_ENGLISH_SOUTH_AFRICA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_JAMAICA" sUBLANG_ENGLISH_JAMAICA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_CARIBBEAN" sUBLANG_ENGLISH_CARIBBEAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_BELIZE" sUBLANG_ENGLISH_BELIZE :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_TRINIDAD" sUBLANG_ENGLISH_TRINIDAD :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_PHILIPPINES" sUBLANG_ENGLISH_PHILIPPINES :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_ENGLISH_ZIMBABWE" sUBLANG_ENGLISH_ZIMBABWE :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_FRENCH_LUXEMBOURG" sUBLANG_FRENCH_LUXEMBOURG :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_FRENCH_MONACO" sUBLANG_FRENCH_MONACO :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_GERMAN_LUXEMBOURG" sUBLANG_GERMAN_LUXEMBOURG :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_GERMAN_LIECHTENSTEIN" sUBLANG_GERMAN_LIECHTENSTEIN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_KASHMIRI_INDIA" sUBLANG_KASHMIRI_INDIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_KOREAN" sUBLANG_KOREAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_LITHUANIAN" sUBLANG_LITHUANIAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_MALAY_MALAYSIA" sUBLANG_MALAY_MALAYSIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_MALAY_BRUNEI_DARUSSALAM" sUBLANG_MALAY_BRUNEI_DARUSSALAM :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_NEPALI_INDIA" sUBLANG_NEPALI_INDIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SERBIAN_LATIN" sUBLANG_SERBIAN_LATIN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SERBIAN_CYRILLIC" sUBLANG_SERBIAN_CYRILLIC :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_GUATEMALA" sUBLANG_SPANISH_GUATEMALA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_COSTA_RICA" sUBLANG_SPANISH_COSTA_RICA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_PANAMA" sUBLANG_SPANISH_PANAMA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_DOMINICAN_REPUBLIC" sUBLANG_SPANISH_DOMINICAN_REPUBLIC :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_VENEZUELA" sUBLANG_SPANISH_VENEZUELA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_COLOMBIA" sUBLANG_SPANISH_COLOMBIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_PERU" sUBLANG_SPANISH_PERU :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_ARGENTINA" sUBLANG_SPANISH_ARGENTINA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_ECUADOR" sUBLANG_SPANISH_ECUADOR :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_CHILE" sUBLANG_SPANISH_CHILE :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_URUGUAY" sUBLANG_SPANISH_URUGUAY :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_PARAGUAY" sUBLANG_SPANISH_PARAGUAY :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_BOLIVIA" sUBLANG_SPANISH_BOLIVIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_EL_SALVADOR" sUBLANG_SPANISH_EL_SALVADOR :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_HONDURAS" sUBLANG_SPANISH_HONDURAS :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_NICARAGUA" sUBLANG_SPANISH_NICARAGUA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SPANISH_PUERTO_RICO" sUBLANG_SPANISH_PUERTO_RICO :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SWEDISH" sUBLANG_SWEDISH :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_SWEDISH_FINLAND" sUBLANG_SWEDISH_FINLAND :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_URDU_PAKISTAN" sUBLANG_URDU_PAKISTAN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_URDU_INDIA" sUBLANG_URDU_INDIA :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_UZBEK_LATIN" sUBLANG_UZBEK_LATIN :: SubLANGID
foreign import  ccall unsafe "Win32NLS_stub_ffi.h prim_sUBLANG_UZBEK_CYRILLIC" sUBLANG_UZBEK_CYRILLIC :: SubLANGID

-- % , SUBLANG_LITHUANIAN_CLASSIC (not in mingw-20001111)
