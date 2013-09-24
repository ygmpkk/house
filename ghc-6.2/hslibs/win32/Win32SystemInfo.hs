module Win32SystemInfo where

import StdDIS
import Win32Types
import GDITypes



----------------------------------------------------------------
-- Environment Strings
----------------------------------------------------------------

-- %fun ExpandEnvironmentStrings :: String -> IO String

----------------------------------------------------------------
-- Computer Name
----------------------------------------------------------------

-- %fun GetComputerName :: IO String
-- %fun SetComputerName :: String -> IO ()
-- %end free(arg1)

----------------------------------------------------------------
-- Hardware Profiles
----------------------------------------------------------------

-- %fun GetCurrentHwProfile :: IO HW_PROFILE_INFO

----------------------------------------------------------------
-- Keyboard Type
----------------------------------------------------------------

-- %fun GetKeyboardType :: KeyboardTypeKind -> IO KeyboardType

----------------------------------------------------------------
-- System Color
----------------------------------------------------------------

type SystemColor   = UINT

-- ToDo: This list is out of date.
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_SCROLLBAR" cOLOR_SCROLLBAR :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_BACKGROUND" cOLOR_BACKGROUND :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_ACTIVECAPTION" cOLOR_ACTIVECAPTION :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_INACTIVECAPTION" cOLOR_INACTIVECAPTION :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_MENU" cOLOR_MENU :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_WINDOW" cOLOR_WINDOW :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_WINDOWFRAME" cOLOR_WINDOWFRAME :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_MENUTEXT" cOLOR_MENUTEXT :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_WINDOWTEXT" cOLOR_WINDOWTEXT :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_CAPTIONTEXT" cOLOR_CAPTIONTEXT :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_ACTIVEBORDER" cOLOR_ACTIVEBORDER :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_INACTIVEBORDER" cOLOR_INACTIVEBORDER :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_APPWORKSPACE" cOLOR_APPWORKSPACE :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_HIGHLIGHT" cOLOR_HIGHLIGHT :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_HIGHLIGHTTEXT" cOLOR_HIGHLIGHTTEXT :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_BTNFACE" cOLOR_BTNFACE :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_BTNSHADOW" cOLOR_BTNSHADOW :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_GRAYTEXT" cOLOR_GRAYTEXT :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_BTNTEXT" cOLOR_BTNTEXT :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_INACTIVECAPTIONTEXT" cOLOR_INACTIVECAPTIONTEXT :: SystemColor
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_cOLOR_BTNHIGHLIGHT" cOLOR_BTNHIGHLIGHT :: SystemColor

-- %fun GetSysColor :: SystemColor -> IO COLORREF
-- %fun SetSysColors :: [(SystemColor,COLORREF)] -> IO ()

----------------------------------------------------------------
-- Standard Directories
----------------------------------------------------------------

-- %fun GetSystemDirectory  :: IO String
-- %fun GetWindowsDirectory :: IO String

----------------------------------------------------------------
-- System Info (Info about processor and memory subsystem)
----------------------------------------------------------------

-- %fun GetSystemInfo :: IO SystemInfo
-- 
-- typedef struct _SYSTEM_INFO { // sinf 
--     union { 
-- 	   DWORD  dwOemId; 
-- 	   struct { 
-- 	       WORD wProcessorArchitecture; 
-- 	       WORD wReserved; 
-- 	   }; 
--     }; 
--     DWORD  dwPageSize; 
--     LPVOID lpMinimumApplicationAddress; 
--     LPVOID lpMaximumApplicationAddress; 
--     DWORD  dwActiveProcessorMask; 
--     DWORD  dwNumberOfProcessors; 
--     DWORD  dwProcessorType; 
--     DWORD  dwAllocationGranularity; 
--     WORD  wProcessorLevel; 
--     WORD  wProcessorRevision; 
-- } SYSTEM_INFO; 
 

----------------------------------------------------------------
-- System metrics
----------------------------------------------------------------

type SMSetting = UINT

foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_ARRANGE" sM_ARRANGE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CLEANBOOT" sM_CLEANBOOT :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CMETRICS" sM_CMETRICS :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CMOUSEBUTTONS" sM_CMOUSEBUTTONS :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXBORDER" sM_CXBORDER :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYBORDER" sM_CYBORDER :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXCURSOR" sM_CXCURSOR :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYCURSOR" sM_CYCURSOR :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXDLGFRAME" sM_CXDLGFRAME :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYDLGFRAME" sM_CYDLGFRAME :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXDOUBLECLK" sM_CXDOUBLECLK :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYDOUBLECLK" sM_CYDOUBLECLK :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXDRAG" sM_CXDRAG :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYDRAG" sM_CYDRAG :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXEDGE" sM_CXEDGE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYEDGE" sM_CYEDGE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXFRAME" sM_CXFRAME :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYFRAME" sM_CYFRAME :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXFULLSCREEN" sM_CXFULLSCREEN :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYFULLSCREEN" sM_CYFULLSCREEN :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXHSCROLL" sM_CXHSCROLL :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYVSCROLL" sM_CYVSCROLL :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXICON" sM_CXICON :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYICON" sM_CYICON :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXICONSPACING" sM_CXICONSPACING :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYICONSPACING" sM_CYICONSPACING :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXMAXIMIZED" sM_CXMAXIMIZED :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYMAXIMIZED" sM_CYMAXIMIZED :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXMENUCHECK" sM_CXMENUCHECK :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYMENUCHECK" sM_CYMENUCHECK :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXMENUSIZE" sM_CXMENUSIZE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYMENUSIZE" sM_CYMENUSIZE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXMIN" sM_CXMIN :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYMIN" sM_CYMIN :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXMINIMIZED" sM_CXMINIMIZED :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYMINIMIZED" sM_CYMINIMIZED :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXMINTRACK" sM_CXMINTRACK :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYMINTRACK" sM_CYMINTRACK :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXSCREEN" sM_CXSCREEN :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYSCREEN" sM_CYSCREEN :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXSIZE" sM_CXSIZE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYSIZE" sM_CYSIZE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXSIZEFRAME" sM_CXSIZEFRAME :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYSIZEFRAME" sM_CYSIZEFRAME :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXSMICON" sM_CXSMICON :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYSMICON" sM_CYSMICON :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXSMSIZE" sM_CXSMSIZE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYSMSIZE" sM_CYSMSIZE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CXVSCROLL" sM_CXVSCROLL :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYHSCROLL" sM_CYHSCROLL :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYVTHUMB" sM_CYVTHUMB :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYCAPTION" sM_CYCAPTION :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYKANJIWINDOW" sM_CYKANJIWINDOW :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYMENU" sM_CYMENU :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_CYSMCAPTION" sM_CYSMCAPTION :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_DBCSENABLED" sM_DBCSENABLED :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_DEBUG" sM_DEBUG :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_MENUDROPALIGNMENT" sM_MENUDROPALIGNMENT :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_MIDEASTENABLED" sM_MIDEASTENABLED :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_MOUSEPRESENT" sM_MOUSEPRESENT :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_NETWORK" sM_NETWORK :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_PENWINDOWS" sM_PENWINDOWS :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_SECURE" sM_SECURE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_SHOWSOUNDS" sM_SHOWSOUNDS :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_SLOWMACHINE" sM_SLOWMACHINE :: SMSetting
foreign import  ccall unsafe "Win32SystemInfo_stub_ffi.h prim_sM_SWAPBUTTON" sM_SWAPBUTTON :: SMSetting

-- %fun GetSystemMetrics :: SMSetting -> IO Int

----------------------------------------------------------------
-- Thread Desktops
----------------------------------------------------------------

-- %fun GetThreadDesktop :: ThreadId -> IO HDESK
-- %fun SetThreadDesktop :: ThreadId -> HDESK -> IO ()

----------------------------------------------------------------
-- User name
----------------------------------------------------------------

-- %fun GetUserName :: IO String

----------------------------------------------------------------
-- Version Info
----------------------------------------------------------------

-- %fun GetVersionEx :: IO VersionInfo
-- 
-- typedef struct _OSVERSIONINFO{ 
--     DWORD dwOSVersionInfoSize; 
--     DWORD dwMajorVersion; 
--     DWORD dwMinorVersion; 
--     DWORD dwBuildNumber; 
--     DWORD dwPlatformId; 
--     TCHAR szCSDVersion[ 128 ]; 
-- } OSVERSIONINFO; 
 
----------------------------------------------------------------
-- Processor features
----------------------------------------------------------------

--
-- Including these lines causes problems on Win95
-- %fun IsProcessorFeaturePresent :: ProcessorFeature -> Bool
--
-- type ProcessorFeature   = DWORD
-- %dis processorFeature x = dWORD x
--
-- %const ProcessorFeature
-- % [ PF_FLOATING_POINT_PRECISION_ERRATA 
-- % , PF_FLOATING_POINT_EMULATED 
-- % , PF_COMPARE_EXCHANGE_DOUBLE 
-- % , PF_MMX_INSTRUCTIONS_AVAILABLE 
-- % ]

----------------------------------------------------------------
-- System Parameter Information
----------------------------------------------------------------

-- %fun SystemParametersInfo :: ?? -> Bool -> IO ??

----------------------------------------------------------------
-- End
----------------------------------------------------------------
