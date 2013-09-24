{-# OPTIONS -#include "Win32Dialogue_stub.h" #-}
module Win32Dialogue where

import StdDIS
import Win32Types
import GDITypes
import Win32Window
import Win32WinMessage
import Win32Control
import Foreign.ForeignPtr
import Foreign.Ptr ( castFunPtrToPtr )


type DTemplate = LPCTSTR

type DTemplateMem = Addr

type DialogStyle = WindowStyle

mkDialogTemplate :: String -> IO DTemplate
mkDialogTemplate = marshall_string_

type ResourceID = Int

mkResource :: ResourceID -> IO Addr
mkResource arg1 =
  prim_mkResource arg1
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_mkResource" prim_mkResource :: Int -> IO (Addr)

mkDialogTemplateFromResource :: Int -> IO DTemplate
mkDialogTemplateFromResource = mkResource

type DialogProc = HWND -> WindowMessage -> WPARAM -> LPARAM -> IO Int
type DialogProc_FFI = Ptr () -> WindowMessage -> WPARAM -> LPARAM -> IO Int


marshall_dialogProc_ :: DialogProc -> IO (FunPtr DialogProc)
marshall_dialogProc_ cl = mkDialogClosure (\ x -> cl (ptrToAddr x))

-- ToDo: this was declared as a stdcall not a ccall - let's
-- hope and pray that it makes no difference - ADR
foreign import ccall "wrapper" mkDialogClosure :: DialogProc_FFI -> IO (FunPtr DialogProc)
--marshall_dialogProc_ x = return nullAddr

dialogBox :: HINSTANCE -> DTemplate -> MbHWND -> DialogProc -> IO Int
dialogBox hInst lpTemp hWndParent gc_arg1 =
  (case hWndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hWndParent) -> (return ((hWndParent)))
   }) >>= \ (hWndParent) ->
  (marshall_dialogProc_ gc_arg1) >>= \ (diaFun) ->
  prim_dialogBox hInst lpTemp hWndParent diaFun
  >>= \ gc_result ->
  access_prim_dialogBox_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_dialogBox_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_dialogBox_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_dialogBox" prim_dialogBox :: Addr -> Addr -> Addr -> FunPtr DialogProc -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBox_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBox_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBox_gc_failstring :: Addr -> IO (Addr)

dialogBoxParam :: HINSTANCE -> DTemplate -> MbHWND -> DialogProc -> LPARAM -> IO Int
dialogBoxParam hInst lpTemp hWndParent gc_arg1 dwInit =
  (case hWndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hWndParent) -> (return ((hWndParent)))
   }) >>= \ (hWndParent) ->
  (marshall_dialogProc_ gc_arg1) >>= \ (diaFun) ->
  prim_dialogBoxParam hInst lpTemp hWndParent diaFun dwInit
  >>= \ gc_result ->
  access_prim_dialogBoxParam_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_dialogBoxParam_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_dialogBoxParam_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_dialogBoxParam" prim_dialogBoxParam :: Addr -> Addr -> Addr -> FunPtr DialogProc -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBoxParam_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBoxParam_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBoxParam_gc_failstring :: Addr -> IO (Addr)

dialogBoxIndirect :: HINSTANCE -> DTemplateMem -> MbHWND -> DialogProc -> IO Int
dialogBoxIndirect hInst lpTemp hWndParent gc_arg1 =
  (case hWndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hWndParent) -> (return ((hWndParent)))
   }) >>= \ (hWndParent) ->
  (marshall_dialogProc_ gc_arg1) >>= \ (diaFun) ->
  prim_dialogBoxIndirect hInst lpTemp hWndParent diaFun
  >>= \ gc_result ->
  access_prim_dialogBoxIndirect_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_dialogBoxIndirect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_dialogBoxIndirect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_dialogBoxIndirect" prim_dialogBoxIndirect :: Addr -> Addr -> Addr -> FunPtr DialogProc -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBoxIndirect_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBoxIndirect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBoxIndirect_gc_failstring :: Addr -> IO (Addr)

dialogBoxIndirectParam :: HINSTANCE -> DTemplateMem -> MbHWND -> DialogProc -> LPARAM -> IO Int
dialogBoxIndirectParam hInst lpTemp hWndParent gc_arg1 dwInit =
  (case hWndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hWndParent) -> (return ((hWndParent)))
   }) >>= \ (hWndParent) ->
  (marshall_dialogProc_ gc_arg1) >>= \ (diaFun) ->
  prim_dialogBoxIndirectParam hInst lpTemp hWndParent diaFun dwInit
  >>= \ gc_result ->
  access_prim_dialogBoxIndirectParam_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_dialogBoxIndirectParam_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_dialogBoxIndirectParam_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_dialogBoxIndirectParam" prim_dialogBoxIndirectParam :: Addr -> Addr -> Addr -> FunPtr DialogProc -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBoxIndirectParam_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBoxIndirectParam_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_dialogBoxIndirectParam_gc_failstring :: Addr -> IO (Addr)


data DialogTemplate
 = DialogTemplate 
      Int Int Int Int  -- x, y, cx, cy
      WindowStyle
      DWORD
      (Either ResourceID String)  -- menu
      (Either ResourceID String)  -- class
      (Either ResourceID String)  -- caption
      (Either ResourceID String)  -- fontname
      Int	 		  -- font height
      [DialogControl]

data DialogControl
 = DialogControl
      Int Int Int Int -- x,y, cx, cy
      (Either ResourceID String) -- text
      (Either ResourceID String) -- classname
      WindowStyle
      DWORD
      Int			 -- id

mkDialogFromTemplate :: DialogTemplate -> IO DTemplateMem
mkDialogFromTemplate (DialogTemplate x y cx cy
				     wstyle extstyle
				     mb_menu mb_class caption
				     font font_height
				     controls) = do
  prim_hmenu    <- marshall_res mb_menu
  prim_class    <- marshall_res mb_class
  prim_caption  <- marshall_res caption
  prim_font     <- marshall_res font
  dtemp <- mkDiaTemplate 0 x y cx cy wstyle extstyle 
  			 prim_hmenu prim_class
			 prim_caption prim_font
			 font_height
  mapM_ (addControl dtemp) controls
  getFinalDialog dtemp

pushButtonControl :: Int -> Int -> Int -> Int
		  -> DWORD -> DWORD -> Int
		  -> String
		  -> DialogControl
pushButtonControl x y cx cy style estyle id lab =
  DialogControl x y cx cy (Left 0x0080) (Right lab)
  		(style + bS_DEFPUSHBUTTON) estyle id

labelControl :: Int -> Int -> Int -> Int
	     -> DWORD -> DWORD -> Int
	     -> String
             -> DialogControl
labelControl x y cx cy style estyle id lab =
  DialogControl x y cx cy (Left 0x0082) (Right lab)
  		(style + sS_LEFT) estyle id

listBoxControl :: Int -> Int -> Int -> Int
	       -> DWORD -> DWORD -> Int
	       -> String
               -> DialogControl
listBoxControl x y cx cy style estyle id lab =
  DialogControl x y cx cy (Left 0x0083) (Right lab)
  		(style) estyle id

comboBoxControl :: Int -> Int -> Int -> Int
	       -> DWORD -> DWORD -> Int
	       -> String
               -> DialogControl
comboBoxControl x y cx cy style estyle id lab =
  DialogControl x y cx cy (Left 0x0085) (Right lab)
  		(style) estyle id

editControl :: Int -> Int -> Int -> Int
	       -> DWORD -> DWORD -> Int
	       -> String
               -> DialogControl
editControl x y cx cy style estyle id lab =
  DialogControl x y cx cy (Left 0x0081) (Right lab)
  		(style + eS_LEFT) estyle id

scrollBarControl :: Int -> Int -> Int -> Int
	       -> DWORD -> DWORD -> Int
	       -> String
               -> DialogControl
scrollBarControl x y cx cy style estyle id lab =
  DialogControl x y cx cy (Left 0x0084) (Right lab)
  		(style) estyle id

getFinalDialog :: Addr -> IO DTemplateMem
getFinalDialog arg1 =
  prim_getFinalDialog arg1
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_getFinalDialog" prim_getFinalDialog :: Addr -> IO (Addr)

mkDiaTemplate :: Int -> Int -> Int -> Int -> Int -> WindowStyle -> DWORD -> Addr -> Addr -> Addr -> Addr -> Int -> IO Addr
mkDiaTemplate arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 =
  prim_mkDiaTemplate arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_mkDiaTemplate" prim_mkDiaTemplate :: Int -> Int -> Int -> Int -> Int -> Word32 -> Word32 -> Addr -> Addr -> Addr -> Addr -> Int -> IO (Addr)

addControl :: Addr -> DialogControl -> IO ()
addControl dtemp (DialogControl x y cx cy mb_text mb_class
				style exstyle
				id) = do
   prim_text  <- marshall_res mb_text
   prim_class <- marshall_res mb_class
   addDiaControl dtemp prim_text id prim_class style 
  		 x y cx cy exstyle
   return ()

addDiaControl :: Addr -> Addr -> Int -> Addr -> DWORD -> Int -> Int -> Int -> Int -> DWORD -> IO Addr
addDiaControl arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 =
  prim_addDiaControl arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_addDiaControl" prim_addDiaControl :: Addr -> Addr -> Int -> Addr -> Word32 -> Int -> Int -> Int -> Int -> Word32 -> IO (Addr)

marshall_res :: Either ResourceID String -> IO Addr
marshall_res (Left r)  = mkResource r
marshall_res (Right s) = toUnicodeStr s

toUnicodeStr :: String -> IO Addr
toUnicodeStr gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_toUnicodeStr arg1
  >>= \  gc_res1  ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_toUnicodeStr" prim_toUnicodeStr :: Addr -> IO (Addr)

-- modeless dialogs
createDialog :: HINSTANCE -> DTemplate -> MbHWND -> DialogProc -> IO HWND
createDialog hInst lpTemp hWndParent gc_arg1 =
  (case hWndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hWndParent) -> (return ((hWndParent)))
   }) >>= \ (hWndParent) ->
  (marshall_dialogProc_ gc_arg1) >>= \ (diaFun) ->
  prim_createDialog hInst lpTemp hWndParent diaFun
  >>= \ gc_result ->
  access_prim_createDialog_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createDialog_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createDialog_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_createDialog" prim_createDialog :: Addr -> Addr -> Addr -> FunPtr DialogProc -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialog_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialog_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialog_gc_failstring :: Addr -> IO (Addr)

createDialogParam :: HINSTANCE -> DTemplate -> MbHWND -> DialogProc -> LPARAM -> IO HWND
createDialogParam hInst lpTemp hWndParent gc_arg1 dwInit =
  (case hWndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hWndParent) -> (return ((hWndParent)))
   }) >>= \ (hWndParent) ->
  (marshall_dialogProc_ gc_arg1) >>= \ (diaFun) ->
  prim_createDialogParam hInst lpTemp hWndParent diaFun dwInit
  >>= \ gc_result ->
  access_prim_createDialogParam_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createDialogParam_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createDialogParam_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_createDialogParam" prim_createDialogParam :: Addr -> Addr -> Addr -> FunPtr DialogProc -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialogParam_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialogParam_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialogParam_gc_failstring :: Addr -> IO (Addr)

createDialogIndirect :: HINSTANCE -> DTemplateMem -> MbHWND -> DialogProc -> IO HWND
createDialogIndirect hInst lpTemp hWndParent gc_arg1 =
  (case hWndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hWndParent) -> (return ((hWndParent)))
   }) >>= \ (hWndParent) ->
  (marshall_dialogProc_ gc_arg1) >>= \ (diaFun) ->
  prim_createDialogIndirect hInst lpTemp hWndParent diaFun
  >>= \ gc_result ->
  access_prim_createDialogIndirect_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createDialogIndirect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createDialogIndirect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_createDialogIndirect" prim_createDialogIndirect :: Addr -> Addr -> Addr -> FunPtr DialogProc -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialogIndirect_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialogIndirect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialogIndirect_gc_failstring :: Addr -> IO (Addr)

createDialogIndirectParam :: HINSTANCE -> DTemplateMem -> MbHWND -> DialogProc -> LPARAM -> IO HWND
createDialogIndirectParam hInst lpTemp hWndParent gc_arg1 dwInit =
  (case hWndParent of {
      Nothing -> (return (nullHANDLE));
      (Just hWndParent) -> (return ((hWndParent)))
   }) >>= \ (hWndParent) ->
  (marshall_dialogProc_ gc_arg1) >>= \ (diaFun) ->
  prim_createDialogIndirectParam hInst lpTemp hWndParent diaFun dwInit
  >>= \ gc_result ->
  access_prim_createDialogIndirectParam_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createDialogIndirectParam_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createDialogIndirectParam_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_createDialogIndirectParam" prim_createDialogIndirectParam :: Addr -> Addr -> Addr -> FunPtr DialogProc -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialogIndirectParam_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialogIndirectParam_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_createDialogIndirectParam_gc_failstring :: Addr -> IO (Addr)

defDlgProc :: MbHWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
defDlgProc arg1 arg2 arg3 arg4 =
  (case arg1 of {
      Nothing -> (return (nullHANDLE));
      (Just arg1) -> (return ((arg1)))
   }) >>= \ (arg1) ->
  prim_defDlgProc arg1 arg2 arg3 arg4
  >>= \  res1  ->
  (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_defDlgProc" prim_defDlgProc :: Addr -> Word32 -> Word32 -> Int32 -> IO (Int32)

endDialog :: HWND -> Int -> IO BOOL
endDialog arg1 arg2 =
  prim_endDialog arg1 arg2
  >>= \ gc_result ->
  access_prim_endDialog_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_endDialog_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_endDialog_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_bool_ res1) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_endDialog" prim_endDialog :: Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_endDialog_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_endDialog_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_endDialog_gc_failstring :: Addr -> IO (Addr)

getDialogBaseUnits :: IO LONG
getDialogBaseUnits =
  prim_getDialogBaseUnits
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_getDialogBaseUnits" prim_getDialogBaseUnits :: IO (Int32)

getDlgCtrlID :: HWND -> IO Int
getDlgCtrlID arg1 =
  prim_getDlgCtrlID arg1
  >>= \ gc_result ->
  access_prim_getDlgCtrlID_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getDlgCtrlID_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getDlgCtrlID_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_getDlgCtrlID" prim_getDlgCtrlID :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgCtrlID_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgCtrlID_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgCtrlID_gc_failstring :: Addr -> IO (Addr)

getDlgItem :: HWND -> Int -> IO HWND
getDlgItem arg1 arg2 =
  prim_getDlgItem arg1 arg2
  >>= \ gc_result ->
  access_prim_getDlgItem_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getDlgItem_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getDlgItem_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_getDlgItem" prim_getDlgItem :: Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgItem_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgItem_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgItem_gc_failstring :: Addr -> IO (Addr)

getDlgItemInt :: HWND -> Int -> Bool -> IO Int
getDlgItemInt arg1 arg2 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_getDlgItemInt arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_getDlgItemInt_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getDlgItemInt_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getDlgItemInt_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_getDlgItemInt" prim_getDlgItemInt :: Addr -> Int -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgItemInt_res1 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgItemInt_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgItemInt_gc_failstring :: Addr -> IO (Addr)

getDlgItemText :: HWND -> Int -> Int -> IO String
getDlgItemText arg1 arg2 arg3 =
  prim_getDlgItemText arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_getDlgItemText_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_getDlgItemText_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getDlgItemText_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ gc_res2) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_getDlgItemText" prim_getDlgItemText :: Addr -> Int -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgItemText_gc_res2 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgItemText_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getDlgItemText_gc_failstring :: Addr -> IO (Addr)

getNextDlgGroupItem :: HWND -> HWND -> BOOL -> IO HWND
getNextDlgGroupItem arg1 arg2 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_getNextDlgGroupItem arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_getNextDlgGroupItem_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getNextDlgGroupItem_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getNextDlgGroupItem_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_getNextDlgGroupItem" prim_getNextDlgGroupItem :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getNextDlgGroupItem_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getNextDlgGroupItem_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getNextDlgGroupItem_gc_failstring :: Addr -> IO (Addr)

getNextDlgTabItem :: HWND -> HWND -> BOOL -> IO HWND
getNextDlgTabItem arg1 arg2 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_getNextDlgTabItem arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_getNextDlgTabItem_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getNextDlgTabItem_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getNextDlgTabItem_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_getNextDlgTabItem" prim_getNextDlgTabItem :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getNextDlgTabItem_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getNextDlgTabItem_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_getNextDlgTabItem_gc_failstring :: Addr -> IO (Addr)

isDialogMessage :: HWND -> LPMSG -> IO BOOL
isDialogMessage arg1 arg2 =
  prim_isDialogMessage arg1 arg2
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_isDialogMessage" prim_isDialogMessage :: Addr -> Addr -> IO (Int)

mapDialogRect :: HWND -> LPRECT -> IO ()
mapDialogRect arg1 arg2 =
  prim_mapDialogRect arg1 arg2
  >>= \ gc_result ->
  access_prim_mapDialogRect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_mapDialogRect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_mapDialogRect" prim_mapDialogRect :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_mapDialogRect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_mapDialogRect_gc_failstring :: Addr -> IO (Addr)

-- No MessageBox* funs in here just yet.

sendDlgItemMessage :: HWND -> Int -> WindowMessage -> WPARAM -> LPARAM -> IO LONG
sendDlgItemMessage arg1 arg2 arg3 arg4 arg5 =
  prim_sendDlgItemMessage arg1 arg2 arg3 arg4 arg5
  >>= \  res1  ->
  (return (res1))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_sendDlgItemMessage" prim_sendDlgItemMessage :: Addr -> Int -> Word32 -> Word32 -> Int32 -> IO (Int32)

setDlgItemInt :: HWND -> Int -> UINT -> BOOL -> IO ()
setDlgItemInt arg1 arg2 arg3 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg4) ->
  prim_setDlgItemInt arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_setDlgItemInt_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setDlgItemInt_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_setDlgItemInt" prim_setDlgItemInt :: Addr -> Int -> Word32 -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_setDlgItemInt_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_setDlgItemInt_gc_failstring :: Addr -> IO (Addr)

setDlgItemText :: HWND -> Int -> String -> IO ()
setDlgItemText arg1 arg2 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg3) ->
  prim_setDlgItemText arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_setDlgItemText_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setDlgItemText_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall "Win32Dialogue_stub_ffi.h prim_setDlgItemText" prim_setDlgItemText :: Addr -> Int -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_setDlgItemText_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Dialogue_stub_ffi.h" access_prim_setDlgItemText_gc_failstring :: Addr -> IO (Addr)

foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_3DLOOK" dS_3DLOOK :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_ABSALIGN" dS_ABSALIGN :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_CENTER" dS_CENTER :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_CENTERMOUSE" dS_CENTERMOUSE :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_CONTEXTHELP" dS_CONTEXTHELP :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_CONTROL" dS_CONTROL :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_FIXEDSYS" dS_FIXEDSYS :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_LOCALEDIT" dS_LOCALEDIT :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_MODALFRAME" dS_MODALFRAME :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_NOFAILCREATE" dS_NOFAILCREATE :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_NOIDLEMSG" dS_NOIDLEMSG :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_SETFONT" dS_SETFONT :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_SETFOREGROUND" dS_SETFOREGROUND :: WindowStyle
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dS_SYSMODAL" dS_SYSMODAL :: WindowStyle

foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dM_GETDEFID" dM_GETDEFID :: WindowMessage
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dM_REPOSITION" dM_REPOSITION :: WindowMessage
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_dM_SETDEFID" dM_SETDEFID :: WindowMessage
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_wM_CTLCOLORDLG" wM_CTLCOLORDLG :: WindowMessage
foreign import  ccall unsafe "Win32Dialogue_stub_ffi.h prim_wM_CTLCOLORMSGBOX" wM_CTLCOLORMSGBOX :: WindowMessage

----------------------------------------------------------------
-- End
----------------------------------------------------------------
