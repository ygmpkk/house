module Win32Control where

import StdDIS
import Win32Types
import GDITypes
import Win32Window
import Win32WinMessage


-- Bindings to the various standard Win32 controls


-- == Command buttons

type ButtonStyle   = WindowStyle

foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_PUSHBUTTON" bS_PUSHBUTTON :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_DEFPUSHBUTTON" bS_DEFPUSHBUTTON :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_CHECKBOX" bS_CHECKBOX :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_AUTOCHECKBOX" bS_AUTOCHECKBOX :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_RADIOBUTTON" bS_RADIOBUTTON :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_3STATE" bS_3STATE :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_AUTO3STATE" bS_AUTO3STATE :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_GROUPBOX" bS_GROUPBOX :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_AUTORADIOBUTTON" bS_AUTORADIOBUTTON :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_OWNERDRAW" bS_OWNERDRAW :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_LEFTTEXT" bS_LEFTTEXT :: ButtonStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bS_USERBUTTON" bS_USERBUTTON :: ButtonStyle

createButton :: String -> WindowStyle -> ButtonStyle -> MbPos -> MbPos -> MbPos -> MbPos -> MbHWND -> MbHMENU -> HANDLE -> IO HWND
createButton gc_arg1 wstyle bstyle x y w h parent hmenu handle =
  (marshall_string_ gc_arg1) >>= \ (nm) ->
  (case x of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just x) -> (return ((x)))
   }) >>= \ (x) ->
  (case y of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just y) -> (return ((y)))
   }) >>= \ (y) ->
  (case w of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just w) -> (return ((w)))
   }) >>= \ (w) ->
  (case h of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just h) -> (return ((h)))
   }) >>= \ (h) ->
  (case parent of {
      Nothing -> (return (nullHANDLE));
      (Just parent) -> (return ((parent)))
   }) >>= \ (parent) ->
  (case hmenu of {
      Nothing -> (return (nullHANDLE));
      (Just hmenu) -> (return ((hmenu)))
   }) >>= \ (hmenu) ->
  prim_createButton nm wstyle bstyle x y w h parent hmenu handle
  >>= \ gc_result ->
  access_prim_createButton_hwnd (gc_result :: Addr) >>= \ hwnd ->
  access_prim_createButton_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createButton_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (hwnd))
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_createButton" prim_createButton :: Addr -> Word32 -> Word32 -> Int -> Int -> Int -> Int -> Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createButton_hwnd :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createButton_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createButton_gc_failstring :: Addr -> IO (Addr)

type ButtonState = UINT

foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bST_CHECKED" bST_CHECKED :: ButtonState
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bST_INDETERMINATE" bST_INDETERMINATE :: ButtonState
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_bST_UNCHECKED" bST_UNCHECKED :: ButtonState

checkDlgButton :: HWND -> Int -> ButtonState -> IO ()
checkDlgButton arg1 arg2 arg3 =
  prim_checkDlgButton arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_checkDlgButton_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_checkDlgButton_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_checkDlgButton" prim_checkDlgButton :: Addr -> Int -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_checkDlgButton_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_checkDlgButton_gc_failstring :: Addr -> IO (Addr)

checkRadioButton :: HWND -> Int -> Int -> Int -> IO ()
checkRadioButton arg1 arg2 arg3 arg4 =
  prim_checkRadioButton arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_checkRadioButton_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_checkRadioButton_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_checkRadioButton" prim_checkRadioButton :: Addr -> Int -> Int -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_checkRadioButton_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_checkRadioButton_gc_failstring :: Addr -> IO (Addr)

isDlgButtonChecked :: HWND -> Int -> IO ButtonState
isDlgButtonChecked arg1 arg2 =
  prim_isDlgButtonChecked arg1 arg2
  >>= \ gc_result ->
  access_prim_isDlgButtonChecked_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_isDlgButtonChecked_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_isDlgButtonChecked_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_isDlgButtonChecked" prim_isDlgButtonChecked :: Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_isDlgButtonChecked_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_isDlgButtonChecked_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_isDlgButtonChecked_gc_failstring :: Addr -> IO (Addr)


-- == ComboBoxes aka. pop up list boxes/selectors.

type ComboBoxStyle = WindowStyle

foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_SIMPLE" cBS_SIMPLE :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_DROPDOWN" cBS_DROPDOWN :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_DROPDOWNLIST" cBS_DROPDOWNLIST :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_OWNERDRAWFIXED" cBS_OWNERDRAWFIXED :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_OWNERDRAWVARIABLE" cBS_OWNERDRAWVARIABLE :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_AUTOHSCROLL" cBS_AUTOHSCROLL :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_OEMCONVERT" cBS_OEMCONVERT :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_SORT" cBS_SORT :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_HASSTRINGS" cBS_HASSTRINGS :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_NOINTEGRALHEIGHT" cBS_NOINTEGRALHEIGHT :: ComboBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_cBS_DISABLENOSCROLL" cBS_DISABLENOSCROLL :: ComboBoxStyle

createComboBox :: String -> WindowStyle -> ComboBoxStyle -> MbPos -> MbPos -> MbPos -> MbPos -> HWND -> MbHMENU -> HANDLE -> IO HWND
createComboBox gc_arg1 wstyle cstyle x y w h parent hmenu handle =
  (marshall_string_ gc_arg1) >>= \ (nm) ->
  (case x of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just x) -> (return ((x)))
   }) >>= \ (x) ->
  (case y of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just y) -> (return ((y)))
   }) >>= \ (y) ->
  (case w of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just w) -> (return ((w)))
   }) >>= \ (w) ->
  (case h of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just h) -> (return ((h)))
   }) >>= \ (h) ->
  (case hmenu of {
      Nothing -> (return (nullHANDLE));
      (Just hmenu) -> (return ((hmenu)))
   }) >>= \ (hmenu) ->
  prim_createComboBox nm wstyle cstyle x y w h parent hmenu handle
  >>= \ gc_result ->
  access_prim_createComboBox_hwnd (gc_result :: Addr) >>= \ hwnd ->
  access_prim_createComboBox_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createComboBox_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (hwnd))
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_createComboBox" prim_createComboBox :: Addr -> Word32 -> Word32 -> Int -> Int -> Int -> Int -> Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createComboBox_hwnd :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createComboBox_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createComboBox_gc_failstring :: Addr -> IO (Addr)

-- see comment about freeing windowNames in Win32Window.createWindow
-- %end free(nm)


--- == Edit controls

----------------------------------------------------------------

type EditStyle = WindowStyle

foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_LEFT" eS_LEFT :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_CENTER" eS_CENTER :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_RIGHT" eS_RIGHT :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_MULTILINE" eS_MULTILINE :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_UPPERCASE" eS_UPPERCASE :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_LOWERCASE" eS_LOWERCASE :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_PASSWORD" eS_PASSWORD :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_AUTOVSCROLL" eS_AUTOVSCROLL :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_AUTOHSCROLL" eS_AUTOHSCROLL :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_NOHIDESEL" eS_NOHIDESEL :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_OEMCONVERT" eS_OEMCONVERT :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_READONLY" eS_READONLY :: EditStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_eS_WANTRETURN" eS_WANTRETURN :: EditStyle

createEditWindow :: String -> WindowStyle -> EditStyle -> MbPos -> MbPos -> MbPos -> MbPos -> HWND -> MbHMENU -> HANDLE -> IO HWND
createEditWindow gc_arg1 wstyle estyle x y w h parent hmenu handle =
  (marshall_string_ gc_arg1) >>= \ (nm) ->
  (case x of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just x) -> (return ((x)))
   }) >>= \ (x) ->
  (case y of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just y) -> (return ((y)))
   }) >>= \ (y) ->
  (case w of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just w) -> (return ((w)))
   }) >>= \ (w) ->
  (case h of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just h) -> (return ((h)))
   }) >>= \ (h) ->
  (case hmenu of {
      Nothing -> (return (nullHANDLE));
      (Just hmenu) -> (return ((hmenu)))
   }) >>= \ (hmenu) ->
  prim_createEditWindow nm wstyle estyle x y w h parent hmenu handle
  >>= \ gc_result ->
  access_prim_createEditWindow_hwnd (gc_result :: Addr) >>= \ hwnd ->
  access_prim_createEditWindow_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createEditWindow_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (hwnd))
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_createEditWindow" prim_createEditWindow :: Addr -> Word32 -> Word32 -> Int -> Int -> Int -> Int -> Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createEditWindow_hwnd :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createEditWindow_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createEditWindow_gc_failstring :: Addr -> IO (Addr)

-- see comment about freeing windowNames in Win32Window.createWindow
-- %end free(nm)

-- == List boxes


----------------------------------------------------------------

type ListBoxStyle   = WindowStyle

foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_NOTIFY" lBS_NOTIFY :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_SORT" lBS_SORT :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_NOREDRAW" lBS_NOREDRAW :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_MULTIPLESEL" lBS_MULTIPLESEL :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_OWNERDRAWFIXED" lBS_OWNERDRAWFIXED :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_OWNERDRAWVARIABLE" lBS_OWNERDRAWVARIABLE :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_HASSTRINGS" lBS_HASSTRINGS :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_USETABSTOPS" lBS_USETABSTOPS :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_NOINTEGRALHEIGHT" lBS_NOINTEGRALHEIGHT :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_MULTICOLUMN" lBS_MULTICOLUMN :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_WANTKEYBOARDINPUT" lBS_WANTKEYBOARDINPUT :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_DISABLENOSCROLL" lBS_DISABLENOSCROLL :: ListBoxStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_lBS_STANDARD" lBS_STANDARD :: ListBoxStyle

createListBox :: String -> WindowStyle -> ListBoxStyle -> MbPos -> MbPos -> MbPos -> MbPos -> HWND -> MbHMENU -> HANDLE -> IO HWND
createListBox gc_arg1 wstyle lstyle x y w h parent hmenu handle =
  (marshall_string_ gc_arg1) >>= \ (nm) ->
  (case x of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just x) -> (return ((x)))
   }) >>= \ (x) ->
  (case y of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just y) -> (return ((y)))
   }) >>= \ (y) ->
  (case w of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just w) -> (return ((w)))
   }) >>= \ (w) ->
  (case h of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just h) -> (return ((h)))
   }) >>= \ (h) ->
  (case hmenu of {
      Nothing -> (return (nullHANDLE));
      (Just hmenu) -> (return ((hmenu)))
   }) >>= \ (hmenu) ->
  prim_createListBox nm wstyle lstyle x y w h parent hmenu handle
  >>= \ gc_result ->
  access_prim_createListBox_hwnd (gc_result :: Addr) >>= \ hwnd ->
  access_prim_createListBox_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createListBox_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (hwnd))
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_createListBox" prim_createListBox :: Addr -> Word32 -> Word32 -> Int -> Int -> Int -> Int -> Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createListBox_hwnd :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createListBox_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createListBox_gc_failstring :: Addr -> IO (Addr)

-- see comment about freeing windowNames in Win32Window.createWindow
-- %end free(nm)

-- == Scrollbars


----------------------------------------------------------------

type ScrollbarStyle = WindowStyle

foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sBS_HORZ" sBS_HORZ :: ScrollbarStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sBS_TOPALIGN" sBS_TOPALIGN :: ScrollbarStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sBS_BOTTOMALIGN" sBS_BOTTOMALIGN :: ScrollbarStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sBS_VERT" sBS_VERT :: ScrollbarStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sBS_LEFTALIGN" sBS_LEFTALIGN :: ScrollbarStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sBS_RIGHTALIGN" sBS_RIGHTALIGN :: ScrollbarStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sBS_SIZEBOX" sBS_SIZEBOX :: ScrollbarStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sBS_SIZEBOXTOPLEFTALIGN" sBS_SIZEBOXTOPLEFTALIGN :: ScrollbarStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sBS_SIZEBOXBOTTOMRIGHTALIGN" sBS_SIZEBOXBOTTOMRIGHTALIGN :: ScrollbarStyle

createScrollbar :: String -> WindowStyle -> ScrollbarStyle -> MbPos -> MbPos -> MbPos -> MbPos -> HWND -> MbHMENU -> HANDLE -> IO HWND
createScrollbar gc_arg1 wstyle sstyle x y w h parent hmenu handle =
  (marshall_string_ gc_arg1) >>= \ (nm) ->
  (case x of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just x) -> (return ((x)))
   }) >>= \ (x) ->
  (case y of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just y) -> (return ((y)))
   }) >>= \ (y) ->
  (case w of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just w) -> (return ((w)))
   }) >>= \ (w) ->
  (case h of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just h) -> (return ((h)))
   }) >>= \ (h) ->
  (case hmenu of {
      Nothing -> (return (nullHANDLE));
      (Just hmenu) -> (return ((hmenu)))
   }) >>= \ (hmenu) ->
  prim_createScrollbar nm wstyle sstyle x y w h parent hmenu handle
  >>= \ gc_result ->
  access_prim_createScrollbar_hwnd (gc_result :: Addr) >>= \ hwnd ->
  access_prim_createScrollbar_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createScrollbar_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (hwnd))
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_createScrollbar" prim_createScrollbar :: Addr -> Word32 -> Word32 -> Int -> Int -> Int -> Int -> Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createScrollbar_hwnd :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createScrollbar_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createScrollbar_gc_failstring :: Addr -> IO (Addr)

-- see comment about freeing windowNames in Win32Window.createWindow
-- %end free(nm)

-- == Static controls aka. labels


----------------------------------------------------------------

type StaticControlStyle = WindowStyle

foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_LEFT" sS_LEFT :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_CENTER" sS_CENTER :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_RIGHT" sS_RIGHT :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_ICON" sS_ICON :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_BLACKRECT" sS_BLACKRECT :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_GRAYRECT" sS_GRAYRECT :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_WHITERECT" sS_WHITERECT :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_BLACKFRAME" sS_BLACKFRAME :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_GRAYFRAME" sS_GRAYFRAME :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_WHITEFRAME" sS_WHITEFRAME :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_SIMPLE" sS_SIMPLE :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_LEFTNOWORDWRAP" sS_LEFTNOWORDWRAP :: StaticControlStyle
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_sS_NOPREFIX" sS_NOPREFIX :: StaticControlStyle

createStaticWindow :: String -> WindowStyle -> StaticControlStyle -> MbPos -> MbPos -> MbPos -> MbPos -> HWND -> MbHMENU -> HANDLE -> IO HWND
createStaticWindow gc_arg1 wstyle sstyle x y w h parent hmenu handle =
  (marshall_string_ gc_arg1) >>= \ (nm) ->
  (case x of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just x) -> (return ((x)))
   }) >>= \ (x) ->
  (case y of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just y) -> (return ((y)))
   }) >>= \ (y) ->
  (case w of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just w) -> (return ((w)))
   }) >>= \ (w) ->
  (case h of {
      Nothing -> (return (cW_USEDEFAULT));
      (Just h) -> (return ((h)))
   }) >>= \ (h) ->
  (case hmenu of {
      Nothing -> (return (nullHANDLE));
      (Just hmenu) -> (return ((hmenu)))
   }) >>= \ (hmenu) ->
  prim_createStaticWindow nm wstyle sstyle x y w h parent hmenu handle
  >>= \ gc_result ->
  access_prim_createStaticWindow_hwnd (gc_result :: Addr) >>= \ hwnd ->
  access_prim_createStaticWindow_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createStaticWindow_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (hwnd))
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_createStaticWindow" prim_createStaticWindow :: Addr -> Word32 -> Word32 -> Int -> Int -> Int -> Int -> Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createStaticWindow_hwnd :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createStaticWindow_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Control_stub_ffi.h" access_prim_createStaticWindow_gc_failstring :: Addr -> IO (Addr)

-- see comment about freeing windowNames in Win32Window.createWindow
-- %end free(nm)

{- UNTESTED - leave out

type CommonControl   = Addr
%dis commonControl x = addr ({LPCTSTR} x)

%const CommonControl
% [ ToolTipsControl = {TOOLTIPS_CLASS}
% , TrackBarControl = {TRACKBAR_CLASS}
% , UpDownControl  = {UPDOWN_CLASS}
% , ProgressBarControl = {PROGRESS_CLASS}
% , HotKeyControl  = {HOTKEY_CLASS}
% , AnimateControl     = {ANIMATE_CLASS}
% , StatusControl     =  {STATUSCLASSNAME}
% , HeaderControl     =  {WC_HEADER}
% , ListViewControl   =  {WC_LISTVIEW}
% , TabControl        =  {WC_TABCONTROL}
% , TreeViewControl   =  {WC_TREEVIEW}
% , MonthCalControl    = {MONTHCAL_CLASS}
% , DateTimePickControl = {DATETIMEPICK_CLASS}
% , ReBarControl      =  {REBARCLASSNAME}
-- Not supplied in mingw-20001111
--% , ComboBoxExControl =  {WC_COMBOBOXEX}
--% , IPAddressControl  =  {WC_IPADDRESS}
--% , PageScrollerControl = {WC_PAGESCROLLER}
% ]

%fun createCommonControl
%    :: CommonControl -> WindowStyle -> String -> WindowStyle
%    -> MbPos -> MbPos -> MbPos -> MbPos 
%    -> MbHWND -> MbHMENU -> HANDLE 
%    -> IO HWND
%call (commonControl c) (windowStyle estyle) (string nm) (windowStyle wstyle)
%     (mbPos x) (mbPos y) (mbPos w) (mbPos h) 
%     (mbHWND parent) (mbHMENU hmenu) (hANDLE handle)
%code hwnd = CreateWindowEx(estyle, c, nm,wstyle,x,y,w,h,parent,hmenu,handle,NULL);
%fail { hwnd == NULL } { ErrorWin("CreateCommonControl") }
%result (hWND hwnd)

%fun InitCommonControls :: IO ()

-}

foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_pBM_DELTAPOS" pBM_DELTAPOS :: WindowMessage
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_pBM_SETPOS" pBM_SETPOS :: WindowMessage
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_pBM_SETRANGE" pBM_SETRANGE :: WindowMessage
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_pBM_SETSTEP" pBM_SETSTEP :: WindowMessage
foreign import  ccall unsafe "Win32Control_stub_ffi.h prim_pBM_STEPIT" pBM_STEPIT :: WindowMessage

-- % , PBM_GETRANGE
-- % , PBM_GETPOS
-- % , PBM_SETBARCOLOR
-- % , PBM_SETBKCOLOR
-- % , PBM_SETRANGE32
