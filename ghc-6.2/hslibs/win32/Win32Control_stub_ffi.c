/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include <commctrl.h>
#include "errors.h"
#include "win32debug.h"
#include "Win32Control_stub_ffi.h"
DWORD prim_bS_PUSHBUTTON()
{ DWORD res1;
  do {res1=BS_PUSHBUTTON;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_DEFPUSHBUTTON()
{ DWORD res1;
  do {res1=BS_DEFPUSHBUTTON;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_CHECKBOX()
{ DWORD res1;
  do {res1=BS_CHECKBOX;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_AUTOCHECKBOX()
{ DWORD res1;
  do {res1=BS_AUTOCHECKBOX;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_RADIOBUTTON()
{ DWORD res1;
  do {res1=BS_RADIOBUTTON;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_3STATE()
{ DWORD res1;
  do {res1=BS_3STATE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_AUTO3STATE()
{ DWORD res1;
  do {res1=BS_AUTO3STATE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_GROUPBOX()
{ DWORD res1;
  do {res1=BS_GROUPBOX;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_AUTORADIOBUTTON()
{ DWORD res1;
  do {res1=BS_AUTORADIOBUTTON;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_OWNERDRAW()
{ DWORD res1;
  do {res1=BS_OWNERDRAW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_LEFTTEXT()
{ DWORD res1;
  do {res1=BS_LEFTTEXT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_bS_USERBUTTON()
{ DWORD res1;
  do {res1=BS_USERBUTTON;
      
      return((DWORD)(res1));} while(0);
}
void* prim_createButton(char * nm,DWORD wstyle,DWORD bstyle,int x,int y,int w,int h,HWND parent,HMENU hmenu,HANDLE handle)
{ static struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND hwnd;int gc_failed;
	    char* gc_failstring;
  do { hwnd = CreateWindow("BUTTON",nm,wstyle|bstyle,x,y,w,h,parent,hmenu,handle,NULL);
      if ((gc_failed = (  hwnd == NULL  ))) {gc_failstring =  ErrorWin("CreateButton")  ;}
      else {gc_failed = 0;}
      gc_result.hwnd = (HWND)(hwnd);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createButton_hwnd(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->hwnd);}
int access_prim_createButton_gc_failed(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createButton_gc_failstring(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
UINT prim_bST_CHECKED()
{ UINT res1;
  do {res1=BST_CHECKED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_bST_INDETERMINATE()
{ UINT res1;
  do {res1=BST_INDETERMINATE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_bST_UNCHECKED()
{ UINT res1;
  do {res1=BST_UNCHECKED;
      
      return((UINT)(res1));} while(0);
}
void* prim_checkDlgButton(HWND arg1,int arg2,UINT arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=CheckDlgButton(arg1,arg2,arg3);
      if ((gc_failed = (  res1 == FALSE  ))) {gc_failstring = ErrorWithCode("CheckDlgButton", res1) ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_checkDlgButton_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_checkDlgButton_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_checkRadioButton(HWND arg1,int arg2,int arg3,int arg4)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=CheckRadioButton(arg1,arg2,arg3,arg4);
      if ((gc_failed = (  res1 == FALSE  ))) {gc_failstring = ErrorWithCode("CheckRadioButton", res1) ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_checkRadioButton_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_checkRadioButton_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_isDlgButtonChecked(HWND arg1,int arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do { BOOL res1=IsDlgButtonChecked(arg1,arg2);
      if ((gc_failed = (  res1 == FALSE  ))) {gc_failstring = ErrorWithCode("IsDlgButtonChecked", res1) ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_isDlgButtonChecked_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_isDlgButtonChecked_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_isDlgButtonChecked_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_cBS_SIMPLE()
{ DWORD res1;
  do {res1=CBS_SIMPLE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_DROPDOWN()
{ DWORD res1;
  do {res1=CBS_DROPDOWN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_DROPDOWNLIST()
{ DWORD res1;
  do {res1=CBS_DROPDOWNLIST;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_OWNERDRAWFIXED()
{ DWORD res1;
  do {res1=CBS_OWNERDRAWFIXED;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_OWNERDRAWVARIABLE()
{ DWORD res1;
  do {res1=CBS_OWNERDRAWVARIABLE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_AUTOHSCROLL()
{ DWORD res1;
  do {res1=CBS_AUTOHSCROLL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_OEMCONVERT()
{ DWORD res1;
  do {res1=CBS_OEMCONVERT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_SORT()
{ DWORD res1;
  do {res1=CBS_SORT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_HASSTRINGS()
{ DWORD res1;
  do {res1=CBS_HASSTRINGS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_NOINTEGRALHEIGHT()
{ DWORD res1;
  do {res1=CBS_NOINTEGRALHEIGHT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_cBS_DISABLENOSCROLL()
{ DWORD res1;
  do {res1=CBS_DISABLENOSCROLL;
      
      return((DWORD)(res1));} while(0);
}
void* prim_createComboBox(char * nm,DWORD wstyle,DWORD cstyle,int x,int y,int w,int h,HWND parent,HMENU hmenu,HANDLE handle)
{ static struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND hwnd;int gc_failed;
	    char* gc_failstring;
  do { hwnd = CreateWindow("COMBOBOX",nm,wstyle|cstyle, x,y,w,h,parent,hmenu,handle,NULL);
      if ((gc_failed = (  hwnd == NULL  ))) {gc_failstring =  ErrorWin("CreateComboBox")  ;}
      else {gc_failed = 0;}
      gc_result.hwnd = (HWND)(hwnd);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createComboBox_hwnd(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->hwnd);}
int access_prim_createComboBox_gc_failed(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createComboBox_gc_failstring(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_eS_LEFT()
{ DWORD res1;
  do {res1=ES_LEFT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_CENTER()
{ DWORD res1;
  do {res1=ES_CENTER;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_RIGHT()
{ DWORD res1;
  do {res1=ES_RIGHT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_MULTILINE()
{ DWORD res1;
  do {res1=ES_MULTILINE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_UPPERCASE()
{ DWORD res1;
  do {res1=ES_UPPERCASE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_LOWERCASE()
{ DWORD res1;
  do {res1=ES_LOWERCASE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_PASSWORD()
{ DWORD res1;
  do {res1=ES_PASSWORD;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_AUTOVSCROLL()
{ DWORD res1;
  do {res1=ES_AUTOVSCROLL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_AUTOHSCROLL()
{ DWORD res1;
  do {res1=ES_AUTOHSCROLL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_NOHIDESEL()
{ DWORD res1;
  do {res1=ES_NOHIDESEL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_OEMCONVERT()
{ DWORD res1;
  do {res1=ES_OEMCONVERT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_READONLY()
{ DWORD res1;
  do {res1=ES_READONLY;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_eS_WANTRETURN()
{ DWORD res1;
  do {res1=ES_WANTRETURN;
      
      return((DWORD)(res1));} while(0);
}
void* prim_createEditWindow(char * nm,DWORD wstyle,DWORD estyle,int x,int y,int w,int h,HWND parent,HMENU hmenu,HANDLE handle)
{ static struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND hwnd;int gc_failed;
	    char* gc_failstring;
  do { hwnd = CreateWindow("EDIT",nm,wstyle|estyle, x,y,w,h,parent,hmenu,handle,NULL);
      if ((gc_failed = (  hwnd == NULL  ))) {gc_failstring =  ErrorWin("CreateEditWindow")  ;}
      else {gc_failed = 0;}
      gc_result.hwnd = (HWND)(hwnd);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createEditWindow_hwnd(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->hwnd);}
int access_prim_createEditWindow_gc_failed(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createEditWindow_gc_failstring(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_lBS_NOTIFY()
{ DWORD res1;
  do {res1=LBS_NOTIFY;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_SORT()
{ DWORD res1;
  do {res1=LBS_SORT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_NOREDRAW()
{ DWORD res1;
  do {res1=LBS_NOREDRAW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_MULTIPLESEL()
{ DWORD res1;
  do {res1=LBS_MULTIPLESEL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_OWNERDRAWFIXED()
{ DWORD res1;
  do {res1=LBS_OWNERDRAWFIXED;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_OWNERDRAWVARIABLE()
{ DWORD res1;
  do {res1=LBS_OWNERDRAWVARIABLE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_HASSTRINGS()
{ DWORD res1;
  do {res1=LBS_HASSTRINGS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_USETABSTOPS()
{ DWORD res1;
  do {res1=LBS_USETABSTOPS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_NOINTEGRALHEIGHT()
{ DWORD res1;
  do {res1=LBS_NOINTEGRALHEIGHT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_MULTICOLUMN()
{ DWORD res1;
  do {res1=LBS_MULTICOLUMN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_WANTKEYBOARDINPUT()
{ DWORD res1;
  do {res1=LBS_WANTKEYBOARDINPUT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_DISABLENOSCROLL()
{ DWORD res1;
  do {res1=LBS_DISABLENOSCROLL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lBS_STANDARD()
{ DWORD res1;
  do {res1=LBS_STANDARD;
      
      return((DWORD)(res1));} while(0);
}
void* prim_createListBox(char * nm,DWORD wstyle,DWORD lstyle,int x,int y,int w,int h,HWND parent,HMENU hmenu,HANDLE handle)
{ static struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND hwnd;int gc_failed;
	    char* gc_failstring;
  do { hwnd = CreateWindow("LISTBOX",nm,wstyle|lstyle,x,y,w,h,parent,hmenu,handle,NULL);
      if ((gc_failed = (  hwnd == NULL  ))) {gc_failstring =  ErrorWin("CreateListBox")  ;}
      else {gc_failed = 0;}
      gc_result.hwnd = (HWND)(hwnd);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createListBox_hwnd(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->hwnd);}
int access_prim_createListBox_gc_failed(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createListBox_gc_failstring(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_sBS_HORZ()
{ DWORD res1;
  do {res1=SBS_HORZ;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sBS_TOPALIGN()
{ DWORD res1;
  do {res1=SBS_TOPALIGN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sBS_BOTTOMALIGN()
{ DWORD res1;
  do {res1=SBS_BOTTOMALIGN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sBS_VERT()
{ DWORD res1;
  do {res1=SBS_VERT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sBS_LEFTALIGN()
{ DWORD res1;
  do {res1=SBS_LEFTALIGN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sBS_RIGHTALIGN()
{ DWORD res1;
  do {res1=SBS_RIGHTALIGN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sBS_SIZEBOX()
{ DWORD res1;
  do {res1=SBS_SIZEBOX;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sBS_SIZEBOXTOPLEFTALIGN()
{ DWORD res1;
  do {res1=SBS_SIZEBOXTOPLEFTALIGN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sBS_SIZEBOXBOTTOMRIGHTALIGN()
{ DWORD res1;
  do {res1=SBS_SIZEBOXBOTTOMRIGHTALIGN;
      
      return((DWORD)(res1));} while(0);
}
void* prim_createScrollbar(char * nm,DWORD wstyle,DWORD sstyle,int x,int y,int w,int h,HWND parent,HMENU hmenu,HANDLE handle)
{ static struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND hwnd;int gc_failed;
	    char* gc_failstring;
  do { hwnd = CreateWindow("SCROLLBAR",nm,wstyle|sstyle, x,y,w,h,parent,hmenu,handle,NULL);
      if ((gc_failed = (  hwnd == NULL  ))) {gc_failstring =  ErrorWin("CreateScrollbar")  ;}
      else {gc_failed = 0;}
      gc_result.hwnd = (HWND)(hwnd);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createScrollbar_hwnd(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->hwnd);}
int access_prim_createScrollbar_gc_failed(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createScrollbar_gc_failstring(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_sS_LEFT()
{ DWORD res1;
  do {res1=SS_LEFT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_CENTER()
{ DWORD res1;
  do {res1=SS_CENTER;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_RIGHT()
{ DWORD res1;
  do {res1=SS_RIGHT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_ICON()
{ DWORD res1;
  do {res1=SS_ICON;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_BLACKRECT()
{ DWORD res1;
  do {res1=SS_BLACKRECT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_GRAYRECT()
{ DWORD res1;
  do {res1=SS_GRAYRECT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_WHITERECT()
{ DWORD res1;
  do {res1=SS_WHITERECT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_BLACKFRAME()
{ DWORD res1;
  do {res1=SS_BLACKFRAME;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_GRAYFRAME()
{ DWORD res1;
  do {res1=SS_GRAYFRAME;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_WHITEFRAME()
{ DWORD res1;
  do {res1=SS_WHITEFRAME;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_SIMPLE()
{ DWORD res1;
  do {res1=SS_SIMPLE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_LEFTNOWORDWRAP()
{ DWORD res1;
  do {res1=SS_LEFTNOWORDWRAP;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sS_NOPREFIX()
{ DWORD res1;
  do {res1=SS_NOPREFIX;
      
      return((DWORD)(res1));} while(0);
}
void* prim_createStaticWindow(char * nm,DWORD wstyle,DWORD sstyle,int x,int y,int w,int h,HWND parent,HMENU hmenu,HANDLE handle)
{ static struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND hwnd;int gc_failed;
	    char* gc_failstring;
  do { hwnd = CreateWindow("STATIC",nm,wstyle|sstyle, x,y,w,h,parent,hmenu,handle,NULL);
      if ((gc_failed = (  hwnd == NULL  ))) {gc_failstring =  ErrorWin("CreateStaticWindow")  ;}
      else {gc_failed = 0;}
      gc_result.hwnd = (HWND)(hwnd);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createStaticWindow_hwnd(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->hwnd);}
int access_prim_createStaticWindow_gc_failed(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createStaticWindow_gc_failstring(void *ptr){ return(((struct {HsPtr hwnd;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_pBM_DELTAPOS()
{ DWORD res1;
  do {res1=PBM_DELTAPOS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pBM_SETPOS()
{ DWORD res1;
  do {res1=PBM_SETPOS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pBM_SETRANGE()
{ DWORD res1;
  do {res1=PBM_SETRANGE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pBM_SETSTEP()
{ DWORD res1;
  do {res1=PBM_SETSTEP;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pBM_STEPIT()
{ DWORD res1;
  do {res1=PBM_STEPIT;
      
      return((DWORD)(res1));} while(0);
}
