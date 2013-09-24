/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Menu_stub_ffi.h"
void* prim_checkMenuItem(HMENU arg1,UINT arg2,UINT arg3)
{ static struct {HsInt gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { DWORD rv = CheckMenuItem(arg1,arg2,arg3);
      if ((gc_failed = ( rv == (DWORD)-1 ))) {gc_failstring =  ErrorString("CheckMenuItem");  ;}
      else {gc_failed = 0;}
      gc_result.gc_res2 = (int)(rv == MF_CHECKED);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_checkMenuItem_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_checkMenuItem_gc_failed(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_checkMenuItem_gc_failstring(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_checkMenuRadioItem(HMENU arg1,UINT arg2,UINT arg3,UINT arg4,UINT arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = CheckMenuRadioItem(arg1,arg2,arg3,arg4,arg5);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("CheckMenuRadioItem");  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_checkMenuRadioItem_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_checkMenuRadioItem_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createMenu()
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HMENU res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = CreateMenu();
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("CreateMenu");  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HMENU)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HMENU access_prim_createMenu_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createMenu_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createMenu_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createPopupMenu()
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HMENU res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = CreatePopupMenu();
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("CreatePopupMenu");  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HMENU)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HMENU access_prim_createPopupMenu_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createPopupMenu_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createPopupMenu_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_drawMenuBar(HWND arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DrawMenuBar(arg1);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("DrawMenuBar")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_drawMenuBar_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_drawMenuBar_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_enableMenuItem(HMENU arg1,UINT arg2,UINT arg3)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do { DWORD res1 = EnableMenuItem(arg1,arg2,arg3);
      if ((gc_failed = ( res1 == (DWORD)-1 ))) {gc_failstring =  ErrorString("EnableMenuItem");  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_enableMenuItem_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_enableMenuItem_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_enableMenuItem_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
UINT prim_gMDI_USEDISABLED()
{ UINT res1;
  do {res1=GMDI_USEDISABLED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMDI_GOINTOPOPUPS()
{ UINT res1;
  do {res1=GMDI_GOINTOPOPUPS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_BYCOMMAND()
{ UINT res1;
  do {res1=MF_BYCOMMAND;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_BYPOSITION()
{ UINT res1;
  do {res1=MF_BYPOSITION;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_INSERT()
{ UINT res1;
  do {res1=MF_INSERT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_CHANGE()
{ UINT res1;
  do {res1=MF_CHANGE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_APPEND()
{ UINT res1;
  do {res1=MF_APPEND;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_DELETE()
{ UINT res1;
  do {res1=MF_DELETE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_REMOVE()
{ UINT res1;
  do {res1=MF_REMOVE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_USECHECKBITMAPS()
{ UINT res1;
  do {res1=MF_USECHECKBITMAPS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_POPUP()
{ UINT res1;
  do {res1=MF_POPUP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_SYSMENU()
{ UINT res1;
  do {res1=MF_SYSMENU;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_HELP()
{ UINT res1;
  do {res1=MF_HELP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_MOUSESELECT()
{ UINT res1;
  do {res1=MF_MOUSESELECT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mF_END()
{ UINT res1;
  do {res1=MF_END;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFT_STRING()
{ UINT res1;
  do {res1=MFT_STRING;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFT_BITMAP()
{ UINT res1;
  do {res1=MFT_BITMAP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFT_MENUBARBREAK()
{ UINT res1;
  do {res1=MFT_MENUBARBREAK;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFT_MENUBREAK()
{ UINT res1;
  do {res1=MFT_MENUBREAK;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFT_OWNERDRAW()
{ UINT res1;
  do {res1=MFT_OWNERDRAW;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFT_RADIOCHECK()
{ UINT res1;
  do {res1=MFT_RADIOCHECK;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFT_SEPARATOR()
{ UINT res1;
  do {res1=MFT_SEPARATOR;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFT_RIGHTORDER()
{ UINT res1;
  do {res1=MFT_RIGHTORDER;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFT_RIGHTJUSTIFY()
{ UINT res1;
  do {res1=MFT_RIGHTJUSTIFY;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFS_GRAYED()
{ UINT res1;
  do {res1=MFS_GRAYED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFS_DISABLED()
{ UINT res1;
  do {res1=MFS_DISABLED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFS_CHECKED()
{ UINT res1;
  do {res1=MFS_CHECKED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFS_HILITE()
{ UINT res1;
  do {res1=MFS_HILITE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFS_ENABLED()
{ UINT res1;
  do {res1=MFS_ENABLED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFS_UNCHECKED()
{ UINT res1;
  do {res1=MFS_UNCHECKED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFS_UNHILITE()
{ UINT res1;
  do {res1=MFS_UNHILITE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mFS_DEFAULT()
{ UINT res1;
  do {res1=MFS_DEFAULT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_LEFTBUTTON()
{ UINT res1;
  do {res1=TPM_LEFTBUTTON;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_RIGHTBUTTON()
{ UINT res1;
  do {res1=TPM_RIGHTBUTTON;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_LEFTALIGN()
{ UINT res1;
  do {res1=TPM_LEFTALIGN;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_CENTERALIGN()
{ UINT res1;
  do {res1=TPM_CENTERALIGN;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_RIGHTALIGN()
{ UINT res1;
  do {res1=TPM_RIGHTALIGN;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_TOPALIGN()
{ UINT res1;
  do {res1=TPM_TOPALIGN;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_VCENTERALIGN()
{ UINT res1;
  do {res1=TPM_VCENTERALIGN;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_BOTTOMALIGN()
{ UINT res1;
  do {res1=TPM_BOTTOMALIGN;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_HORIZONTAL()
{ UINT res1;
  do {res1=TPM_HORIZONTAL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_VERTICAL()
{ UINT res1;
  do {res1=TPM_VERTICAL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_NONOTIFY()
{ UINT res1;
  do {res1=TPM_NONOTIFY;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tPM_RETURNCMD()
{ UINT res1;
  do {res1=TPM_RETURNCMD;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_SIZE()
{ UINT res1;
  do {res1=SC_SIZE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_MOVE()
{ UINT res1;
  do {res1=SC_MOVE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_MINIMIZE()
{ UINT res1;
  do {res1=SC_MINIMIZE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_MAXIMIZE()
{ UINT res1;
  do {res1=SC_MAXIMIZE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_NEXTWINDOW()
{ UINT res1;
  do {res1=SC_NEXTWINDOW;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_PREVWINDOW()
{ UINT res1;
  do {res1=SC_PREVWINDOW;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_CLOSE()
{ UINT res1;
  do {res1=SC_CLOSE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_VSCROLL()
{ UINT res1;
  do {res1=SC_VSCROLL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_HSCROLL()
{ UINT res1;
  do {res1=SC_HSCROLL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_MOUSEMENU()
{ UINT res1;
  do {res1=SC_MOUSEMENU;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_KEYMENU()
{ UINT res1;
  do {res1=SC_KEYMENU;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_ARRANGE()
{ UINT res1;
  do {res1=SC_ARRANGE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_RESTORE()
{ UINT res1;
  do {res1=SC_RESTORE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_TASKLIST()
{ UINT res1;
  do {res1=SC_TASKLIST;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_SCREENSAVE()
{ UINT res1;
  do {res1=SC_SCREENSAVE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_HOTKEY()
{ UINT res1;
  do {res1=SC_HOTKEY;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_DEFAULT()
{ UINT res1;
  do {res1=SC_DEFAULT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_MONITORPOWER()
{ UINT res1;
  do {res1=SC_MONITORPOWER;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_CONTEXTHELP()
{ UINT res1;
  do {res1=SC_CONTEXTHELP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sC_SEPARATOR()
{ UINT res1;
  do {res1=SC_SEPARATOR;
      
      return((UINT)(res1));} while(0);
}
int prim_isMenu(HMENU arg1)
{ do { BOOL success = IsMenu(arg1);
      
      return((int)(success));} while(0);
}
HMENU prim_getSystemMenu(HWND arg1,int arg2)
{ HMENU res1;
  do {res1 = GetSystemMenu(arg1, arg2);
      
      return((HMENU)(res1));} while(0);
}
HMENU prim_getMenu(HWND arg1)
{ HMENU res1;
  do {res1 = GetMenu(arg1);
      
      return((HMENU)(res1));} while(0);
}
void* prim_getMenuDefaultItem(HMENU arg1,int arg2,UINT arg3)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do { int res1 = GetMenuDefaultItem(arg1,arg2,arg3);
      if ((gc_failed = ( res1==-1 ))) {gc_failstring =  ErrorString("GetMenuDefaultItem")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_getMenuDefaultItem_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getMenuDefaultItem_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getMenuDefaultItem_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getMenuState(HMENU arg1,UINT arg2,UINT arg3)
{ static struct {HsWord32 res;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res;int gc_failed;
	   char* gc_failstring;
  do { UINT res = GetMenuState(arg1,arg2,arg3);
      if ((gc_failed = ( (int)res=-1 ))) {gc_failstring =  ErrorString("GetMenuState")  ;}
      else {gc_failed = 0;}
      gc_result.res = (UINT)(res);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_getMenuState_res(void *ptr){ return(((struct {HsWord32 res;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res);}
int access_prim_getMenuState_gc_failed(void *ptr){ return(((struct {HsWord32 res;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getMenuState_gc_failstring(void *ptr){ return(((struct {HsWord32 res;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
HMENU prim_getSubMenu(HMENU arg1,UINT arg2)
{ HMENU res1;
  do {res1 = GetSubMenu(arg1, arg2);
      
      return((HMENU)(res1));} while(0);
}
void* prim_setMenu(HWND arg1,HMENU arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = SetMenu(arg1,arg2);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("SetMenu")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_setMenu_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setMenu_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getMenuItemCount(HMENU arg1)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do { int res1 = GetMenuItemCount(arg1);
      if ((gc_failed = ( res1==-1 ))) {gc_failstring =  ErrorString("GetMenuItemCount")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_getMenuItemCount_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getMenuItemCount_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getMenuItemCount_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getMenuItemID(HMENU arg1,UINT arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do { UINT res1 = GetMenuItemCount(arg1);
      if ((gc_failed = ( res1==(UINT)-1 ))) {gc_failstring =  ErrorString("GetMenuItemID")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_getMenuItemID_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getMenuItemID_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getMenuItemID_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_unravelItemInfo(void * arg1)
{ static struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;} gc_result;
  do { MENUITEMINFO* res1=arg1;
      gc_result.gc_res1 = (UINT)(res1->fType);
      gc_result.gc_res2 = (UINT)(res1->fState);
      gc_result.gc_res3 = (UINT)(res1->wID);
      gc_result.gc_res4 = (HMENU)(res1->hSubMenu);
      gc_result.gc_res5 = (HBITMAP)(res1->hbmpChecked);
      gc_result.gc_res6 = (HBITMAP)(res1->hbmpUnchecked);
      gc_result.gc_res7 = (DWORD)(res1->dwItemData);
      gc_result.gc_res9 = (char *)(res1->dwTypeData);
      gc_result.gc_res10 = (int)(res1->cch);
      
      return(&gc_result);} while(0);
}
UINT access_prim_unravelItemInfo_gc_res1(void *ptr){ return(((struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;}*) ptr)->gc_res1);}
UINT access_prim_unravelItemInfo_gc_res2(void *ptr){ return(((struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;}*) ptr)->gc_res2);}
UINT access_prim_unravelItemInfo_gc_res3(void *ptr){ return(((struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;}*) ptr)->gc_res3);}
HMENU access_prim_unravelItemInfo_gc_res4(void *ptr){ return(((struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;}*) ptr)->gc_res4);}
HBITMAP access_prim_unravelItemInfo_gc_res5(void *ptr){ return(((struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;}*) ptr)->gc_res5);}
HBITMAP access_prim_unravelItemInfo_gc_res6(void *ptr){ return(((struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;}*) ptr)->gc_res6);}
DWORD access_prim_unravelItemInfo_gc_res7(void *ptr){ return(((struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;}*) ptr)->gc_res7);}
char * access_prim_unravelItemInfo_gc_res9(void *ptr){ return(((struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;}*) ptr)->gc_res9);}
int access_prim_unravelItemInfo_gc_res10(void *ptr){ return(((struct {HsWord32 gc_res1;HsWord32 gc_res2;HsWord32 gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;HsWord32 gc_res7;HsPtr gc_res9;HsInt gc_res10;}*) ptr)->gc_res10);}
void* prim_mallocMenuItemInfo()
{ static struct {HsPtr mp;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  MENUITEMINFO* mp;int gc_failed;
		   char* gc_failstring;
  do { mp = (MENUITEMINFO*) malloc(sizeof(MENUITEMINFO));
     if (mp!=NULL) mp->cbSize = sizeof(MENUITEMINFO);
      if ((gc_failed = ( mp==NULL ))) {gc_failstring =  MallocError("mallocMenuItemInfo")  ;}
      else {gc_failed = 0;}
      gc_result.mp = (MENUITEMINFO*)(mp);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
MENUITEMINFO* access_prim_mallocMenuItemInfo_mp(void *ptr){ return(((struct {HsPtr mp;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->mp);}
int access_prim_mallocMenuItemInfo_gc_failed(void *ptr){ return(((struct {HsPtr mp;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_mallocMenuItemInfo_gc_failstring(void *ptr){ return(((struct {HsPtr mp;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void prim_assignMenuItemInfo(void * arg1,UINT arg2,UINT arg3,UINT arg4,HMENU arg5,HBITMAP arg6,HBITMAP arg7,DWORD arg8,void * arg9,int arg10)
{ do { MENUITEMINFO* ptr;
     ptr=(MENUITEMINFO*)arg1;
     ptr->fType         = arg2;
     ptr->fState        = arg3;
     ptr->wID           = arg4;
     ptr->hSubMenu      = arg5;
     ptr->hbmpChecked   = arg6;
     ptr->hbmpUnchecked = arg7;
     ptr->dwItemData    = arg8;
     ptr->dwTypeData    = arg9;
     ptr->cch           = arg10;
      ;} while(0);
}
UINT prim_mIIM_CHECKMARKS()
{ UINT res1;
  do {res1=MIIM_CHECKMARKS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mIIM_DATA()
{ UINT res1;
  do {res1=MIIM_DATA;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mIIM_ID()
{ UINT res1;
  do {res1=MIIM_ID;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mIIM_STATE()
{ UINT res1;
  do {res1=MIIM_STATE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mIIM_SUBMENU()
{ UINT res1;
  do {res1=MIIM_SUBMENU;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mIIM_TYPE()
{ UINT res1;
  do {res1=MIIM_TYPE;
      
      return((UINT)(res1));} while(0);
}
void* prim_getMenuItemInfo(HMENU arg1,UINT arg2,int arg3,UINT arg4)
{ static struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { MENUITEMINFO* mi_info;
     BOOL success;
     mi_info = malloc(sizeof(MENUITEMINFO));
     if (mi_info == NULL) {
        success = FALSE;
     } else {
        mi_info->cbSize = sizeof(MENUITEMINFO);
        mi_info->fMask  = arg4;
        success = GetMenuItemInfo(arg1,arg2,arg3,mi_info);
     };
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("GetMenuItemInfo")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res2 = (void *)(mi_info);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void * access_prim_getMenuItemInfo_gc_res2(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_getMenuItemInfo_gc_failed(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getMenuItemInfo_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getMenuItemRect(HWND arg1,HMENU arg2,UINT arg3)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { RECT r; BOOL success = GetMenuItemRect(arg1,arg2,arg3,&r);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("GetMenuItemRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((r).left);
      gc_result.gc_res2 = (LONG)((r).top);
      gc_result.gc_res3 = (LONG)((r).right);
      gc_result.gc_res4 = (LONG)((r).bottom);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_getMenuItemRect_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_getMenuItemRect_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
LONG access_prim_getMenuItemRect_gc_res3(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
LONG access_prim_getMenuItemRect_gc_res4(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res4);}
int access_prim_getMenuItemRect_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getMenuItemRect_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_hiliteMenuItem(HWND arg1,HMENU arg2,UINT arg3,UINT arg4)
{ do { BOOL success = HiliteMenuItem(arg1,arg2,arg3,arg4);
      
      return((int)(success));} while(0);
}
void* prim_insertMenuItem(HMENU arg1,UINT arg2,int arg3,void * arg4)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = InsertMenuItem(arg1,arg2,arg3,arg4);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("InsertMenuItem")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_insertMenuItem_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_insertMenuItem_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_loadMenu(HINSTANCE arg1,LPCSTR arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HMENU res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = LoadMenu(arg1, arg2);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("LoadMenu")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HMENU)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HMENU access_prim_loadMenu_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_loadMenu_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_loadMenu_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
UINT prim_menuItemFromPoint(HWND arg1,HMENU arg2,LONG gc_arg2,LONG gc_arg3)
{ UINT res1;POINT arg3;
  (arg3).x = (LONG)gc_arg2; (arg3).y = (LONG)gc_arg3;
  do { UINT res1=MenuItemFromPoint(arg1,arg2,arg3);
      
      return((UINT)(res1));} while(0);
}
void* prim_setMenuDefaultItem(HMENU arg1,UINT arg2,int arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = SetMenuDefaultItem(arg1,arg2,arg3);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("SetMenuDefaultItem")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_setMenuDefaultItem_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setMenuDefaultItem_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setMenuItemBitmaps(HMENU arg1,UINT arg2,UINT arg3,HBITMAP arg4,HBITMAP arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = SetMenuItemBitmaps(arg1,arg2,arg3,arg4,arg5);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("SetMenuItemBitmaps")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_setMenuItemBitmaps_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setMenuItemBitmaps_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_destroyMenu(HMENU arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DestroyMenu(arg1);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("DestroyMenu")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_destroyMenu_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_destroyMenu_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_deleteMenu(HMENU arg1,UINT arg2,UINT arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DeleteMenu(arg1,arg2,arg3);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("DeleteMenu")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_deleteMenu_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_deleteMenu_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setMenuItemInfo(HMENU arg1,UINT arg2,int arg3,UINT arg4,void * arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success;
     ((MENUITEMINFO*)arg5)->fMask = arg4;
     success = SetMenuItemInfo(arg1,arg2,arg3,arg5);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("SetMenuItemInfo")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_setMenuItemInfo_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setMenuItemInfo_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_trackPopupMenu(HMENU arg1,UINT arg2,int arg3,int arg4,HWND arg5,LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;RECT arg6;
  (arg6).left = (LONG)gc_arg2; (arg6).top = (LONG)gc_arg3; (arg6).right = (LONG)gc_arg4; (arg6).bottom = (LONG)gc_arg5;
  do { BOOL success = TrackPopupMenu(arg1,arg2,arg3,arg4,0,arg5,&arg6);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("TrackPopupMenu")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_trackPopupMenu_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_trackPopupMenu_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_trackPopupMenuEx(HMENU arg1,UINT arg2,int arg3,int arg4,HWND arg5,LPRECT arg6)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { TPMPARAMS* ptpmp;
     BOOL success;
     if (arg6 == NULL) 
        ptpmp = NULL;
     else {
        ptpmp = malloc(sizeof(TPMPARAMS));
        if (ptpmp != NULL) {
           ptpmp->cbSize=sizeof(TPMPARAMS);
           ptpmp->rcExclude.left  = arg6->left;
           ptpmp->rcExclude.top   = arg6->top;
           ptpmp->rcExclude.right = arg6->right;
           ptpmp->rcExclude.bottom = arg6->bottom;
       }
     }
     success = TrackPopupMenuEx(arg1,arg2,arg3,arg4,arg5,ptpmp);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("TrackPopupMenuEx")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_trackPopupMenuEx_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_trackPopupMenuEx_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_appendMenu(HMENU arg1,UINT arg2,UINT arg3,char * arg4)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = AppendMenu(arg1,arg2,arg3,arg4);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("AppendMenu")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_appendMenu_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_appendMenu_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_insertMenu(HMENU arg1,UINT arg2,UINT arg3,UINT arg4,char * arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = InsertMenu(arg1,arg2,arg3,arg4,arg5);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("InsertMenu")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_insertMenu_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_insertMenu_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_modifyMenu(HMENU arg1,UINT arg2,UINT arg3,UINT arg4,char * arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = ModifyMenu(arg1,arg2,arg3,arg4,arg5);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("ModifyMenu")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_modifyMenu_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_modifyMenu_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_removeMenu(HMENU arg1,UINT arg2,UINT arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = RemoveMenu(arg1,arg2,arg3);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("RemoveMenu")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_removeMenu_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_removeMenu_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
