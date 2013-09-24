/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "WndProc.h"
#include "Win32Window_stub_ffi.h"
LPCTSTR prim_mkClassName(char * arg1)
{ do {
      return((LPCTSTR)(arg1));} while(0);
}
UINT prim_cS_VREDRAW()
{ UINT res1;
  do {res1=CS_VREDRAW;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_HREDRAW()
{ UINT res1;
  do {res1=CS_HREDRAW;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_OWNDC()
{ UINT res1;
  do {res1=CS_OWNDC;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_CLASSDC()
{ UINT res1;
  do {res1=CS_CLASSDC;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_PARENTDC()
{ UINT res1;
  do {res1=CS_PARENTDC;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_SAVEBITS()
{ UINT res1;
  do {res1=CS_SAVEBITS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_DBLCLKS()
{ UINT res1;
  do {res1=CS_DBLCLKS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_BYTEALIGNCLIENT()
{ UINT res1;
  do {res1=CS_BYTEALIGNCLIENT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_BYTEALIGNWINDOW()
{ UINT res1;
  do {res1=CS_BYTEALIGNWINDOW;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_NOCLOSE()
{ UINT res1;
  do {res1=CS_NOCLOSE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cS_GLOBALCLASS()
{ UINT res1;
  do {res1=CS_GLOBALCLASS;
      
      return((UINT)(res1));} while(0);
}
void* prim_marshall_wndClass_(UINT style,HINSTANCE hInstance,HICON hIcon,HCURSOR hCursor,HBRUSH hbrBackground,LPCTSTR lpszMenuName,LPCTSTR lpszClassName)
{ static struct {HsPtr c;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WNDCLASS* c;int gc_failed;
	      char* gc_failstring;
  do { c = (WNDCLASS*) malloc(sizeof(WNDCLASS));
     if (c) { 
	  c->style 	   = style;
	  c->hInstance 	   = hInstance;
	  c->hIcon 	   = hIcon;
	  c->hCursor 	   = hCursor;
	  c->hbrBackground = hbrBackground;
	  c->lpszMenuName  = lpszMenuName;
	  c->lpszClassName = lpszClassName;
     };
      if ((gc_failed = ( c==0 ))) {gc_failstring =  MallocError("marshall_WNDCLASS")  ;}
      else {gc_failed = 0;}
      gc_result.c = (WNDCLASS*)(c);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WNDCLASS* access_prim_marshall_wndClass__c(void *ptr){ return(((struct {HsPtr c;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->c);}
int access_prim_marshall_wndClass__gc_failed(void *ptr){ return(((struct {HsPtr c;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_marshall_wndClass__gc_failstring(void *ptr){ return(((struct {HsPtr c;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
ATOM prim_registerClass(WNDCLASS * arg1)
{ ATOM res1;
  do { arg1->lpfnWndProc = genericWndProc;
     arg1->cbClsExtra = 0;
     arg1->cbWndExtra =0;
     res1 = RegisterClass(arg1);
      
      return((ATOM)(res1));} while(0);
}
void prim_unregisterClass(LPCTSTR arg1,HINSTANCE arg2)
{ do {UnregisterClass(arg1, arg2);
      ;} while(0);
}
DWORD prim_wS_OVERLAPPED()
{ DWORD res1;
  do {res1=WS_OVERLAPPED;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_POPUP()
{ DWORD res1;
  do {res1=WS_POPUP;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_CHILD()
{ DWORD res1;
  do {res1=WS_CHILD;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_CLIPSIBLINGS()
{ DWORD res1;
  do {res1=WS_CLIPSIBLINGS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_CLIPCHILDREN()
{ DWORD res1;
  do {res1=WS_CLIPCHILDREN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_VISIBLE()
{ DWORD res1;
  do {res1=WS_VISIBLE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_DISABLED()
{ DWORD res1;
  do {res1=WS_DISABLED;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_MINIMIZE()
{ DWORD res1;
  do {res1=WS_MINIMIZE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_MAXIMIZE()
{ DWORD res1;
  do {res1=WS_MAXIMIZE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_CAPTION()
{ DWORD res1;
  do {res1=WS_CAPTION;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_BORDER()
{ DWORD res1;
  do {res1=WS_BORDER;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_DLGFRAME()
{ DWORD res1;
  do {res1=WS_DLGFRAME;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_VSCROLL()
{ DWORD res1;
  do {res1=WS_VSCROLL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_HSCROLL()
{ DWORD res1;
  do {res1=WS_HSCROLL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_SYSMENU()
{ DWORD res1;
  do {res1=WS_SYSMENU;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_THICKFRAME()
{ DWORD res1;
  do {res1=WS_THICKFRAME;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_MINIMIZEBOX()
{ DWORD res1;
  do {res1=WS_MINIMIZEBOX;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_MAXIMIZEBOX()
{ DWORD res1;
  do {res1=WS_MAXIMIZEBOX;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_GROUP()
{ DWORD res1;
  do {res1=WS_GROUP;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_TABSTOP()
{ DWORD res1;
  do {res1=WS_TABSTOP;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_OVERLAPPEDWINDOW()
{ DWORD res1;
  do {res1=WS_OVERLAPPEDWINDOW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_POPUPWINDOW()
{ DWORD res1;
  do {res1=WS_POPUPWINDOW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_CHILDWINDOW()
{ DWORD res1;
  do {res1=WS_CHILDWINDOW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_TILED()
{ DWORD res1;
  do {res1=WS_TILED;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_ICONIC()
{ DWORD res1;
  do {res1=WS_ICONIC;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_SIZEBOX()
{ DWORD res1;
  do {res1=WS_SIZEBOX;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_TILEDWINDOW()
{ DWORD res1;
  do {res1=WS_TILEDWINDOW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_DLGMODALFRAME()
{ DWORD res1;
  do {res1=WS_EX_DLGMODALFRAME;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_NOPARENTNOTIFY()
{ DWORD res1;
  do {res1=WS_EX_NOPARENTNOTIFY;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_TOPMOST()
{ DWORD res1;
  do {res1=WS_EX_TOPMOST;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_ACCEPTFILES()
{ DWORD res1;
  do {res1=WS_EX_ACCEPTFILES;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_TRANSPARENT()
{ DWORD res1;
  do {res1=WS_EX_TRANSPARENT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_MDICHILD()
{ DWORD res1;
  do {res1=WS_EX_MDICHILD;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_TOOLWINDOW()
{ DWORD res1;
  do {res1=WS_EX_TOOLWINDOW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_WINDOWEDGE()
{ DWORD res1;
  do {res1=WS_EX_WINDOWEDGE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_CLIENTEDGE()
{ DWORD res1;
  do {res1=WS_EX_CLIENTEDGE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_CONTEXTHELP()
{ DWORD res1;
  do {res1=WS_EX_CONTEXTHELP;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_RIGHT()
{ DWORD res1;
  do {res1=WS_EX_RIGHT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_LEFT()
{ DWORD res1;
  do {res1=WS_EX_LEFT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_RTLREADING()
{ DWORD res1;
  do {res1=WS_EX_RTLREADING;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_LTRREADING()
{ DWORD res1;
  do {res1=WS_EX_LTRREADING;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_LEFTSCROLLBAR()
{ DWORD res1;
  do {res1=WS_EX_LEFTSCROLLBAR;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_RIGHTSCROLLBAR()
{ DWORD res1;
  do {res1=WS_EX_RIGHTSCROLLBAR;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_CONTROLPARENT()
{ DWORD res1;
  do {res1=WS_EX_CONTROLPARENT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_STATICEDGE()
{ DWORD res1;
  do {res1=WS_EX_STATICEDGE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_APPWINDOW()
{ DWORD res1;
  do {res1=WS_EX_APPWINDOW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_OVERLAPPEDWINDOW()
{ DWORD res1;
  do {res1=WS_EX_OVERLAPPEDWINDOW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wS_EX_PALETTEWINDOW()
{ DWORD res1;
  do {res1=WS_EX_PALETTEWINDOW;
      
      return((DWORD)(res1));} while(0);
}
int prim_cW_USEDEFAULT()
{ int res1;
  do {res1=CW_USEDEFAULT;
      
      return((int)(res1));} while(0);
}
void prim_setWindowClosure(HWND hwnd,void * closure)
{ do { SetWindowLong(hwnd, GWL_USERDATA, (LONG)closure);
      ;} while(0);
}
void* prim_createWindow(LPCTSTR name,char * windowName,DWORD style,int x,int y,int width,int height,HWND hwndParent,HMENU hmenu,HINSTANCE hinst,void * closure)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = CreateWindow(name, windowName,style,x,y,width,height,hwndParent,hmenu,hinst,NULL);
     if (res1 != NULL) { SetWindowLong(res1, GWL_USERDATA, (LONG)closure); };
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("CreateWindow") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createWindow_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createWindow_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createWindow_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createWindowEx(DWORD estyle,LPCTSTR cls,char * wname,DWORD wstyle,int x,int y,int nWidth,int nHeight,HWND hwndParent,HMENU hmenu,HINSTANCE hinstance,void * closure)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = CreateWindowEx(estyle, cls, wname, wstyle,x,y,nWidth,nHeight,hwndParent,hmenu,hinstance,NULL);
     if (res1 != NULL) { SetWindowLong(res1, GWL_USERDATA, (LONG)closure); };
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("CreateWindowEx") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createWindowEx_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createWindowEx_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createWindowEx_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
LRESULT prim_defWindowProc(HWND arg1,DWORD arg2,WPARAM arg3,LPARAM arg4)
{ LRESULT res1;
  do { LRESULT res1 = DefWindowProc(arg1,arg2,arg3,arg4);
      
      return((LRESULT)(res1));} while(0);
}
void* prim_getClientRect(HWND arg1)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  RECT res1;int gc_failed;
	    char* gc_failstring;
  do { BOOL success = GetClientRect(arg1,&res1);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("GetClientRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((res1).left);
      gc_result.gc_res2 = (LONG)((res1).top);
      gc_result.gc_res3 = (LONG)((res1).right);
      gc_result.gc_res4 = (LONG)((res1).bottom);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_getClientRect_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_getClientRect_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
LONG access_prim_getClientRect_gc_res3(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
LONG access_prim_getClientRect_gc_res4(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res4);}
int access_prim_getClientRect_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getClientRect_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getWindowRect(HWND arg1)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  RECT res1;int gc_failed;
	    char* gc_failstring;
  do { BOOL success = GetWindowRect(arg1,&res1);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("GetWindowRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((res1).left);
      gc_result.gc_res2 = (LONG)((res1).top);
      gc_result.gc_res3 = (LONG)((res1).right);
      gc_result.gc_res4 = (LONG)((res1).bottom);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_getWindowRect_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_getWindowRect_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
LONG access_prim_getWindowRect_gc_res3(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
LONG access_prim_getWindowRect_gc_res4(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res4);}
int access_prim_getWindowRect_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getWindowRect_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_invalidateRect(HWND arg1,LPRECT arg2,int arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = InvalidateRect(arg1,arg2,arg3);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("InvalidateRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_invalidateRect_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_invalidateRect_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_screenToClient(HWND arg1,LONG gc_arg2,LONG gc_arg3)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;POINT scr;
  (scr).x = (LONG)gc_arg2; (scr).y = (LONG)gc_arg3;
  do { BOOL success = ScreenToClient(arg1, &scr);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("ScreenToClient")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((scr).x);
      gc_result.gc_res2 = (LONG)((scr).y);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_screenToClient_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_screenToClient_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_screenToClient_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_screenToClient_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_clientToScreen(HWND hwnd,LONG gc_arg2,LONG gc_arg3)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;POINT cli;
  (cli).x = (LONG)gc_arg2; (cli).y = (LONG)gc_arg3;
  do { BOOL success = ClientToScreen(hwnd,&cli);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("ClientToScreen")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((cli).x);
      gc_result.gc_res2 = (LONG)((cli).y);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_clientToScreen_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_clientToScreen_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_clientToScreen_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_clientToScreen_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setWindowText(HWND arg1,char * arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = SetWindowText(arg1,arg2);
      if ((gc_failed = ( !success ))) {gc_failstring = ErrorString("SetWindowText") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
       free(arg2);
      return(&gc_result);} while(0);
}
int access_prim_setWindowText_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setWindowText_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_sizeofPAINTSTRUCT()
{ do {
      return((DWORD)( sizeof(PAINTSTRUCT) ));} while(0);
}
void* prim_beginPaint(HWND arg1,LPPAINTSTRUCT arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HDC res1;int gc_failed;
	   char* gc_failstring;
  do { HDC res1 = BeginPaint(arg1,arg2);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorString("BeginPaint") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HDC)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HDC access_prim_beginPaint_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_beginPaint_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_beginPaint_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void prim_endPaint(HWND arg1,LPPAINTSTRUCT arg2)
{ do { EndPaint((HWND)arg1,(LPPAINTSTRUCT)arg2);
      ;} while(0);
}
DWORD prim_sW_HIDE()
{ DWORD res1;
  do {res1=SW_HIDE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_SHOWNORMAL()
{ DWORD res1;
  do {res1=SW_SHOWNORMAL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_SHOWMINIMIZED()
{ DWORD res1;
  do {res1=SW_SHOWMINIMIZED;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_SHOWMAXIMIZED()
{ DWORD res1;
  do {res1=SW_SHOWMAXIMIZED;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_MAXIMIZE()
{ DWORD res1;
  do {res1=SW_MAXIMIZE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_SHOWNOACTIVATE()
{ DWORD res1;
  do {res1=SW_SHOWNOACTIVATE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_SHOW()
{ DWORD res1;
  do {res1=SW_SHOW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_MINIMIZE()
{ DWORD res1;
  do {res1=SW_MINIMIZE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_SHOWMINNOACTIVE()
{ DWORD res1;
  do {res1=SW_SHOWMINNOACTIVE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_SHOWNA()
{ DWORD res1;
  do {res1=SW_SHOWNA;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sW_RESTORE()
{ DWORD res1;
  do {res1=SW_RESTORE;
      
      return((DWORD)(res1));} while(0);
}
int prim_showWindow(HWND arg1,DWORD arg2)
{ int res1;
  do { BOOL res1 = ShowWindow(arg1,arg2);
      
      return((int)(res1));} while(0);
}
void* prim_adjustWindowRect(LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5,DWORD arg2,int arg3)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;RECT arg1;
  (arg1).left = (LONG)gc_arg2; (arg1).top = (LONG)gc_arg3; (arg1).right = (LONG)gc_arg4; (arg1).bottom = (LONG)gc_arg5;
  do { BOOL success = AdjustWindowRect(&arg1, arg2, arg3);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("AdjustWindowRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((arg1).left);
      gc_result.gc_res2 = (LONG)((arg1).top);
      gc_result.gc_res3 = (LONG)((arg1).right);
      gc_result.gc_res4 = (LONG)((arg1).bottom);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_adjustWindowRect_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_adjustWindowRect_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
LONG access_prim_adjustWindowRect_gc_res3(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
LONG access_prim_adjustWindowRect_gc_res4(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res4);}
int access_prim_adjustWindowRect_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_adjustWindowRect_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_adjustWindowRectEx(LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5,DWORD arg2,int arg3,DWORD arg4)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;RECT arg1;
  (arg1).left = (LONG)gc_arg2; (arg1).top = (LONG)gc_arg3; (arg1).right = (LONG)gc_arg4; (arg1).bottom = (LONG)gc_arg5;
  do { BOOL success = AdjustWindowRectEx(&arg1, arg2, arg3, arg4);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("AdjustWindowRectEx")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((arg1).left);
      gc_result.gc_res2 = (LONG)((arg1).top);
      gc_result.gc_res3 = (LONG)((arg1).right);
      gc_result.gc_res4 = (LONG)((arg1).bottom);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_adjustWindowRectEx_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_adjustWindowRectEx_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
LONG access_prim_adjustWindowRectEx_gc_res3(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
LONG access_prim_adjustWindowRectEx_gc_res4(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res4);}
int access_prim_adjustWindowRectEx_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_adjustWindowRectEx_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_anyPopup()
{ int res1;
  do {res1 = AnyPopup();
      
      return((int)(res1));} while(0);
}
void* prim_arrangeIconicWindows(HWND arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { UINT success = ArrangeIconicWindows(arg1);
      if ((gc_failed = ( success == 0 ))) {gc_failstring =  ErrorWin("ArrangeIconicWindows")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_arrangeIconicWindows_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_arrangeIconicWindows_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_beginDeferWindowPos(int arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HDWP res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = BeginDeferWindowPos(arg1);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring =  ErrorWin("BeginDeferWindowPos")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HDWP)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HDWP access_prim_beginDeferWindowPos_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_beginDeferWindowPos_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_beginDeferWindowPos_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_bringWindowToTop(HWND arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = BringWindowToTop(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("BringWindowToTop")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_bringWindowToTop_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bringWindowToTop_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_childWindowFromPoint(HWND hwnd,LONG gc_arg2,LONG gc_arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;POINT p;
  (p).x = (LONG)gc_arg2; (p).y = (LONG)gc_arg3;
  do { BOOL success = ChildWindowFromPoint(hwnd,p);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("ChildWindowFromPoint")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_childWindowFromPoint_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_childWindowFromPoint_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_childWindowFromPoint_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_childWindowFromPointEx(HWND hwnd,LONG gc_arg2,LONG gc_arg3,DWORD arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;POINT p;
  (p).x = (LONG)gc_arg2; (p).y = (LONG)gc_arg3;
  do { BOOL success = ChildWindowFromPointEx(hwnd,p,arg2);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("ChildWindowFromPointEx")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_childWindowFromPointEx_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_childWindowFromPointEx_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_childWindowFromPointEx_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_closeWindow(HWND arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DestroyWindow(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("CloseWindow")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_closeWindow_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_closeWindow_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_deferWindowPos(HDWP arg1,HWND arg2,HWND arg3,int arg4,int arg5,int arg6,int arg7,UINT arg8)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HDWP res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = DeferWindowPos(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring =  ErrorWin("DeferWindowPos")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HDWP)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HDWP access_prim_deferWindowPos_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_deferWindowPos_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_deferWindowPos_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_destroyWindow(HWND arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DestroyWindow(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("DestroyWindow")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_destroyWindow_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_destroyWindow_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_endDeferWindowPos(HDWP arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = EndDeferWindowPos(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("EndDeferWindowPos")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_endDeferWindowPos_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_endDeferWindowPos_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
HWND prim_findWindow(char * arg1,char * arg2)
{ HWND res1;
  do {res1 = FindWindow(arg1, arg2);
      
      return((HWND)(res1));} while(0);
}
HWND prim_findWindowEx(HWND arg1,HWND arg2,char * arg3,char * arg4)
{ HWND res1;
  do {res1 = FindWindowEx(arg1, arg2, arg3, arg4);
      
      return((HWND)(res1));} while(0);
}
int prim_flashWindow(HWND arg1,int arg2)
{ int res1;
  do {res1 = FlashWindow(arg1, arg2);
      
      return((int)(res1));} while(0);
}
void* prim_moveWindow(HWND arg1,int arg2,int arg3,int arg4,int arg5,int arg6)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = MoveWindow(arg1,arg2,arg3,arg4,arg5,arg6);
      if ((gc_failed = (  success==0  ))) {gc_failstring =  ErrorWin("MoveWindow")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_moveWindow_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_moveWindow_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
HWND prim_getDesktopWindow()
{ HWND res1;
  do {res1 = GetDesktopWindow();
      
      return((HWND)(res1));} while(0);
}
HWND prim_getForegroundWindow()
{ HWND res1;
  do {res1 = GetForegroundWindow();
      
      return((HWND)(res1));} while(0);
}
void* prim_getParent(HWND arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do { HWND res1 = GetParent(arg1);
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring =  ErrorWin("GetParent")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_getParent_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getParent_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getParent_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getTopWindow(HWND arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do { HWND res1 = GetTopWindow(arg1);
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring =  ErrorWin("GetTopWindow")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_getTopWindow_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getTopWindow_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getTopWindow_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
UINT prim_sWP_NOSIZE()
{ UINT res1;
  do {res1=SWP_NOSIZE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_NOMOVE()
{ UINT res1;
  do {res1=SWP_NOMOVE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_NOZORDER()
{ UINT res1;
  do {res1=SWP_NOZORDER;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_NOREDRAW()
{ UINT res1;
  do {res1=SWP_NOREDRAW;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_NOACTIVATE()
{ UINT res1;
  do {res1=SWP_NOACTIVATE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_FRAMECHANGED()
{ UINT res1;
  do {res1=SWP_FRAMECHANGED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_SHOWWINDOW()
{ UINT res1;
  do {res1=SWP_SHOWWINDOW;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_HIDEWINDOW()
{ UINT res1;
  do {res1=SWP_HIDEWINDOW;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_NOCOPYBITS()
{ UINT res1;
  do {res1=SWP_NOCOPYBITS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_NOOWNERZORDER()
{ UINT res1;
  do {res1=SWP_NOOWNERZORDER;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_NOSENDCHANGING()
{ UINT res1;
  do {res1=SWP_NOSENDCHANGING;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_DRAWFRAME()
{ UINT res1;
  do {res1=SWP_DRAWFRAME;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sWP_NOREPOSITION()
{ UINT res1;
  do {res1=SWP_NOREPOSITION;
      
      return((UINT)(res1));} while(0);
}
DWORD prim_dCX_WINDOW()
{ DWORD res1;
  do {res1=DCX_WINDOW;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dCX_CACHE()
{ DWORD res1;
  do {res1=DCX_CACHE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dCX_CLIPCHILDREN()
{ DWORD res1;
  do {res1=DCX_CLIPCHILDREN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dCX_CLIPSIBLINGS()
{ DWORD res1;
  do {res1=DCX_CLIPSIBLINGS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dCX_PARENTCLIP()
{ DWORD res1;
  do {res1=DCX_PARENTCLIP;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dCX_EXCLUDERGN()
{ DWORD res1;
  do {res1=DCX_EXCLUDERGN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dCX_INTERSECTRGN()
{ DWORD res1;
  do {res1=DCX_INTERSECTRGN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dCX_LOCKWINDOWUPDATE()
{ DWORD res1;
  do {res1=DCX_LOCKWINDOWUPDATE;
      
      return((DWORD)(res1));} while(0);
}
void* prim_getDCEx(HWND arg1,HRGN arg2,DWORD arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HDC res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = GetDCEx(arg1, arg2, arg3);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorString("GetDCEx") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HDC)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HDC access_prim_getDCEx_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getDCEx_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getDCEx_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getDC(HWND arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HDC res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = GetDC(arg1);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorString("GetDC") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HDC)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HDC access_prim_getDC_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getDC_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getDC_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getWindowDC(HWND arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HDC res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = GetWindowDC(arg1);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorString("GetWindowDC") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HDC)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HDC access_prim_getWindowDC_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getWindowDC_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getWindowDC_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_releaseDC(HWND arg1,HDC arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = ReleaseDC(arg1,arg2);
      if ((gc_failed = ( !success ))) {gc_failstring = ErrorString("ReleaseDC") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_releaseDC_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_releaseDC_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getDCOrgEx(HDC arg1)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  POINT res1;int gc_failed;
	     char* gc_failstring;
  do { BOOL success = GetDCOrgEx(arg1,&res1);
      if ((gc_failed = ( !success ))) {gc_failstring = ErrorString("GetDCOrgEx") ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((res1).x);
      gc_result.gc_res2 = (LONG)((res1).y);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_getDCOrgEx_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_getDCOrgEx_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_getDCOrgEx_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getDCOrgEx_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_hideCaret(HWND arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = HideCaret(arg1);
      if ((gc_failed = ( !success ))) {gc_failstring = ErrorString("HideCaret") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_hideCaret_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_hideCaret_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_showCaret(HWND arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = ShowCaret(arg1);
      if ((gc_failed = ( !success ))) {gc_failstring = ErrorString("ShowCaret") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_showCaret_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_showCaret_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createCaret(HWND arg1,HBITMAP arg2,INT arg3,INT arg4)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = CreateCaret(arg1,arg2,arg3,arg4);
      if ((gc_failed = ( !success ))) {gc_failstring = ErrorWin("CreateCaret") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_createCaret_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createCaret_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_destroyCaret()
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DestroyCaret();
      if ((gc_failed = ( !success ))) {gc_failstring = ErrorWin("DestroyCaret") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_destroyCaret_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_destroyCaret_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getCaretPos()
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  POINT res1;int gc_failed;
	     char* gc_failstring;
  do {  BOOL success = GetCaretPos(&res1);
      if ((gc_failed = ( !success ))) {gc_failstring = ErrorWin("GetCaretPos") ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((res1).x);
      gc_result.gc_res2 = (LONG)((res1).y);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_getCaretPos_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_getCaretPos_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_getCaretPos_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getCaretPos_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setCaretPos(LONG gc_arg2,LONG gc_arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;POINT arg1;
  (arg1).x = (LONG)gc_arg2; (arg1).y = (LONG)gc_arg3;
  do { BOOL success = SetCaretPos(arg1.x, arg1.y);
      if ((gc_failed = ( !success ))) {gc_failstring = ErrorWin("SetCaretPos") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_setCaretPos_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setCaretPos_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getMessage(HWND arg1)
{ static struct {HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { static MSG msg;
     BOOL success = GetMessage(&msg, arg1, 0, 0);
      if ((gc_failed = (  success == -1  ))) {gc_failstring = ErrorString("GetMessage") ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (MSG *)(&msg);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
MSG * access_prim_getMessage_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_getMessage_gc_failed(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getMessage_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getMessage2(HWND arg1)
{ static struct {HsPtr gc_res1;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { static MSG msg;
     BOOL success = GetMessage(&msg, arg1, 0, 0);
      if ((gc_failed = (  success == -1  ))) {gc_failstring = ErrorString("GetMessage") ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (MSG *)(&msg);
      gc_result.gc_res3 = (int)(success !=0);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
MSG * access_prim_getMessage2_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_getMessage2_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
int access_prim_getMessage2_gc_failed(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getMessage2_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_peekMessage(HWND arg1,UINT arg2,UINT arg3,UINT arg4)
{ static struct {HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { static MSG msg;
     BOOL success = PeekMessage(&msg, arg1, arg2, arg3,arg4);
      if ((gc_failed = (  success == -1  ))) {gc_failstring = ErrorString("PeekMessage") ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (MSG *)(&msg);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
MSG * access_prim_peekMessage_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_peekMessage_gc_failed(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_peekMessage_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
BOOL prim_translateMessage(MSG * arg1)
{ BOOL res1;
  do { BOOL res1 = TranslateMessage(arg1);
      
      return((BOOL)(res1));} while(0);
}
void* prim_updateWindow(HWND arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = UpdateWindow(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("UpdateWindow")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_updateWindow_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_updateWindow_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
LONG prim_dispatchMessage(MSG * arg1)
{ LONG res1;
  do { LONG res1 = DispatchMessage(arg1);
      
      return((LONG)(res1));} while(0);
}
LRESULT prim_sendMessage(HWND arg1,DWORD arg2,WPARAM arg3,LPARAM arg4)
{ LRESULT res1;
  do { LRESULT res1 = SendMessage(arg1,arg2,arg3,arg4);
      
      return((LRESULT)(res1));} while(0);
}
