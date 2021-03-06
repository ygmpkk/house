#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "WndProc.h"
#include "HsFFI.h"
extern LPCTSTR prim_mkClassName(char * arg1);
extern UINT prim_cS_VREDRAW();
extern UINT prim_cS_HREDRAW();
extern UINT prim_cS_OWNDC();
extern UINT prim_cS_CLASSDC();
extern UINT prim_cS_PARENTDC();
extern UINT prim_cS_SAVEBITS();
extern UINT prim_cS_DBLCLKS();
extern UINT prim_cS_BYTEALIGNCLIENT();
extern UINT prim_cS_BYTEALIGNWINDOW();
extern UINT prim_cS_NOCLOSE();
extern UINT prim_cS_GLOBALCLASS();
extern void* prim_marshall_wndClass_(UINT style,HINSTANCE hInstance,HICON hIcon,HCURSOR hCursor,HBRUSH hbrBackground,LPCTSTR lpszMenuName,LPCTSTR lpszClassName);
extern WNDCLASS* access_prim_marshall_wndClass__c(HsPtr);
extern int access_prim_marshall_wndClass__gc_failed(HsPtr);
extern void* access_prim_marshall_wndClass__gc_failstring(HsPtr);
extern ATOM prim_registerClass(WNDCLASS * arg1);
extern void prim_unregisterClass(LPCTSTR arg1,HINSTANCE arg2);
extern DWORD prim_wS_OVERLAPPED();
extern DWORD prim_wS_POPUP();
extern DWORD prim_wS_CHILD();
extern DWORD prim_wS_CLIPSIBLINGS();
extern DWORD prim_wS_CLIPCHILDREN();
extern DWORD prim_wS_VISIBLE();
extern DWORD prim_wS_DISABLED();
extern DWORD prim_wS_MINIMIZE();
extern DWORD prim_wS_MAXIMIZE();
extern DWORD prim_wS_CAPTION();
extern DWORD prim_wS_BORDER();
extern DWORD prim_wS_DLGFRAME();
extern DWORD prim_wS_VSCROLL();
extern DWORD prim_wS_HSCROLL();
extern DWORD prim_wS_SYSMENU();
extern DWORD prim_wS_THICKFRAME();
extern DWORD prim_wS_MINIMIZEBOX();
extern DWORD prim_wS_MAXIMIZEBOX();
extern DWORD prim_wS_GROUP();
extern DWORD prim_wS_TABSTOP();
extern DWORD prim_wS_OVERLAPPEDWINDOW();
extern DWORD prim_wS_POPUPWINDOW();
extern DWORD prim_wS_CHILDWINDOW();
extern DWORD prim_wS_TILED();
extern DWORD prim_wS_ICONIC();
extern DWORD prim_wS_SIZEBOX();
extern DWORD prim_wS_TILEDWINDOW();
extern DWORD prim_wS_EX_DLGMODALFRAME();
extern DWORD prim_wS_EX_NOPARENTNOTIFY();
extern DWORD prim_wS_EX_TOPMOST();
extern DWORD prim_wS_EX_ACCEPTFILES();
extern DWORD prim_wS_EX_TRANSPARENT();
extern DWORD prim_wS_EX_MDICHILD();
extern DWORD prim_wS_EX_TOOLWINDOW();
extern DWORD prim_wS_EX_WINDOWEDGE();
extern DWORD prim_wS_EX_CLIENTEDGE();
extern DWORD prim_wS_EX_CONTEXTHELP();
extern DWORD prim_wS_EX_RIGHT();
extern DWORD prim_wS_EX_LEFT();
extern DWORD prim_wS_EX_RTLREADING();
extern DWORD prim_wS_EX_LTRREADING();
extern DWORD prim_wS_EX_LEFTSCROLLBAR();
extern DWORD prim_wS_EX_RIGHTSCROLLBAR();
extern DWORD prim_wS_EX_CONTROLPARENT();
extern DWORD prim_wS_EX_STATICEDGE();
extern DWORD prim_wS_EX_APPWINDOW();
extern DWORD prim_wS_EX_OVERLAPPEDWINDOW();
extern DWORD prim_wS_EX_PALETTEWINDOW();
extern int prim_cW_USEDEFAULT();
extern void prim_setWindowClosure(HWND hwnd,void * closure);
extern void* prim_createWindow(LPCTSTR name,char * windowName,DWORD style,int x,int y,int width,int height,HWND hwndParent,HMENU hmenu,HINSTANCE hinst,void * closure);
extern HWND access_prim_createWindow_res1(HsPtr);
extern int access_prim_createWindow_gc_failed(HsPtr);
extern void* access_prim_createWindow_gc_failstring(HsPtr);
extern void* prim_createWindowEx(DWORD estyle,LPCTSTR cls,char * wname,DWORD wstyle,int x,int y,int nWidth,int nHeight,HWND hwndParent,HMENU hmenu,HINSTANCE hinstance,void * closure);
extern HWND access_prim_createWindowEx_res1(HsPtr);
extern int access_prim_createWindowEx_gc_failed(HsPtr);
extern void* access_prim_createWindowEx_gc_failstring(HsPtr);
extern LRESULT prim_defWindowProc(HWND arg1,DWORD arg2,WPARAM arg3,LPARAM arg4);
extern void* prim_getClientRect(HWND arg1);
extern LONG access_prim_getClientRect_gc_res1(HsPtr);
extern LONG access_prim_getClientRect_gc_res2(HsPtr);
extern LONG access_prim_getClientRect_gc_res3(HsPtr);
extern LONG access_prim_getClientRect_gc_res4(HsPtr);
extern int access_prim_getClientRect_gc_failed(HsPtr);
extern void* access_prim_getClientRect_gc_failstring(HsPtr);
extern void* prim_getWindowRect(HWND arg1);
extern LONG access_prim_getWindowRect_gc_res1(HsPtr);
extern LONG access_prim_getWindowRect_gc_res2(HsPtr);
extern LONG access_prim_getWindowRect_gc_res3(HsPtr);
extern LONG access_prim_getWindowRect_gc_res4(HsPtr);
extern int access_prim_getWindowRect_gc_failed(HsPtr);
extern void* access_prim_getWindowRect_gc_failstring(HsPtr);
extern void* prim_invalidateRect(HWND arg1,LPRECT arg2,int arg3);
extern int access_prim_invalidateRect_gc_failed(HsPtr);
extern void* access_prim_invalidateRect_gc_failstring(HsPtr);
extern void* prim_screenToClient(HWND arg1,LONG gc_arg2,LONG gc_arg3);
extern LONG access_prim_screenToClient_gc_res1(HsPtr);
extern LONG access_prim_screenToClient_gc_res2(HsPtr);
extern int access_prim_screenToClient_gc_failed(HsPtr);
extern void* access_prim_screenToClient_gc_failstring(HsPtr);
extern void* prim_clientToScreen(HWND hwnd,LONG gc_arg2,LONG gc_arg3);
extern LONG access_prim_clientToScreen_gc_res1(HsPtr);
extern LONG access_prim_clientToScreen_gc_res2(HsPtr);
extern int access_prim_clientToScreen_gc_failed(HsPtr);
extern void* access_prim_clientToScreen_gc_failstring(HsPtr);
extern void* prim_setWindowText(HWND arg1,char * arg2);
extern int access_prim_setWindowText_gc_failed(HsPtr);
extern void* access_prim_setWindowText_gc_failstring(HsPtr);
extern DWORD prim_sizeofPAINTSTRUCT();
extern void* prim_beginPaint(HWND arg1,LPPAINTSTRUCT arg2);
extern HDC access_prim_beginPaint_res1(HsPtr);
extern int access_prim_beginPaint_gc_failed(HsPtr);
extern void* access_prim_beginPaint_gc_failstring(HsPtr);
extern void prim_endPaint(HWND arg1,LPPAINTSTRUCT arg2);
extern DWORD prim_sW_HIDE();
extern DWORD prim_sW_SHOWNORMAL();
extern DWORD prim_sW_SHOWMINIMIZED();
extern DWORD prim_sW_SHOWMAXIMIZED();
extern DWORD prim_sW_MAXIMIZE();
extern DWORD prim_sW_SHOWNOACTIVATE();
extern DWORD prim_sW_SHOW();
extern DWORD prim_sW_MINIMIZE();
extern DWORD prim_sW_SHOWMINNOACTIVE();
extern DWORD prim_sW_SHOWNA();
extern DWORD prim_sW_RESTORE();
extern int prim_showWindow(HWND arg1,DWORD arg2);
extern void* prim_adjustWindowRect(LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5,DWORD arg2,int arg3);
extern LONG access_prim_adjustWindowRect_gc_res1(HsPtr);
extern LONG access_prim_adjustWindowRect_gc_res2(HsPtr);
extern LONG access_prim_adjustWindowRect_gc_res3(HsPtr);
extern LONG access_prim_adjustWindowRect_gc_res4(HsPtr);
extern int access_prim_adjustWindowRect_gc_failed(HsPtr);
extern void* access_prim_adjustWindowRect_gc_failstring(HsPtr);
extern void* prim_adjustWindowRectEx(LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5,DWORD arg2,int arg3,DWORD arg4);
extern LONG access_prim_adjustWindowRectEx_gc_res1(HsPtr);
extern LONG access_prim_adjustWindowRectEx_gc_res2(HsPtr);
extern LONG access_prim_adjustWindowRectEx_gc_res3(HsPtr);
extern LONG access_prim_adjustWindowRectEx_gc_res4(HsPtr);
extern int access_prim_adjustWindowRectEx_gc_failed(HsPtr);
extern void* access_prim_adjustWindowRectEx_gc_failstring(HsPtr);
extern int prim_anyPopup();
extern void* prim_arrangeIconicWindows(HWND arg1);
extern int access_prim_arrangeIconicWindows_gc_failed(HsPtr);
extern void* access_prim_arrangeIconicWindows_gc_failstring(HsPtr);
extern void* prim_beginDeferWindowPos(int arg1);
extern HDWP access_prim_beginDeferWindowPos_res1(HsPtr);
extern int access_prim_beginDeferWindowPos_gc_failed(HsPtr);
extern void* access_prim_beginDeferWindowPos_gc_failstring(HsPtr);
extern void* prim_bringWindowToTop(HWND arg1);
extern int access_prim_bringWindowToTop_gc_failed(HsPtr);
extern void* access_prim_bringWindowToTop_gc_failstring(HsPtr);
extern void* prim_childWindowFromPoint(HWND hwnd,LONG gc_arg2,LONG gc_arg3);
extern HWND access_prim_childWindowFromPoint_res1(HsPtr);
extern int access_prim_childWindowFromPoint_gc_failed(HsPtr);
extern void* access_prim_childWindowFromPoint_gc_failstring(HsPtr);
extern void* prim_childWindowFromPointEx(HWND hwnd,LONG gc_arg2,LONG gc_arg3,DWORD arg2);
extern HWND access_prim_childWindowFromPointEx_res1(HsPtr);
extern int access_prim_childWindowFromPointEx_gc_failed(HsPtr);
extern void* access_prim_childWindowFromPointEx_gc_failstring(HsPtr);
extern void* prim_closeWindow(HWND arg1);
extern int access_prim_closeWindow_gc_failed(HsPtr);
extern void* access_prim_closeWindow_gc_failstring(HsPtr);
extern void* prim_deferWindowPos(HDWP arg1,HWND arg2,HWND arg3,int arg4,int arg5,int arg6,int arg7,UINT arg8);
extern HDWP access_prim_deferWindowPos_res1(HsPtr);
extern int access_prim_deferWindowPos_gc_failed(HsPtr);
extern void* access_prim_deferWindowPos_gc_failstring(HsPtr);
extern void* prim_destroyWindow(HWND arg1);
extern int access_prim_destroyWindow_gc_failed(HsPtr);
extern void* access_prim_destroyWindow_gc_failstring(HsPtr);
extern void* prim_endDeferWindowPos(HDWP arg1);
extern int access_prim_endDeferWindowPos_gc_failed(HsPtr);
extern void* access_prim_endDeferWindowPos_gc_failstring(HsPtr);
extern HWND prim_findWindow(char * arg1,char * arg2);
extern HWND prim_findWindowEx(HWND arg1,HWND arg2,char * arg3,char * arg4);
extern int prim_flashWindow(HWND arg1,int arg2);
extern void* prim_moveWindow(HWND arg1,int arg2,int arg3,int arg4,int arg5,int arg6);
extern int access_prim_moveWindow_gc_failed(HsPtr);
extern void* access_prim_moveWindow_gc_failstring(HsPtr);
extern HWND prim_getDesktopWindow();
extern HWND prim_getForegroundWindow();
extern void* prim_getParent(HWND arg1);
extern HWND access_prim_getParent_res1(HsPtr);
extern int access_prim_getParent_gc_failed(HsPtr);
extern void* access_prim_getParent_gc_failstring(HsPtr);
extern void* prim_getTopWindow(HWND arg1);
extern HWND access_prim_getTopWindow_res1(HsPtr);
extern int access_prim_getTopWindow_gc_failed(HsPtr);
extern void* access_prim_getTopWindow_gc_failstring(HsPtr);
extern UINT prim_sWP_NOSIZE();
extern UINT prim_sWP_NOMOVE();
extern UINT prim_sWP_NOZORDER();
extern UINT prim_sWP_NOREDRAW();
extern UINT prim_sWP_NOACTIVATE();
extern UINT prim_sWP_FRAMECHANGED();
extern UINT prim_sWP_SHOWWINDOW();
extern UINT prim_sWP_HIDEWINDOW();
extern UINT prim_sWP_NOCOPYBITS();
extern UINT prim_sWP_NOOWNERZORDER();
extern UINT prim_sWP_NOSENDCHANGING();
extern UINT prim_sWP_DRAWFRAME();
extern UINT prim_sWP_NOREPOSITION();
extern DWORD prim_dCX_WINDOW();
extern DWORD prim_dCX_CACHE();
extern DWORD prim_dCX_CLIPCHILDREN();
extern DWORD prim_dCX_CLIPSIBLINGS();
extern DWORD prim_dCX_PARENTCLIP();
extern DWORD prim_dCX_EXCLUDERGN();
extern DWORD prim_dCX_INTERSECTRGN();
extern DWORD prim_dCX_LOCKWINDOWUPDATE();
extern void* prim_getDCEx(HWND arg1,HRGN arg2,DWORD arg3);
extern HDC access_prim_getDCEx_res1(HsPtr);
extern int access_prim_getDCEx_gc_failed(HsPtr);
extern void* access_prim_getDCEx_gc_failstring(HsPtr);
extern void* prim_getDC(HWND arg1);
extern HDC access_prim_getDC_res1(HsPtr);
extern int access_prim_getDC_gc_failed(HsPtr);
extern void* access_prim_getDC_gc_failstring(HsPtr);
extern void* prim_getWindowDC(HWND arg1);
extern HDC access_prim_getWindowDC_res1(HsPtr);
extern int access_prim_getWindowDC_gc_failed(HsPtr);
extern void* access_prim_getWindowDC_gc_failstring(HsPtr);
extern void* prim_releaseDC(HWND arg1,HDC arg2);
extern int access_prim_releaseDC_gc_failed(HsPtr);
extern void* access_prim_releaseDC_gc_failstring(HsPtr);
extern void* prim_getDCOrgEx(HDC arg1);
extern LONG access_prim_getDCOrgEx_gc_res1(HsPtr);
extern LONG access_prim_getDCOrgEx_gc_res2(HsPtr);
extern int access_prim_getDCOrgEx_gc_failed(HsPtr);
extern void* access_prim_getDCOrgEx_gc_failstring(HsPtr);
extern void* prim_hideCaret(HWND arg1);
extern int access_prim_hideCaret_gc_failed(HsPtr);
extern void* access_prim_hideCaret_gc_failstring(HsPtr);
extern void* prim_showCaret(HWND arg1);
extern int access_prim_showCaret_gc_failed(HsPtr);
extern void* access_prim_showCaret_gc_failstring(HsPtr);
extern void* prim_createCaret(HWND arg1,HBITMAP arg2,INT arg3,INT arg4);
extern int access_prim_createCaret_gc_failed(HsPtr);
extern void* access_prim_createCaret_gc_failstring(HsPtr);
extern void* prim_destroyCaret();
extern int access_prim_destroyCaret_gc_failed(HsPtr);
extern void* access_prim_destroyCaret_gc_failstring(HsPtr);
extern void* prim_getCaretPos();
extern LONG access_prim_getCaretPos_gc_res1(HsPtr);
extern LONG access_prim_getCaretPos_gc_res2(HsPtr);
extern int access_prim_getCaretPos_gc_failed(HsPtr);
extern void* access_prim_getCaretPos_gc_failstring(HsPtr);
extern void* prim_setCaretPos(LONG gc_arg2,LONG gc_arg3);
extern int access_prim_setCaretPos_gc_failed(HsPtr);
extern void* access_prim_setCaretPos_gc_failstring(HsPtr);
extern void* prim_getMessage(HWND arg1);
extern MSG * access_prim_getMessage_gc_res1(HsPtr);
extern int access_prim_getMessage_gc_failed(HsPtr);
extern void* access_prim_getMessage_gc_failstring(HsPtr);
extern void* prim_getMessage2(HWND arg1);
extern MSG * access_prim_getMessage2_gc_res1(HsPtr);
extern int access_prim_getMessage2_gc_res3(HsPtr);
extern int access_prim_getMessage2_gc_failed(HsPtr);
extern void* access_prim_getMessage2_gc_failstring(HsPtr);
extern void* prim_peekMessage(HWND arg1,UINT arg2,UINT arg3,UINT arg4);
extern MSG * access_prim_peekMessage_gc_res1(HsPtr);
extern int access_prim_peekMessage_gc_failed(HsPtr);
extern void* access_prim_peekMessage_gc_failstring(HsPtr);
extern BOOL prim_translateMessage(MSG * arg1);
extern void* prim_updateWindow(HWND arg1);
extern int access_prim_updateWindow_gc_failed(HsPtr);
extern void* access_prim_updateWindow_gc_failstring(HsPtr);
extern LONG prim_dispatchMessage(MSG * arg1);
extern LRESULT prim_sendMessage(HWND arg1,DWORD arg2,WPARAM arg3,LPARAM arg4);
