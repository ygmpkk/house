#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern void* prim_checkMenuItem(HMENU arg1,UINT arg2,UINT arg3);
extern int access_prim_checkMenuItem_gc_res2(HsPtr);
extern int access_prim_checkMenuItem_gc_failed(HsPtr);
extern void* access_prim_checkMenuItem_gc_failstring(HsPtr);
extern void* prim_checkMenuRadioItem(HMENU arg1,UINT arg2,UINT arg3,UINT arg4,UINT arg5);
extern int access_prim_checkMenuRadioItem_gc_failed(HsPtr);
extern void* access_prim_checkMenuRadioItem_gc_failstring(HsPtr);
extern void* prim_createMenu();
extern HMENU access_prim_createMenu_res1(HsPtr);
extern int access_prim_createMenu_gc_failed(HsPtr);
extern void* access_prim_createMenu_gc_failstring(HsPtr);
extern void* prim_createPopupMenu();
extern HMENU access_prim_createPopupMenu_res1(HsPtr);
extern int access_prim_createPopupMenu_gc_failed(HsPtr);
extern void* access_prim_createPopupMenu_gc_failstring(HsPtr);
extern void* prim_drawMenuBar(HWND arg1);
extern int access_prim_drawMenuBar_gc_failed(HsPtr);
extern void* access_prim_drawMenuBar_gc_failstring(HsPtr);
extern void* prim_enableMenuItem(HMENU arg1,UINT arg2,UINT arg3);
extern UINT access_prim_enableMenuItem_res1(HsPtr);
extern int access_prim_enableMenuItem_gc_failed(HsPtr);
extern void* access_prim_enableMenuItem_gc_failstring(HsPtr);
extern UINT prim_gMDI_USEDISABLED();
extern UINT prim_gMDI_GOINTOPOPUPS();
extern UINT prim_mF_BYCOMMAND();
extern UINT prim_mF_BYPOSITION();
extern UINT prim_mF_INSERT();
extern UINT prim_mF_CHANGE();
extern UINT prim_mF_APPEND();
extern UINT prim_mF_DELETE();
extern UINT prim_mF_REMOVE();
extern UINT prim_mF_USECHECKBITMAPS();
extern UINT prim_mF_POPUP();
extern UINT prim_mF_SYSMENU();
extern UINT prim_mF_HELP();
extern UINT prim_mF_MOUSESELECT();
extern UINT prim_mF_END();
extern UINT prim_mFT_STRING();
extern UINT prim_mFT_BITMAP();
extern UINT prim_mFT_MENUBARBREAK();
extern UINT prim_mFT_MENUBREAK();
extern UINT prim_mFT_OWNERDRAW();
extern UINT prim_mFT_RADIOCHECK();
extern UINT prim_mFT_SEPARATOR();
extern UINT prim_mFT_RIGHTORDER();
extern UINT prim_mFT_RIGHTJUSTIFY();
extern UINT prim_mFS_GRAYED();
extern UINT prim_mFS_DISABLED();
extern UINT prim_mFS_CHECKED();
extern UINT prim_mFS_HILITE();
extern UINT prim_mFS_ENABLED();
extern UINT prim_mFS_UNCHECKED();
extern UINT prim_mFS_UNHILITE();
extern UINT prim_mFS_DEFAULT();
extern UINT prim_tPM_LEFTBUTTON();
extern UINT prim_tPM_RIGHTBUTTON();
extern UINT prim_tPM_LEFTALIGN();
extern UINT prim_tPM_CENTERALIGN();
extern UINT prim_tPM_RIGHTALIGN();
extern UINT prim_tPM_TOPALIGN();
extern UINT prim_tPM_VCENTERALIGN();
extern UINT prim_tPM_BOTTOMALIGN();
extern UINT prim_tPM_HORIZONTAL();
extern UINT prim_tPM_VERTICAL();
extern UINT prim_tPM_NONOTIFY();
extern UINT prim_tPM_RETURNCMD();
extern UINT prim_sC_SIZE();
extern UINT prim_sC_MOVE();
extern UINT prim_sC_MINIMIZE();
extern UINT prim_sC_MAXIMIZE();
extern UINT prim_sC_NEXTWINDOW();
extern UINT prim_sC_PREVWINDOW();
extern UINT prim_sC_CLOSE();
extern UINT prim_sC_VSCROLL();
extern UINT prim_sC_HSCROLL();
extern UINT prim_sC_MOUSEMENU();
extern UINT prim_sC_KEYMENU();
extern UINT prim_sC_ARRANGE();
extern UINT prim_sC_RESTORE();
extern UINT prim_sC_TASKLIST();
extern UINT prim_sC_SCREENSAVE();
extern UINT prim_sC_HOTKEY();
extern UINT prim_sC_DEFAULT();
extern UINT prim_sC_MONITORPOWER();
extern UINT prim_sC_CONTEXTHELP();
extern UINT prim_sC_SEPARATOR();
extern int prim_isMenu(HMENU arg1);
extern HMENU prim_getSystemMenu(HWND arg1,int arg2);
extern HMENU prim_getMenu(HWND arg1);
extern void* prim_getMenuDefaultItem(HMENU arg1,int arg2,UINT arg3);
extern UINT access_prim_getMenuDefaultItem_res1(HsPtr);
extern int access_prim_getMenuDefaultItem_gc_failed(HsPtr);
extern void* access_prim_getMenuDefaultItem_gc_failstring(HsPtr);
extern void* prim_getMenuState(HMENU arg1,UINT arg2,UINT arg3);
extern UINT access_prim_getMenuState_res(HsPtr);
extern int access_prim_getMenuState_gc_failed(HsPtr);
extern void* access_prim_getMenuState_gc_failstring(HsPtr);
extern HMENU prim_getSubMenu(HMENU arg1,UINT arg2);
extern void* prim_setMenu(HWND arg1,HMENU arg2);
extern int access_prim_setMenu_gc_failed(HsPtr);
extern void* access_prim_setMenu_gc_failstring(HsPtr);
extern void* prim_getMenuItemCount(HMENU arg1);
extern int access_prim_getMenuItemCount_res1(HsPtr);
extern int access_prim_getMenuItemCount_gc_failed(HsPtr);
extern void* access_prim_getMenuItemCount_gc_failstring(HsPtr);
extern void* prim_getMenuItemID(HMENU arg1,UINT arg2);
extern UINT access_prim_getMenuItemID_res1(HsPtr);
extern int access_prim_getMenuItemID_gc_failed(HsPtr);
extern void* access_prim_getMenuItemID_gc_failstring(HsPtr);
extern void* prim_unravelItemInfo(void * arg1);
extern UINT access_prim_unravelItemInfo_gc_res1(HsPtr);
extern UINT access_prim_unravelItemInfo_gc_res2(HsPtr);
extern UINT access_prim_unravelItemInfo_gc_res3(HsPtr);
extern HMENU access_prim_unravelItemInfo_gc_res4(HsPtr);
extern HBITMAP access_prim_unravelItemInfo_gc_res5(HsPtr);
extern HBITMAP access_prim_unravelItemInfo_gc_res6(HsPtr);
extern DWORD access_prim_unravelItemInfo_gc_res7(HsPtr);
extern char * access_prim_unravelItemInfo_gc_res9(HsPtr);
extern int access_prim_unravelItemInfo_gc_res10(HsPtr);
extern void* prim_mallocMenuItemInfo();
extern MENUITEMINFO* access_prim_mallocMenuItemInfo_mp(HsPtr);
extern int access_prim_mallocMenuItemInfo_gc_failed(HsPtr);
extern void* access_prim_mallocMenuItemInfo_gc_failstring(HsPtr);
extern void prim_assignMenuItemInfo(void * arg1,UINT arg2,UINT arg3,UINT arg4,HMENU arg5,HBITMAP arg6,HBITMAP arg7,DWORD arg8,void * arg9,int arg10);
extern UINT prim_mIIM_CHECKMARKS();
extern UINT prim_mIIM_DATA();
extern UINT prim_mIIM_ID();
extern UINT prim_mIIM_STATE();
extern UINT prim_mIIM_SUBMENU();
extern UINT prim_mIIM_TYPE();
extern void* prim_getMenuItemInfo(HMENU arg1,UINT arg2,int arg3,UINT arg4);
extern void * access_prim_getMenuItemInfo_gc_res2(HsPtr);
extern int access_prim_getMenuItemInfo_gc_failed(HsPtr);
extern void* access_prim_getMenuItemInfo_gc_failstring(HsPtr);
extern void* prim_getMenuItemRect(HWND arg1,HMENU arg2,UINT arg3);
extern LONG access_prim_getMenuItemRect_gc_res1(HsPtr);
extern LONG access_prim_getMenuItemRect_gc_res2(HsPtr);
extern LONG access_prim_getMenuItemRect_gc_res3(HsPtr);
extern LONG access_prim_getMenuItemRect_gc_res4(HsPtr);
extern int access_prim_getMenuItemRect_gc_failed(HsPtr);
extern void* access_prim_getMenuItemRect_gc_failstring(HsPtr);
extern int prim_hiliteMenuItem(HWND arg1,HMENU arg2,UINT arg3,UINT arg4);
extern void* prim_insertMenuItem(HMENU arg1,UINT arg2,int arg3,void * arg4);
extern int access_prim_insertMenuItem_gc_failed(HsPtr);
extern void* access_prim_insertMenuItem_gc_failstring(HsPtr);
extern void* prim_loadMenu(HINSTANCE arg1,LPCSTR arg2);
extern HMENU access_prim_loadMenu_res1(HsPtr);
extern int access_prim_loadMenu_gc_failed(HsPtr);
extern void* access_prim_loadMenu_gc_failstring(HsPtr);
extern UINT prim_menuItemFromPoint(HWND arg1,HMENU arg2,LONG gc_arg2,LONG gc_arg3);
extern void* prim_setMenuDefaultItem(HMENU arg1,UINT arg2,int arg3);
extern int access_prim_setMenuDefaultItem_gc_failed(HsPtr);
extern void* access_prim_setMenuDefaultItem_gc_failstring(HsPtr);
extern void* prim_setMenuItemBitmaps(HMENU arg1,UINT arg2,UINT arg3,HBITMAP arg4,HBITMAP arg5);
extern int access_prim_setMenuItemBitmaps_gc_failed(HsPtr);
extern void* access_prim_setMenuItemBitmaps_gc_failstring(HsPtr);
extern void* prim_destroyMenu(HMENU arg1);
extern int access_prim_destroyMenu_gc_failed(HsPtr);
extern void* access_prim_destroyMenu_gc_failstring(HsPtr);
extern void* prim_deleteMenu(HMENU arg1,UINT arg2,UINT arg3);
extern int access_prim_deleteMenu_gc_failed(HsPtr);
extern void* access_prim_deleteMenu_gc_failstring(HsPtr);
extern void* prim_setMenuItemInfo(HMENU arg1,UINT arg2,int arg3,UINT arg4,void * arg5);
extern int access_prim_setMenuItemInfo_gc_failed(HsPtr);
extern void* access_prim_setMenuItemInfo_gc_failstring(HsPtr);
extern void* prim_trackPopupMenu(HMENU arg1,UINT arg2,int arg3,int arg4,HWND arg5,LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5);
extern int access_prim_trackPopupMenu_gc_failed(HsPtr);
extern void* access_prim_trackPopupMenu_gc_failstring(HsPtr);
extern void* prim_trackPopupMenuEx(HMENU arg1,UINT arg2,int arg3,int arg4,HWND arg5,LPRECT arg6);
extern int access_prim_trackPopupMenuEx_gc_failed(HsPtr);
extern void* access_prim_trackPopupMenuEx_gc_failstring(HsPtr);
extern void* prim_appendMenu(HMENU arg1,UINT arg2,UINT arg3,char * arg4);
extern int access_prim_appendMenu_gc_failed(HsPtr);
extern void* access_prim_appendMenu_gc_failstring(HsPtr);
extern void* prim_insertMenu(HMENU arg1,UINT arg2,UINT arg3,UINT arg4,char * arg5);
extern int access_prim_insertMenu_gc_failed(HsPtr);
extern void* access_prim_insertMenu_gc_failstring(HsPtr);
extern void* prim_modifyMenu(HMENU arg1,UINT arg2,UINT arg3,UINT arg4,char * arg5);
extern int access_prim_modifyMenu_gc_failed(HsPtr);
extern void* access_prim_modifyMenu_gc_failstring(HsPtr);
extern void* prim_removeMenu(HMENU arg1,UINT arg2,UINT arg3);
extern int access_prim_removeMenu_gc_failed(HsPtr);
extern void* access_prim_removeMenu_gc_failstring(HsPtr);
