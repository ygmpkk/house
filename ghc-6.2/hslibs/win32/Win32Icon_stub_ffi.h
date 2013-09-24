#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern void* prim_copyIcon(HICON arg1);
extern HICON access_prim_copyIcon_res1(HsPtr);
extern int access_prim_copyIcon_gc_failed(HsPtr);
extern void* access_prim_copyIcon_gc_failstring(HsPtr);
extern void* prim_drawIcon(HDC arg1,int arg2,int arg3,HICON arg4);
extern int access_prim_drawIcon_gc_failed(HsPtr);
extern void* access_prim_drawIcon_gc_failstring(HsPtr);
extern void* prim_destroyIcon(HICON arg1);
extern int access_prim_destroyIcon_gc_failed(HsPtr);
extern void* access_prim_destroyIcon_gc_failstring(HsPtr);
