#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern void* prim_beginPath(HDC arg1);
extern int access_prim_beginPath_gc_failed(HsPtr);
extern void* access_prim_beginPath_gc_failstring(HsPtr);
extern void* prim_closeFigure(HDC arg1);
extern int access_prim_closeFigure_gc_failed(HsPtr);
extern void* access_prim_closeFigure_gc_failstring(HsPtr);
extern void* prim_endPath(HDC arg1);
extern int access_prim_endPath_gc_failed(HsPtr);
extern void* access_prim_endPath_gc_failstring(HsPtr);
extern void* prim_fillPath(HDC arg1);
extern int access_prim_fillPath_gc_failed(HsPtr);
extern void* access_prim_fillPath_gc_failstring(HsPtr);
extern void* prim_flattenPath(HDC arg1);
extern int access_prim_flattenPath_gc_failed(HsPtr);
extern void* access_prim_flattenPath_gc_failstring(HsPtr);
extern void* prim_pathToRegion(HDC arg1);
extern void* access_prim_pathToRegion_gc_res3(HsPtr);
extern HRGN access_prim_pathToRegion_gc_res1(HsPtr);
extern int access_prim_pathToRegion_gc_failed(HsPtr);
extern void* access_prim_pathToRegion_gc_failstring(HsPtr);
extern void* prim_strokeAndFillPath(HDC arg1);
extern int access_prim_strokeAndFillPath_gc_failed(HsPtr);
extern void* access_prim_strokeAndFillPath_gc_failstring(HsPtr);
extern void* prim_strokePath(HDC arg1);
extern int access_prim_strokePath_gc_failed(HsPtr);
extern void* access_prim_strokePath_gc_failstring(HsPtr);
extern void* prim_widenPath(HDC arg1);
extern int access_prim_widenPath_gc_failed(HsPtr);
extern void* access_prim_widenPath_gc_failstring(HsPtr);
