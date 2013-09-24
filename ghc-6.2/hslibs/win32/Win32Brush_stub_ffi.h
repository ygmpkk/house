#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern void* prim_createSolidBrush(DWORD arg1);
extern HBRUSH access_prim_createSolidBrush_res1(HsPtr);
extern int access_prim_createSolidBrush_gc_failed(HsPtr);
extern void* access_prim_createSolidBrush_gc_failstring(HsPtr);
extern void* prim_createHatchBrush(WORD arg1,DWORD arg2);
extern HBRUSH access_prim_createHatchBrush_res1(HsPtr);
extern int access_prim_createHatchBrush_gc_failed(HsPtr);
extern void* access_prim_createHatchBrush_gc_failstring(HsPtr);
extern void* prim_createPatternBrush(HBITMAP arg1);
extern HBRUSH access_prim_createPatternBrush_res1(HsPtr);
extern int access_prim_createPatternBrush_gc_failed(HsPtr);
extern void* access_prim_createPatternBrush_gc_failstring(HsPtr);
extern void* prim_deleteBrush(HBRUSH arg1);
extern int access_prim_deleteBrush_gc_failed(HsPtr);
extern void* access_prim_deleteBrush_gc_failstring(HsPtr);
extern WORD prim_wHITE_BRUSH();
extern WORD prim_lTGRAY_BRUSH();
extern WORD prim_gRAY_BRUSH();
extern WORD prim_dKGRAY_BRUSH();
extern WORD prim_bLACK_BRUSH();
extern WORD prim_nULL_BRUSH();
extern WORD prim_hOLLOW_BRUSH();
extern void* prim_getStockBrush(WORD arg1);
extern HBRUSH access_prim_getStockBrush_res1(HsPtr);
extern int access_prim_getStockBrush_gc_failed(HsPtr);
extern void* access_prim_getStockBrush_gc_failstring(HsPtr);
