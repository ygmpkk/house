#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern WORD prim_wHITE_PEN();
extern WORD prim_bLACK_PEN();
extern WORD prim_nULL_PEN();
extern void* prim_getStockPen(WORD arg1);
extern HPEN access_prim_getStockPen_res1(HsPtr);
extern int access_prim_getStockPen_gc_failed(HsPtr);
extern void* access_prim_getStockPen_gc_failstring(HsPtr);
extern void* prim_deletePen(HPEN arg1);
extern int access_prim_deletePen_gc_failed(HsPtr);
extern void* access_prim_deletePen_gc_failstring(HsPtr);
extern int prim_pS_SOLID();
extern int prim_pS_DASH();
extern int prim_pS_DOT();
extern int prim_pS_DASHDOT();
extern int prim_pS_DASHDOTDOT();
extern int prim_pS_NULL();
extern int prim_pS_INSIDEFRAME();
extern int prim_pS_USERSTYLE();
extern int prim_pS_ALTERNATE();
extern int prim_pS_STYLE_MASK();
extern int prim_pS_ENDCAP_ROUND();
extern int prim_pS_ENDCAP_SQUARE();
extern int prim_pS_ENDCAP_FLAT();
extern int prim_pS_ENDCAP_MASK();
extern int prim_pS_JOIN_ROUND();
extern int prim_pS_JOIN_BEVEL();
extern int prim_pS_JOIN_MITER();
extern int prim_pS_COSMETIC();
extern int prim_pS_GEOMETRIC();
extern int prim_pS_TYPE_MASK();
extern void* prim_createPen(int arg1,INT arg2,DWORD arg3);
extern HPEN access_prim_createPen_res1(HsPtr);
extern int access_prim_createPen_gc_failed(HsPtr);
extern void* access_prim_createPen_gc_failstring(HsPtr);
