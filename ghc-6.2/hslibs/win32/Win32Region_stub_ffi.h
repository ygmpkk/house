#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern void* prim_combineRgn(HRGN arg1,HRGN arg2,HRGN arg3,UINT arg4);
extern WORD access_prim_combineRgn_res1(HsPtr);
extern int access_prim_combineRgn_gc_failed(HsPtr);
extern void* access_prim_combineRgn_gc_failstring(HsPtr);
extern void* prim_offsetRgn(HRGN arg1,INT arg2,INT arg3);
extern WORD access_prim_offsetRgn_res1(HsPtr);
extern int access_prim_offsetRgn_gc_failed(HsPtr);
extern void* access_prim_offsetRgn_gc_failstring(HsPtr);
extern void* prim_getRgnBox(HRGN arg1,LPRECT arg2);
extern WORD access_prim_getRgnBox_res1(HsPtr);
extern int access_prim_getRgnBox_gc_failed(HsPtr);
extern void* access_prim_getRgnBox_gc_failstring(HsPtr);
extern void* prim_createEllipticRgn(INT arg1,INT arg2,INT arg3,INT arg4);
extern void* access_prim_createEllipticRgn_gc_res3(HsPtr);
extern HRGN access_prim_createEllipticRgn_gc_res1(HsPtr);
extern int access_prim_createEllipticRgn_gc_failed(HsPtr);
extern void* access_prim_createEllipticRgn_gc_failstring(HsPtr);
extern void* prim_createEllipticRgnIndirect(LPRECT arg1);
extern void* access_prim_createEllipticRgnIndirect_gc_res3(HsPtr);
extern HRGN access_prim_createEllipticRgnIndirect_gc_res1(HsPtr);
extern int access_prim_createEllipticRgnIndirect_gc_failed(HsPtr);
extern void* access_prim_createEllipticRgnIndirect_gc_failstring(HsPtr);
extern void* prim_createRectRgn(INT arg1,INT arg2,INT arg3,INT arg4);
extern void* access_prim_createRectRgn_gc_res3(HsPtr);
extern HRGN access_prim_createRectRgn_gc_res1(HsPtr);
extern int access_prim_createRectRgn_gc_failed(HsPtr);
extern void* access_prim_createRectRgn_gc_failstring(HsPtr);
extern void* prim_createRectRgnIndirect(LPRECT arg1);
extern void* access_prim_createRectRgnIndirect_gc_res3(HsPtr);
extern HRGN access_prim_createRectRgnIndirect_gc_res1(HsPtr);
extern int access_prim_createRectRgnIndirect_gc_failed(HsPtr);
extern void* access_prim_createRectRgnIndirect_gc_failstring(HsPtr);
extern void* prim_createRoundRectRgn(INT arg1,INT arg2,INT arg3,INT arg4,INT arg5,INT arg6);
extern void* access_prim_createRoundRectRgn_gc_res3(HsPtr);
extern HRGN access_prim_createRoundRectRgn_gc_res1(HsPtr);
extern int access_prim_createRoundRectRgn_gc_failed(HsPtr);
extern void* access_prim_createRoundRectRgn_gc_failstring(HsPtr);
extern void* prim_createPolygonRgn(POINT * ps,int num_ps,WORD mode);
extern void* access_prim_createPolygonRgn_gc_res3(HsPtr);
extern HRGN access_prim_createPolygonRgn_gc_res1(HsPtr);
extern int access_prim_createPolygonRgn_gc_failed(HsPtr);
extern void* access_prim_createPolygonRgn_gc_failstring(HsPtr);
extern int prim_equalRgn(HRGN arg1,HRGN arg2);
extern void* prim_fillRgn(HDC arg1,HRGN arg2,HBRUSH arg3);
extern int access_prim_fillRgn_gc_failed(HsPtr);
extern void* access_prim_fillRgn_gc_failstring(HsPtr);
extern void* prim_invertRgn(HDC arg1,HRGN arg2);
extern int access_prim_invertRgn_gc_failed(HsPtr);
extern void* access_prim_invertRgn_gc_failstring(HsPtr);
extern void* prim_paintRgn(HDC arg1,HRGN arg2);
extern int access_prim_paintRgn_gc_failed(HsPtr);
extern void* access_prim_paintRgn_gc_failstring(HsPtr);
extern void* prim_frameRgn(HDC arg1,HRGN arg2,HBRUSH arg3,int arg4,int arg5);
extern int access_prim_frameRgn_gc_failed(HsPtr);
extern void* access_prim_frameRgn_gc_failstring(HsPtr);
extern int prim_ptInRegion(HRGN arg1,int arg2,int arg3);
extern int prim_rectInRegion(HRGN arg1,LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5);
extern void* prim_deleteRegion(HRGN arg1);
extern int access_prim_deleteRegion_gc_failed(HsPtr);
extern void* access_prim_deleteRegion_gc_failstring(HsPtr);