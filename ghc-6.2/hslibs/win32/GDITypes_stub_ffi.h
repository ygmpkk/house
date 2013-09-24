#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern void* prim_mallocPOINTs(int arg1);
extern POINT* access_prim_mallocPOINTs_ps(HsPtr);
extern int access_prim_mallocPOINTs_gc_failed(HsPtr);
extern void* access_prim_mallocPOINTs_gc_failstring(HsPtr);
extern void prim_setPOINT(POINT* ps,int i,LONG gc_arg2,LONG gc_arg3);
extern void* prim_getRECT(RECT* r);
extern LONG access_prim_getRECT_gc_res1(HsPtr);
extern LONG access_prim_getRECT_gc_res2(HsPtr);
extern LONG access_prim_getRECT_gc_res3(HsPtr);
extern LONG access_prim_getRECT_gc_res4(HsPtr);
extern HWND prim_hWND_BOTTOM();
extern HWND prim_hWND_NOTOPMOST();
extern HWND prim_hWND_TOP();
extern HWND prim_hWND_TOPMOST();
extern DWORD prim_rgb(BYTE arg1,BYTE arg2,BYTE arg3);
extern BYTE prim_getRValue(DWORD arg1);
extern BYTE prim_getGValue(DWORD arg1);
extern BYTE prim_getBValue(DWORD arg1);
extern WORD prim_aLTERNATE();
extern WORD prim_wINDING();
extern WORD prim_aD_COUNTERCLOCKWISE();
extern WORD prim_aD_CLOCKWISE();
extern DWORD prim_gM_COMPATIBLE();
extern DWORD prim_gM_ADVANCED();
extern UINT prim_tRANSPARENT();
extern UINT prim_oPAQUE();
extern WORD prim_hS_HORIZONTAL();
extern WORD prim_hS_VERTICAL();
extern WORD prim_hS_FDIAGONAL();
extern WORD prim_hS_BDIAGONAL();
extern WORD prim_hS_CROSS();
extern WORD prim_hS_DIAGCROSS();
extern UINT prim_bLACKONWHITE();
extern UINT prim_wHITEONBLACK();
extern UINT prim_cOLORONCOLOR();
extern UINT prim_hALFTONE();
extern UINT prim_sTRETCH_ANDSCANS();
extern UINT prim_sTRETCH_ORSCANS();
extern UINT prim_sTRETCH_DELETESCANS();
extern UINT prim_tA_NOUPDATECP();
extern UINT prim_tA_UPDATECP();
extern UINT prim_tA_LEFT();
extern UINT prim_tA_RIGHT();
extern UINT prim_tA_CENTER();
extern UINT prim_tA_TOP();
extern UINT prim_tA_BOTTOM();
extern UINT prim_tA_BASELINE();
extern UINT prim_rGN_AND();
extern UINT prim_rGN_OR();
extern UINT prim_rGN_XOR();
extern UINT prim_rGN_DIFF();
extern UINT prim_rGN_COPY();
extern WORD prim_eRROR();
extern WORD prim_nULLREGION();
extern WORD prim_sIMPLEREGION();
extern WORD prim_cOMPLEXREGION();