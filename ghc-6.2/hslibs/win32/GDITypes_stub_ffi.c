/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "GDITypes_stub_ffi.h"
void* prim_mallocPOINTs(int arg1)
{ static struct {HsPtr ps;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  POINT* ps;int gc_failed;
	    char* gc_failstring;
  do { ps = (POINT*) malloc(arg1 * sizeof(POINT));
      if ((gc_failed = ( ps==0 ))) {gc_failstring =  MallocError("mallocPOINTs")  ;}
      else {gc_failed = 0;}
      gc_result.ps = (POINT*)(ps);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
POINT* access_prim_mallocPOINTs_ps(void *ptr){ return(((struct {HsPtr ps;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->ps);}
int access_prim_mallocPOINTs_gc_failed(void *ptr){ return(((struct {HsPtr ps;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_mallocPOINTs_gc_failstring(void *ptr){ return(((struct {HsPtr ps;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void prim_setPOINT(POINT* ps,int i,LONG gc_arg2,LONG gc_arg3)
{ (ps[i]).x = (LONG)gc_arg2; (ps[i]).y = (LONG)gc_arg3;
  do {;} while(0);
}
void* prim_getRECT(RECT* r)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;} gc_result;
  do {gc_result.gc_res1 = (LONG)((*r).left);
      gc_result.gc_res2 = (LONG)((*r).top);
      gc_result.gc_res3 = (LONG)((*r).right);
      gc_result.gc_res4 = (LONG)((*r).bottom);
      
      return(&gc_result);} while(0);
}
LONG access_prim_getRECT_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;}*) ptr)->gc_res1);}
LONG access_prim_getRECT_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;}*) ptr)->gc_res2);}
LONG access_prim_getRECT_gc_res3(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;}*) ptr)->gc_res3);}
LONG access_prim_getRECT_gc_res4(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;}*) ptr)->gc_res4);}
HWND prim_hWND_BOTTOM()
{ HWND res1;
  do {res1=HWND_BOTTOM;
      
      return((HWND)(res1));} while(0);
}
HWND prim_hWND_NOTOPMOST()
{ HWND res1;
  do {res1=HWND_NOTOPMOST;
      
      return((HWND)(res1));} while(0);
}
HWND prim_hWND_TOP()
{ HWND res1;
  do {res1=HWND_TOP;
      
      return((HWND)(res1));} while(0);
}
HWND prim_hWND_TOPMOST()
{ HWND res1;
  do {res1=HWND_TOPMOST;
      
      return((HWND)(res1));} while(0);
}
DWORD prim_rgb(BYTE arg1,BYTE arg2,BYTE arg3)
{ DWORD res1;
  do { res1 = RGB(arg1,arg2,arg3);
      
      return((DWORD)(res1));} while(0);
}
BYTE prim_getRValue(DWORD arg1)
{ BYTE res1;
  do {res1 = GetRValue(arg1);
      
      return((BYTE)(res1));} while(0);
}
BYTE prim_getGValue(DWORD arg1)
{ BYTE res1;
  do {res1 = GetGValue(arg1);
      
      return((BYTE)(res1));} while(0);
}
BYTE prim_getBValue(DWORD arg1)
{ BYTE res1;
  do {res1 = GetBValue(arg1);
      
      return((BYTE)(res1));} while(0);
}
WORD prim_aLTERNATE()
{ WORD res1;
  do {res1=ALTERNATE;
      
      return((WORD)(res1));} while(0);
}
WORD prim_wINDING()
{ WORD res1;
  do {res1=WINDING;
      
      return((WORD)(res1));} while(0);
}
WORD prim_aD_COUNTERCLOCKWISE()
{ WORD res1;
  do {res1=AD_COUNTERCLOCKWISE;
      
      return((WORD)(res1));} while(0);
}
WORD prim_aD_CLOCKWISE()
{ WORD res1;
  do {res1=AD_CLOCKWISE;
      
      return((WORD)(res1));} while(0);
}
DWORD prim_gM_COMPATIBLE()
{ DWORD res1;
  do {res1=GM_COMPATIBLE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_gM_ADVANCED()
{ DWORD res1;
  do {res1=GM_ADVANCED;
      
      return((DWORD)(res1));} while(0);
}
UINT prim_tRANSPARENT()
{ UINT res1;
  do {res1=TRANSPARENT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_oPAQUE()
{ UINT res1;
  do {res1=OPAQUE;
      
      return((UINT)(res1));} while(0);
}
WORD prim_hS_HORIZONTAL()
{ WORD res1;
  do {res1=HS_HORIZONTAL;
      
      return((WORD)(res1));} while(0);
}
WORD prim_hS_VERTICAL()
{ WORD res1;
  do {res1=HS_VERTICAL;
      
      return((WORD)(res1));} while(0);
}
WORD prim_hS_FDIAGONAL()
{ WORD res1;
  do {res1=HS_FDIAGONAL;
      
      return((WORD)(res1));} while(0);
}
WORD prim_hS_BDIAGONAL()
{ WORD res1;
  do {res1=HS_BDIAGONAL;
      
      return((WORD)(res1));} while(0);
}
WORD prim_hS_CROSS()
{ WORD res1;
  do {res1=HS_CROSS;
      
      return((WORD)(res1));} while(0);
}
WORD prim_hS_DIAGCROSS()
{ WORD res1;
  do {res1=HS_DIAGCROSS;
      
      return((WORD)(res1));} while(0);
}
UINT prim_bLACKONWHITE()
{ UINT res1;
  do {res1=BLACKONWHITE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_wHITEONBLACK()
{ UINT res1;
  do {res1=WHITEONBLACK;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cOLORONCOLOR()
{ UINT res1;
  do {res1=COLORONCOLOR;
      
      return((UINT)(res1));} while(0);
}
UINT prim_hALFTONE()
{ UINT res1;
  do {res1=HALFTONE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sTRETCH_ANDSCANS()
{ UINT res1;
  do {res1=STRETCH_ANDSCANS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sTRETCH_ORSCANS()
{ UINT res1;
  do {res1=STRETCH_ORSCANS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_sTRETCH_DELETESCANS()
{ UINT res1;
  do {res1=STRETCH_DELETESCANS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tA_NOUPDATECP()
{ UINT res1;
  do {res1=TA_NOUPDATECP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tA_UPDATECP()
{ UINT res1;
  do {res1=TA_UPDATECP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tA_LEFT()
{ UINT res1;
  do {res1=TA_LEFT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tA_RIGHT()
{ UINT res1;
  do {res1=TA_RIGHT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tA_CENTER()
{ UINT res1;
  do {res1=TA_CENTER;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tA_TOP()
{ UINT res1;
  do {res1=TA_TOP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tA_BOTTOM()
{ UINT res1;
  do {res1=TA_BOTTOM;
      
      return((UINT)(res1));} while(0);
}
UINT prim_tA_BASELINE()
{ UINT res1;
  do {res1=TA_BASELINE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_rGN_AND()
{ UINT res1;
  do {res1=RGN_AND;
      
      return((UINT)(res1));} while(0);
}
UINT prim_rGN_OR()
{ UINT res1;
  do {res1=RGN_OR;
      
      return((UINT)(res1));} while(0);
}
UINT prim_rGN_XOR()
{ UINT res1;
  do {res1=RGN_XOR;
      
      return((UINT)(res1));} while(0);
}
UINT prim_rGN_DIFF()
{ UINT res1;
  do {res1=RGN_DIFF;
      
      return((UINT)(res1));} while(0);
}
UINT prim_rGN_COPY()
{ UINT res1;
  do {res1=RGN_COPY;
      
      return((UINT)(res1));} while(0);
}
WORD prim_eRROR()
{ WORD res1;
  do {res1=ERROR;
      
      return((WORD)(res1));} while(0);
}
WORD prim_nULLREGION()
{ WORD res1;
  do {res1=NULLREGION;
      
      return((WORD)(res1));} while(0);
}
WORD prim_sIMPLEREGION()
{ WORD res1;
  do {res1=SIMPLEREGION;
      
      return((WORD)(res1));} while(0);
}
WORD prim_cOMPLEXREGION()
{ WORD res1;
  do {res1=COMPLEXREGION;
      
      return((WORD)(res1));} while(0);
}
