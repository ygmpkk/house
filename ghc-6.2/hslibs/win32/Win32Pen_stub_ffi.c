/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Pen_stub_ffi.h"
WORD prim_wHITE_PEN()
{ WORD res1;
  do {res1=WHITE_PEN;
      
      return((WORD)(res1));} while(0);
}
WORD prim_bLACK_PEN()
{ WORD res1;
  do {res1=BLACK_PEN;
      
      return((WORD)(res1));} while(0);
}
WORD prim_nULL_PEN()
{ WORD res1;
  do {res1=NULL_PEN;
      
      return((WORD)(res1));} while(0);
}
void* prim_getStockPen(WORD arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HPEN res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = GetStockObject(arg1);
      if ((gc_failed = (  res1 == (HPEN) NULL  ))) {gc_failstring =  ErrorString("GetStockPen")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HPEN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HPEN access_prim_getStockPen_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getStockPen_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getStockPen_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_deletePen(HPEN arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DeleteObject(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("DeletePen")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_deletePen_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_deletePen_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_pS_SOLID()
{ int res1;
  do {res1=PS_SOLID;
      
      return((int)(res1));} while(0);
}
int prim_pS_DASH()
{ int res1;
  do {res1=PS_DASH;
      
      return((int)(res1));} while(0);
}
int prim_pS_DOT()
{ int res1;
  do {res1=PS_DOT;
      
      return((int)(res1));} while(0);
}
int prim_pS_DASHDOT()
{ int res1;
  do {res1=PS_DASHDOT;
      
      return((int)(res1));} while(0);
}
int prim_pS_DASHDOTDOT()
{ int res1;
  do {res1=PS_DASHDOTDOT;
      
      return((int)(res1));} while(0);
}
int prim_pS_NULL()
{ int res1;
  do {res1=PS_NULL;
      
      return((int)(res1));} while(0);
}
int prim_pS_INSIDEFRAME()
{ int res1;
  do {res1=PS_INSIDEFRAME;
      
      return((int)(res1));} while(0);
}
int prim_pS_USERSTYLE()
{ int res1;
  do {res1=PS_USERSTYLE;
      
      return((int)(res1));} while(0);
}
int prim_pS_ALTERNATE()
{ int res1;
  do {res1=PS_ALTERNATE;
      
      return((int)(res1));} while(0);
}
int prim_pS_STYLE_MASK()
{ int res1;
  do {res1=PS_STYLE_MASK;
      
      return((int)(res1));} while(0);
}
int prim_pS_ENDCAP_ROUND()
{ int res1;
  do {res1=PS_ENDCAP_ROUND;
      
      return((int)(res1));} while(0);
}
int prim_pS_ENDCAP_SQUARE()
{ int res1;
  do {res1=PS_ENDCAP_SQUARE;
      
      return((int)(res1));} while(0);
}
int prim_pS_ENDCAP_FLAT()
{ int res1;
  do {res1=PS_ENDCAP_FLAT;
      
      return((int)(res1));} while(0);
}
int prim_pS_ENDCAP_MASK()
{ int res1;
  do {res1=PS_ENDCAP_MASK;
      
      return((int)(res1));} while(0);
}
int prim_pS_JOIN_ROUND()
{ int res1;
  do {res1=PS_JOIN_ROUND;
      
      return((int)(res1));} while(0);
}
int prim_pS_JOIN_BEVEL()
{ int res1;
  do {res1=PS_JOIN_BEVEL;
      
      return((int)(res1));} while(0);
}
int prim_pS_JOIN_MITER()
{ int res1;
  do {res1=PS_JOIN_MITER;
      
      return((int)(res1));} while(0);
}
int prim_pS_COSMETIC()
{ int res1;
  do {res1=PS_COSMETIC;
      
      return((int)(res1));} while(0);
}
int prim_pS_GEOMETRIC()
{ int res1;
  do {res1=PS_GEOMETRIC;
      
      return((int)(res1));} while(0);
}
int prim_pS_TYPE_MASK()
{ int res1;
  do {res1=PS_TYPE_MASK;
      
      return((int)(res1));} while(0);
}
void* prim_createPen(int arg1,INT arg2,DWORD arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HPEN res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = CreatePen(arg1, arg2, arg3);
      if ((gc_failed = ( res1 == (HPEN) NULL ))) {gc_failstring =  ErrorString("CreatePen")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HPEN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HPEN access_prim_createPen_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createPen_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createPen_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
