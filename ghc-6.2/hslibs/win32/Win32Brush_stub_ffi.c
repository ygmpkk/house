/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Brush_stub_ffi.h"
void* prim_createSolidBrush(DWORD arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBRUSH res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = CreateSolidBrush(arg1);
      if ((gc_failed = (  res1 == (HBRUSH) NULL  ))) {gc_failstring =  ErrorString("CreateSolidBrush")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBRUSH)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBRUSH access_prim_createSolidBrush_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createSolidBrush_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createSolidBrush_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createHatchBrush(WORD arg1,DWORD arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBRUSH res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = CreateHatchBrush(arg1, arg2);
      if ((gc_failed = (  res1 == (HBRUSH) NULL  ))) {gc_failstring =  ErrorString("CreateHatchBrush")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBRUSH)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBRUSH access_prim_createHatchBrush_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createHatchBrush_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createHatchBrush_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createPatternBrush(HBITMAP arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBRUSH res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = CreatePatternBrush(arg1);
      if ((gc_failed = (  res1 == (HBRUSH) NULL  ))) {gc_failstring =  ErrorString("CreatePatternBrush")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBRUSH)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBRUSH access_prim_createPatternBrush_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createPatternBrush_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createPatternBrush_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_deleteBrush(HBRUSH arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DeleteObject(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("DeleteBrush")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_deleteBrush_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_deleteBrush_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
WORD prim_wHITE_BRUSH()
{ WORD res1;
  do {res1=WHITE_BRUSH;
      
      return((WORD)(res1));} while(0);
}
WORD prim_lTGRAY_BRUSH()
{ WORD res1;
  do {res1=LTGRAY_BRUSH;
      
      return((WORD)(res1));} while(0);
}
WORD prim_gRAY_BRUSH()
{ WORD res1;
  do {res1=GRAY_BRUSH;
      
      return((WORD)(res1));} while(0);
}
WORD prim_dKGRAY_BRUSH()
{ WORD res1;
  do {res1=DKGRAY_BRUSH;
      
      return((WORD)(res1));} while(0);
}
WORD prim_bLACK_BRUSH()
{ WORD res1;
  do {res1=BLACK_BRUSH;
      
      return((WORD)(res1));} while(0);
}
WORD prim_nULL_BRUSH()
{ WORD res1;
  do {res1=NULL_BRUSH;
      
      return((WORD)(res1));} while(0);
}
WORD prim_hOLLOW_BRUSH()
{ WORD res1;
  do {res1=HOLLOW_BRUSH;
      
      return((WORD)(res1));} while(0);
}
void* prim_getStockBrush(WORD arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBRUSH res1;int gc_failed;
	      char* gc_failstring;
  do { res1 = GetStockObject(arg1);
      if ((gc_failed = (  res1 == (HBRUSH) NULL  ))) {gc_failstring =  ErrorString("GetStockBrush")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBRUSH)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBRUSH access_prim_getStockBrush_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getStockBrush_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getStockBrush_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
