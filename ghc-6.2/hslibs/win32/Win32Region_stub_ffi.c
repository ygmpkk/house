/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Region_stub_ffi.h"
void* prim_combineRgn(HRGN arg1,HRGN arg2,HRGN arg3,UINT arg4)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = CombineRgn(arg1, arg2, arg3, arg4);
      if ((gc_failed = (  BadRgnTest(res1)  ))) {gc_failstring =  ErrorString("CombineRgn")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_combineRgn_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_combineRgn_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_combineRgn_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_offsetRgn(HRGN arg1,INT arg2,INT arg3)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = OffsetRgn(arg1, arg2, arg3);
      if ((gc_failed = (  BadRgnTest(res1)  ))) {gc_failstring =  ErrorString("OffsetRgn")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_offsetRgn_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_offsetRgn_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_offsetRgn_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getRgnBox(HRGN arg1,LPRECT arg2)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetRgnBox(arg1, arg2);
      if ((gc_failed = (  BadRgnTest(res1)  ))) {gc_failstring =  ErrorString("GetRgnBox")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_getRgnBox_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getRgnBox_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getRgnBox_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createEllipticRgn(INT arg1,INT arg2,INT arg3,INT arg4)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HRGN res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = CreateEllipticRgn(arg1, arg2, arg3, arg4);
      if ((gc_failed = (  res1 == (HRGN) 0  ))) {gc_failstring =  ErrorString("CreateEllipticRgn")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HRGN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_createEllipticRgn_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HRGN access_prim_createEllipticRgn_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_createEllipticRgn_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createEllipticRgn_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createEllipticRgnIndirect(LPRECT arg1)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HRGN res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = CreateEllipticRgnIndirect(arg1);
      if ((gc_failed = (  res1 == (HRGN) 0  ))) {gc_failstring =  ErrorString("CreateEllipticRgnIndirect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HRGN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_createEllipticRgnIndirect_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HRGN access_prim_createEllipticRgnIndirect_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_createEllipticRgnIndirect_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createEllipticRgnIndirect_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createRectRgn(INT arg1,INT arg2,INT arg3,INT arg4)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HRGN res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = CreateRectRgn(arg1, arg2, arg3, arg4);
      if ((gc_failed = (  res1 == (HRGN) 0  ))) {gc_failstring =  ErrorString("CreateRectRgn")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HRGN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_createRectRgn_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HRGN access_prim_createRectRgn_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_createRectRgn_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createRectRgn_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createRectRgnIndirect(LPRECT arg1)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HRGN res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = CreateRectRgnIndirect(arg1);
      if ((gc_failed = (  res1 == (HRGN) 0  ))) {gc_failstring =  ErrorString("CreateRectRgnIndirect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HRGN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_createRectRgnIndirect_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HRGN access_prim_createRectRgnIndirect_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_createRectRgnIndirect_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createRectRgnIndirect_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createRoundRectRgn(INT arg1,INT arg2,INT arg3,INT arg4,INT arg5,INT arg6)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HRGN res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = CreateRoundRectRgn(arg1, arg2, arg3, arg4, arg5, arg6);
      if ((gc_failed = (  res1 == (HRGN) 0  ))) {gc_failstring =  ErrorString("CreateRoundRgn")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HRGN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_createRoundRectRgn_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HRGN access_prim_createRoundRectRgn_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_createRoundRectRgn_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createRoundRectRgn_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createPolygonRgn(POINT * ps,int num_ps,WORD mode)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HRGN h;int gc_failed;
	 char* gc_failstring;
  do { h = CreatePolygonRgn(ps,num_ps,mode);
      if ((gc_failed = (  h == (HRGN) 0  ))) {gc_failstring =  ErrorString("CreatePolygonRgn")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HRGN)(h);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
       free(ps);
      return(&gc_result);} while(0);
}
void* access_prim_createPolygonRgn_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HRGN access_prim_createPolygonRgn_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_createPolygonRgn_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createPolygonRgn_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_equalRgn(HRGN arg1,HRGN arg2)
{ int res1;
  do {res1 = EqualRgn(arg1, arg2);
      
      return((int)(res1));} while(0);
}
void* prim_fillRgn(HDC arg1,HRGN arg2,HBRUSH arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = FillRgn(arg1, arg2, arg3);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("FillRgn")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_fillRgn_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_fillRgn_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_invertRgn(HDC arg1,HRGN arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = InvertRgn(arg1, arg2);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("InvertRgn")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_invertRgn_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_invertRgn_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_paintRgn(HDC arg1,HRGN arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = PaintRgn(arg1, arg2);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("PaintRgn")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_paintRgn_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_paintRgn_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_frameRgn(HDC arg1,HRGN arg2,HBRUSH arg3,int arg4,int arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = FrameRgn(arg1, arg2, arg3, arg4, arg5);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("FrameRgn")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_frameRgn_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_frameRgn_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_ptInRegion(HRGN arg1,int arg2,int arg3)
{ int res1;
  do {res1 = PtInRegion(arg1, arg2, arg3);
      
      return((int)(res1));} while(0);
}
int prim_rectInRegion(HRGN arg1,LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5)
{ int res1;RECT arg2;
  (arg2).left = (LONG)gc_arg2; (arg2).top = (LONG)gc_arg3; (arg2).right = (LONG)gc_arg4; (arg2).bottom = (LONG)gc_arg5;
  do { res1 = RectInRegion(arg1,&arg2);
      
      return((int)(res1));} while(0);
}
void* prim_deleteRegion(HRGN arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DeleteObject(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("DeleteRegion")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_deleteRegion_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_deleteRegion_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
