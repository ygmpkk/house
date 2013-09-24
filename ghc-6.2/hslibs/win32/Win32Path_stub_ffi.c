/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Path_stub_ffi.h"
void* prim_beginPath(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = BeginPath(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("BeginPath")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_beginPath_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_beginPath_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_closeFigure(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = CloseFigure(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("CloseFigure")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_closeFigure_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_closeFigure_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_endPath(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = EndPath(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("EndPath")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_endPath_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_endPath_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_fillPath(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = FillPath(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("FillPath")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_fillPath_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_fillPath_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_flattenPath(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = FlattenPath(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("FlattenPath")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_flattenPath_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_flattenPath_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_pathToRegion(HDC arg1)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HRGN res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = PathToRegion(arg1);
      if ((gc_failed = (  res1 == (HRGN) 0  ))) {gc_failstring =  ErrorWin("PathToRegion")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HRGN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_pathToRegion_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HRGN access_prim_pathToRegion_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_pathToRegion_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_pathToRegion_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_strokeAndFillPath(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = StrokeAndFillPath(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("StrokeAndFillPath")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_strokeAndFillPath_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_strokeAndFillPath_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_strokePath(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = StrokePath(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("StrokePath")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_strokePath_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_strokePath_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_widenPath(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = WidenPath(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("WidenPath")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_widenPath_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_widenPath_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
