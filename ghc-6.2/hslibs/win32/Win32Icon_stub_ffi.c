/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Icon_stub_ffi.h"
void* prim_copyIcon(HICON arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HICON res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = CopyIcon(arg1);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("CopyIcon")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HICON)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HICON access_prim_copyIcon_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_copyIcon_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_copyIcon_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_drawIcon(HDC arg1,int arg2,int arg3,HICON arg4)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DrawIcon(arg1, arg2, arg3, arg4);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("DrawIcon")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_drawIcon_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_drawIcon_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_destroyIcon(HICON arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DestroyIcon(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("DestroyIcon")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_destroyIcon_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_destroyIcon_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
