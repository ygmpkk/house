/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32DLL_stub_ffi.h"
void* prim_disableThreadLibraryCalls(HMODULE arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1 = DisableThreadLibraryCalls(arg1);
      if ((gc_failed = (  res1 == FALSE  ))) {gc_failstring =  ErrorWin("DisableThreadLibraryCalls") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_disableThreadLibraryCalls_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_disableThreadLibraryCalls_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_freeLibrary(HMODULE arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1 = FreeLibrary(arg1);
      if ((gc_failed = (  res1 == FALSE  ))) {gc_failstring =  ErrorWin("FreeLibrary") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_freeLibrary_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_freeLibrary_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getModuleFileName(HMODULE arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  char * res1;int gc_failed;
	      char* gc_failstring;
  do { char* res1; DWORD dw = 1;
     if ((res1=malloc(sizeof(char)*512)) == NULL) {
        res1=NULL;
     } else {
        dw = GetModuleFileName(arg1,res1,512);
     };
      if ((gc_failed = ( res1 == NULL || dw == 0 ))) {gc_failstring = ErrorWin("GetModuleFileName") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (char *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
char * access_prim_getModuleFileName_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getModuleFileName_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getModuleFileName_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getModuleHandle(char * arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HMODULE res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = GetModuleHandle(arg1);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("GetModuleHandle") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HMODULE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HMODULE access_prim_getModuleHandle_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getModuleHandle_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getModuleHandle_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getProcAddress(HMODULE arg1,char * arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  void * res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = GetProcAddress(arg1, arg2);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("GetProcAddress") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (void *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void * access_prim_getProcAddress_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getProcAddress_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getProcAddress_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_loadLibrary(char * arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HINSTANCE res1;int gc_failed;
		 char* gc_failstring;
  do {res1 = LoadLibrary(arg1);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("LoadLibrary") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HINSTANCE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HINSTANCE access_prim_loadLibrary_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_loadLibrary_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_loadLibrary_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_lOAD_LIBRARY_AS_DATAFILE()
{ DWORD res1;
  do {res1=LOAD_LIBRARY_AS_DATAFILE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_lOAD_WITH_ALTERED_SEARCH_PATH()
{ DWORD res1;
  do {res1=LOAD_WITH_ALTERED_SEARCH_PATH;
      
      return((DWORD)(res1));} while(0);
}
void* prim_loadLibraryEx(char * arg1,HANDLE arg2,DWORD arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HINSTANCE res1;int gc_failed;
		 char* gc_failstring;
  do {res1 = LoadLibraryEx(arg1, arg2, arg3);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("LoadLibraryEx") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HINSTANCE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HINSTANCE access_prim_loadLibraryEx_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_loadLibraryEx_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_loadLibraryEx_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
