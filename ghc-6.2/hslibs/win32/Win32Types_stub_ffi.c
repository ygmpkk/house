/* Auto generated GreenCard 2 code for FFI */
#include <stdlib.h>
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Types_stub_ffi.h"
void* prim_unmarshall_lpctstr_(void * arg1)
{ static struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { char* res1;
     size_t l = wcstombs(NULL,arg1,-1);
     if ((res1=malloc(sizeof(char)*l)) == NULL ) {
        res1 = NULL;
     } else {
        wcstombs(res1,arg1,-1);
     };
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring = ErrorWithCode("unmarshall_lpctstr_",0) ;}
      else {gc_failed = 0;}
      gc_result.gc_res2 = (char *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
       free(res1);
      return(&gc_result);} while(0);
}
char * access_prim_unmarshall_lpctstr__gc_res2(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_unmarshall_lpctstr__gc_failed(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_unmarshall_lpctstr__gc_failstring(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_marshall_lpctstr_(char * arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  void * res1;int gc_failed;
	      char* gc_failstring;
  do { wchar_t* res1;
     /* figure out how much to allocate */
     size_t l = mbstowcs(NULL,arg1,-1);
     if ((res1=malloc(sizeof(wchar_t)*l)) == NULL ) {
        res1 = NULL;
     } else {
        mbstowcs(res1,arg1,-1);
     };
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring = ErrorWithCode("marshall_lpctstr_",0) ;}
      else {gc_failed = 0;}
      gc_result.res1 = (void *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void * access_prim_marshall_lpctstr__res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_marshall_lpctstr__gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_marshall_lpctstr__gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
UINT prim_handleToWord(HANDLE arg1)
{ UINT res1;
  do { res1=(UINT)arg1;
      
      return((UINT)(res1));} while(0);
}
void * prim_nullHANDLE()
{ void * res1;
  do {res1=(HANDLE) NULL;
      
      return((void *)(res1));} while(0);
}
