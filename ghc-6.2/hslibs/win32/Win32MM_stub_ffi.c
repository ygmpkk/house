/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32MM_stub_ffi.h"
void prim_copyMemory(void * arg1,void * arg2,DWORD arg3)
{ do {CopyMemory(arg1, arg2, arg3);
      ;} while(0);
}
void prim_fillMemory(void * arg1,DWORD arg2,BYTE arg3)
{ do {FillMemory(arg1, arg2, arg3);
      ;} while(0);
}
HANDLE prim_getProcessHeap()
{ HANDLE res1;
  do {res1 = GetProcessHeap();
      
      return((HANDLE)(res1));} while(0);
}
DWORD prim_getProcessHeaps(DWORD arg1,void * arg2)
{ DWORD res1;
  do {res1 = GetProcessHeaps(arg1, arg2);
      
      return((DWORD)(res1));} while(0);
}
UINT prim_gMEM_FIXED()
{ UINT res1;
  do {res1=GMEM_FIXED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMEM_MOVEABLE()
{ UINT res1;
  do {res1=GMEM_MOVEABLE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gPTR()
{ UINT res1;
  do {res1=GPTR;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gHND()
{ UINT res1;
  do {res1=GHND;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMEM_DDESHARE()
{ UINT res1;
  do {res1=GMEM_DDESHARE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMEM_SHARE()
{ UINT res1;
  do {res1=GMEM_SHARE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMEM_LOWER()
{ UINT res1;
  do {res1=GMEM_LOWER;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMEM_NOCOMPACT()
{ UINT res1;
  do {res1=GMEM_NOCOMPACT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMEM_NODISCARD()
{ UINT res1;
  do {res1=GMEM_NODISCARD;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMEM_NOT_BANKED()
{ UINT res1;
  do {res1=GMEM_NOT_BANKED;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMEM_NOTIFY()
{ UINT res1;
  do {res1=GMEM_NOTIFY;
      
      return((UINT)(res1));} while(0);
}
UINT prim_gMEM_ZEROINIT()
{ UINT res1;
  do {res1=GMEM_ZEROINIT;
      
      return((UINT)(res1));} while(0);
}
void* prim_globalAlloc(UINT arg1,DWORD arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HGLOBAL res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = GlobalAlloc(arg1, arg2);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("GlobalAlloc") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HGLOBAL)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HGLOBAL access_prim_globalAlloc_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_globalAlloc_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_globalAlloc_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_globalFlags(HGLOBAL arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GlobalFlags(arg1);
      if ((gc_failed = ( res1==GMEM_INVALID_HANDLE ))) {gc_failstring = ErrorWin("GlobalFlags") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_globalFlags_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_globalFlags_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_globalFlags_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_globalFree(HGLOBAL arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HGLOBAL res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = GlobalFree(arg1);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("GlobalFree") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HGLOBAL)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HGLOBAL access_prim_globalFree_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_globalFree_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_globalFree_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_globalHandle(void * arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HGLOBAL res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = GlobalHandle(arg1);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("GlobalHandle") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HGLOBAL)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HGLOBAL access_prim_globalHandle_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_globalHandle_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_globalHandle_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_globalLock(HGLOBAL arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  void * res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = GlobalLock(arg1);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("GlobalLock") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (void *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void * access_prim_globalLock_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_globalLock_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_globalLock_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_globalReAlloc(HGLOBAL arg1,DWORD arg2,UINT arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HGLOBAL res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = GlobalReAlloc(arg1, arg2, arg3);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("GlobalReAlloc") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HGLOBAL)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HGLOBAL access_prim_globalReAlloc_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_globalReAlloc_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_globalReAlloc_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_globalSize(HGLOBAL arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  DWORD res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = GlobalSize(arg1);
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("GlobalSize") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (DWORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
DWORD access_prim_globalSize_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_globalSize_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_globalSize_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_globalUnlock(HGLOBAL arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res=GlobalSize(arg1);
      if ((gc_failed = ( res==0 ))) {gc_failstring = ErrorWin("GlobalUnlock") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_globalUnlock_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_globalUnlock_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_hEAP_GENERATE_EXCEPTIONS()
{ DWORD res1;
  do {res1=HEAP_GENERATE_EXCEPTIONS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_hEAP_NO_SERIALIZE()
{ DWORD res1;
  do {res1=HEAP_NO_SERIALIZE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_hEAP_ZERO_MEMORY()
{ DWORD res1;
  do {res1=HEAP_ZERO_MEMORY;
      
      return((DWORD)(res1));} while(0);
}
void* prim_heapAlloc(HANDLE arg1,DWORD arg2,DWORD arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  void * res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = HeapAlloc(arg1, arg2, arg3);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("HeapAlloc") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (void *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void * access_prim_heapAlloc_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_heapAlloc_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_heapAlloc_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_heapCompact(HANDLE arg1,DWORD arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = HeapCompact(arg1, arg2);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("HeapCompact") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_heapCompact_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_heapCompact_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_heapCompact_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_heapCreate(DWORD arg1,DWORD arg2,DWORD arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HANDLE res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = HeapCreate(arg1, arg2, arg3);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("HeapCreate") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HANDLE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HANDLE access_prim_heapCreate_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_heapCreate_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_heapCreate_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_heapDestroy(HANDLE arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=HeapDestroy(arg1);
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("HeapDestroy") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_heapDestroy_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_heapDestroy_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_heapFree(HANDLE arg1,DWORD arg2,void * arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=HeapFree(arg1,arg2,arg3);
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("HeapFree") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_heapFree_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_heapFree_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_heapLock(HANDLE arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=HeapLock(arg1);
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("HeapLock") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_heapLock_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_heapLock_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_heapReAlloc(HANDLE arg1,DWORD arg2,void * arg3,DWORD arg4)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  void * res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = HeapReAlloc(arg1, arg2, arg3, arg4);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("HeapReAlloc") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (void *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void * access_prim_heapReAlloc_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_heapReAlloc_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_heapReAlloc_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_heapSize(HANDLE arg1,DWORD arg2,void * arg3)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  DWORD res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = HeapSize(arg1, arg2, arg3);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("HeapSize") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (DWORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
DWORD access_prim_heapSize_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_heapSize_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_heapSize_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_heapUnlock(HANDLE arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=HeapUnlock(arg1);
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("HeapUnlock") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_heapUnlock_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_heapUnlock_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_heapValidate(HANDLE arg1,DWORD arg2,void * arg3)
{ int res1;
  do {res1 = HeapValidate(arg1, arg2, arg3);
      
      return((int)(res1));} while(0);
}
void prim_moveMemory(void * arg1,void * arg2,DWORD arg3)
{ do {MoveMemory(arg1, arg2, arg3);
      ;} while(0);
}
DWORD prim_mEM_COMMIT()
{ DWORD res1;
  do {res1=MEM_COMMIT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_mEM_RESERVE()
{ DWORD res1;
  do {res1=MEM_RESERVE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pAGE_READONLY()
{ DWORD res1;
  do {res1=PAGE_READONLY;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pAGE_READWRITE()
{ DWORD res1;
  do {res1=PAGE_READWRITE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pAGE_EXECUTE()
{ DWORD res1;
  do {res1=PAGE_EXECUTE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pAGE_EXECUTE_READ()
{ DWORD res1;
  do {res1=PAGE_EXECUTE_READ;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pAGE_EXECUTE_READWRITE()
{ DWORD res1;
  do {res1=PAGE_EXECUTE_READWRITE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pAGE_GUARD()
{ DWORD res1;
  do {res1=PAGE_GUARD;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pAGE_NOACCESS()
{ DWORD res1;
  do {res1=PAGE_NOACCESS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pAGE_NOCACHE()
{ DWORD res1;
  do {res1=PAGE_NOCACHE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_mEM_DECOMMIT()
{ DWORD res1;
  do {res1=MEM_DECOMMIT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_mEM_RELEASE()
{ DWORD res1;
  do {res1=MEM_RELEASE;
      
      return((DWORD)(res1));} while(0);
}
void* prim_virtualAlloc(void * arg1,DWORD arg2,DWORD arg3,DWORD arg4)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  void * res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = VirtualAlloc(arg1, arg2, arg3, arg4);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("VirtualAlloc") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (void *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void * access_prim_virtualAlloc_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_virtualAlloc_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_virtualAlloc_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_virtualFree(void * arg1,DWORD arg2,DWORD arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=VirtualFree(arg1,arg2,arg3);
      if ((gc_failed = ( res1=0 ))) {gc_failstring = ErrorWin("VirtualFree") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_virtualFree_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_virtualFree_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_virtualLock(void * arg1,DWORD arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=VirtualLock(arg1,arg2);
      if ((gc_failed = ( res1=0 ))) {gc_failstring = ErrorWin("VirtualLock") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_virtualLock_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_virtualLock_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_virtualProtect(void * arg1,DWORD arg2,DWORD arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=VirtualLock(arg1,arg2);
      if ((gc_failed = ( res1=0 ))) {gc_failstring = ErrorWin("VirtualProtect") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_virtualProtect_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_virtualProtect_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_virtualProtectEx(HANDLE arg1,void * arg2,DWORD arg3,DWORD arg4,void * arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=VirtualProtectEx(arg1,arg2,arg3,arg4,arg5);
      if ((gc_failed = ( res1=0 ))) {gc_failstring = ErrorWin("VirtualProtectEx") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_virtualProtectEx_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_virtualProtectEx_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_virtualUnlock(void * arg1,DWORD arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=VirtualUnlock(arg1,arg2);
      if ((gc_failed = ( res1=0 ))) {gc_failstring = ErrorWin("VirtualUnlock") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_virtualUnlock_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_virtualUnlock_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void prim_zeroMemory(void * arg1,DWORD arg2)
{ do {ZeroMemory(arg1, arg2);
      ;} while(0);
}
