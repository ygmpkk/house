#include <stdlib.h>
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern void* prim_unmarshall_lpctstr_(void * arg1);
extern char * access_prim_unmarshall_lpctstr__gc_res2(HsPtr);
extern int access_prim_unmarshall_lpctstr__gc_failed(HsPtr);
extern void* access_prim_unmarshall_lpctstr__gc_failstring(HsPtr);
extern void* prim_marshall_lpctstr_(char * arg1);
extern void * access_prim_marshall_lpctstr__res1(HsPtr);
extern int access_prim_marshall_lpctstr__gc_failed(HsPtr);
extern void* access_prim_marshall_lpctstr__gc_failstring(HsPtr);
extern UINT prim_handleToWord(HANDLE arg1);
extern void * prim_nullHANDLE();
