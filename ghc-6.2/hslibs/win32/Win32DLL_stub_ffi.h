#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern void* prim_disableThreadLibraryCalls(HMODULE arg1);
extern int access_prim_disableThreadLibraryCalls_gc_failed(HsPtr);
extern void* access_prim_disableThreadLibraryCalls_gc_failstring(HsPtr);
extern void* prim_freeLibrary(HMODULE arg1);
extern int access_prim_freeLibrary_gc_failed(HsPtr);
extern void* access_prim_freeLibrary_gc_failstring(HsPtr);
extern void* prim_getModuleFileName(HMODULE arg1);
extern char * access_prim_getModuleFileName_res1(HsPtr);
extern int access_prim_getModuleFileName_gc_failed(HsPtr);
extern void* access_prim_getModuleFileName_gc_failstring(HsPtr);
extern void* prim_getModuleHandle(char * arg1);
extern HMODULE access_prim_getModuleHandle_res1(HsPtr);
extern int access_prim_getModuleHandle_gc_failed(HsPtr);
extern void* access_prim_getModuleHandle_gc_failstring(HsPtr);
extern void* prim_getProcAddress(HMODULE arg1,char * arg2);
extern void * access_prim_getProcAddress_res1(HsPtr);
extern int access_prim_getProcAddress_gc_failed(HsPtr);
extern void* access_prim_getProcAddress_gc_failstring(HsPtr);
extern void* prim_loadLibrary(char * arg1);
extern HINSTANCE access_prim_loadLibrary_res1(HsPtr);
extern int access_prim_loadLibrary_gc_failed(HsPtr);
extern void* access_prim_loadLibrary_gc_failstring(HsPtr);
extern DWORD prim_lOAD_LIBRARY_AS_DATAFILE();
extern DWORD prim_lOAD_WITH_ALTERED_SEARCH_PATH();
extern void* prim_loadLibraryEx(char * arg1,HANDLE arg2,DWORD arg3);
extern HINSTANCE access_prim_loadLibraryEx_res1(HsPtr);
extern int access_prim_loadLibraryEx_gc_failed(HsPtr);
extern void* access_prim_loadLibraryEx_gc_failstring(HsPtr);
