#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "HsFFI.h"
extern WORD prim_dEFAULT_PALETTE();
extern void* prim_getStockPalette(WORD arg1);
extern HPALETTE access_prim_getStockPalette_res1(HsPtr);
extern int access_prim_getStockPalette_gc_failed(HsPtr);
extern void* access_prim_getStockPalette_gc_failstring(HsPtr);
extern void* prim_deletePalette(HPALETTE arg1);
extern int access_prim_deletePalette_gc_failed(HsPtr);
extern void* access_prim_deletePalette_gc_failstring(HsPtr);
extern DWORD prim_pALETTERGB(BYTE arg1,BYTE arg2,BYTE arg3);
extern DWORD prim_pALETTEINDEX(WORD arg1);
