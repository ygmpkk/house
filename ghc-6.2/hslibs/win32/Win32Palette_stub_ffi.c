/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Palette_stub_ffi.h"
WORD prim_dEFAULT_PALETTE()
{ WORD res1;
  do {res1=DEFAULT_PALETTE;
      
      return((WORD)(res1));} while(0);
}
void* prim_getStockPalette(WORD arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HPALETTE res1;int gc_failed;
		char* gc_failstring;
  do { res1 = GetStockObject(arg1);
      if ((gc_failed = (  res1 == (HPALETTE) NULL  ))) {gc_failstring =  ErrorString("GetStockPalette")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HPALETTE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HPALETTE access_prim_getStockPalette_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getStockPalette_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getStockPalette_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_deletePalette(HPALETTE arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DeleteObject(arg1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("DeletePalette")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_deletePalette_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_deletePalette_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_pALETTERGB(BYTE arg1,BYTE arg2,BYTE arg3)
{ DWORD res1;
  do {res1 = PALETTERGB(arg1, arg2, arg3);
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_pALETTEINDEX(WORD arg1)
{ DWORD res1;
  do {res1 = PALETTEINDEX(arg1);
      
      return((DWORD)(res1));} while(0);
}
