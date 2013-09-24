/* Auto generated GreenCard 2 code for FFI */
#include "Win32Aux.h"
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "dumpBMP.h"
#include "Win32Bitmap_stub_ffi.h"
unsigned int prim_sRCCOPY()
{ unsigned int res1;
  do {res1=SRCCOPY;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_sRCPAINT()
{ unsigned int res1;
  do {res1=SRCPAINT;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_sRCAND()
{ unsigned int res1;
  do {res1=SRCAND;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_sRCINVERT()
{ unsigned int res1;
  do {res1=SRCINVERT;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_sRCERASE()
{ unsigned int res1;
  do {res1=SRCERASE;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_nOTSRCCOPY()
{ unsigned int res1;
  do {res1=NOTSRCCOPY;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_nOTSRCERASE()
{ unsigned int res1;
  do {res1=NOTSRCERASE;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_mERGECOPY()
{ unsigned int res1;
  do {res1=MERGECOPY;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_mERGEPAINT()
{ unsigned int res1;
  do {res1=MERGEPAINT;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_pATCOPY()
{ unsigned int res1;
  do {res1=PATCOPY;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_pATPAINT()
{ unsigned int res1;
  do {res1=PATPAINT;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_pATINVERT()
{ unsigned int res1;
  do {res1=PATINVERT;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_dSTINVERT()
{ unsigned int res1;
  do {res1=DSTINVERT;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_bLACKNESS()
{ unsigned int res1;
  do {res1=BLACKNESS;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_wHITENESS()
{ unsigned int res1;
  do {res1=WHITENESS;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_mAKEROP4(unsigned int arg1,unsigned int arg2)
{ unsigned int res1;
  do {res1 = MAKEROP4(arg1, arg2);
      
      return((unsigned int)(res1));} while(0);
}
void prim_setBITMAP(BITMAP * arg1,LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5,WORD gc_arg6,WORD gc_arg7,LPVOID gc_arg8)
{ (*arg1).bmType = (LONG)gc_arg2; (*arg1).bmWidth = (LONG)gc_arg3; (*arg1).bmHeight = (LONG)gc_arg4; (*arg1).bmWidthBytes = (LONG)gc_arg5; (*arg1).bmPlanes = (WORD)gc_arg6; (*arg1).bmBitsPixel = (WORD)gc_arg7; (*arg1).bmBits = (LPVOID)gc_arg8;
  do {;} while(0);
}
void* prim_deleteBitmap(HBITMAP arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res = DeleteObject(arg1);
      if ((gc_failed = (  !res  ))) {gc_failstring =  ErrorString("DeleteBitmap")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_deleteBitmap_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_deleteBitmap_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createCompatibleBitmap(HDC arg1,int arg2,int arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBITMAP res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = CreateCompatibleBitmap(arg1, arg2, arg3);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("CreateCompatibleBitmap")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBITMAP)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBITMAP access_prim_createCompatibleBitmap_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createCompatibleBitmap_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createCompatibleBitmap_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createBitmap(INT arg1,INT arg2,UINT arg3,UINT arg4,LPVOID arg5)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBITMAP res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = CreateBitmap(arg1, arg2, arg3, arg4, arg5);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("CreateBitmap")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBITMAP)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBITMAP access_prim_createBitmap_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createBitmap_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createBitmap_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createBitmapIndirect(BITMAP * arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBITMAP res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = CreateBitmapIndirect(arg1);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("CreateBitmapIndirect")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBITMAP)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBITMAP access_prim_createBitmapIndirect_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createBitmapIndirect_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createBitmapIndirect_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createDIBPatternBrushPt(LPVOID arg1,DWORD arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBRUSH res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = CreateDIBPatternBrushPt(arg1, arg2);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("CreateDIBPatternBrushPt")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBRUSH)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBRUSH access_prim_createDIBPatternBrushPt_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createDIBPatternBrushPt_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createDIBPatternBrushPt_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getBitmapDimensionEx(HBITMAP h)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { SIZE sz; 
     BOOL success = GetBitmapDimensionEx(h,&sz);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("GetBitmapDimensionEx")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((sz).cx);
      gc_result.gc_res2 = (LONG)((sz).cy);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_getBitmapDimensionEx_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_getBitmapDimensionEx_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_getBitmapDimensionEx_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getBitmapDimensionEx_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setBitmapDimensionEx(HBITMAP h,LONG gc_arg2,LONG gc_arg3)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;SIZE sz_in;
  (sz_in).cx = (LONG)gc_arg2; (sz_in).cy = (LONG)gc_arg3;
  do { SIZE sz_out;
     BOOL success = SetBitmapDimensionEx(h,sz_in.cx,sz_in.cy,&sz_out);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("SetBitmapDimensionEx")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((sz_out).cx);
      gc_result.gc_res2 = (LONG)((sz_out).cy);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_setBitmapDimensionEx_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_setBitmapDimensionEx_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_setBitmapDimensionEx_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setBitmapDimensionEx_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getBitmapInfo(HBITMAP x)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BITMAP bm;
     int nbytes = GetObject(x,sizeof(BITMAP),&bm);
      if ((gc_failed = (  nbytes != sizeof(BITMAP)  ))) {gc_failstring =  ErrorWin("getBitmapInfo")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((bm).bmType);
      gc_result.gc_res2 = (LONG)((bm).bmWidth);
      gc_result.gc_res3 = (LONG)((bm).bmHeight);
      gc_result.gc_res4 = (LONG)((bm).bmWidthBytes);
      gc_result.gc_res5 = (WORD)((bm).bmPlanes);
      gc_result.gc_res6 = (WORD)((bm).bmBitsPixel);
      gc_result.gc_res7 = (LPVOID)((bm).bmBits);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_getBitmapInfo_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_getBitmapInfo_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
LONG access_prim_getBitmapInfo_gc_res3(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
LONG access_prim_getBitmapInfo_gc_res4(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res4);}
WORD access_prim_getBitmapInfo_gc_res5(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res5);}
WORD access_prim_getBitmapInfo_gc_res6(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res6);}
LPVOID access_prim_getBitmapInfo_gc_res7(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res7);}
int access_prim_getBitmapInfo_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getBitmapInfo_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsInt32 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsPtr gc_res7;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
WORD prim_bI_RGB()
{ WORD res1;
  do {res1=BI_RGB;
      
      return((WORD)(res1));} while(0);
}
WORD prim_bI_RLE8()
{ WORD res1;
  do {res1=BI_RLE8;
      
      return((WORD)(res1));} while(0);
}
WORD prim_bI_RLE4()
{ WORD res1;
  do {res1=BI_RLE4;
      
      return((WORD)(res1));} while(0);
}
WORD prim_bI_BITFIELDS()
{ WORD res1;
  do {res1=BI_BITFIELDS;
      
      return((WORD)(res1));} while(0);
}
DWORD prim_dIB_PAL_COLORS()
{ DWORD res1;
  do {res1=DIB_PAL_COLORS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dIB_RGB_COLORS()
{ DWORD res1;
  do {res1=DIB_RGB_COLORS;
      
      return((DWORD)(res1));} while(0);
}
void* prim_getBITMAPINFOHEADER_(BITMAPINFOHEADER * arg1)
{ static struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;} gc_result;
  do {gc_result.gc_res1 = (DWORD)((*arg1).biSize);
      gc_result.gc_res2 = (LONG)((*arg1).biWidth);
      gc_result.gc_res3 = (LONG)((*arg1).biHeight);
      gc_result.gc_res4 = (WORD)((*arg1).biPlanes);
      gc_result.gc_res5 = (WORD)((*arg1).biBitCount);
      gc_result.gc_res6 = (WORD)((*arg1).biCompression);
      gc_result.gc_res7 = (DWORD)((*arg1).biSizeImage);
      gc_result.gc_res8 = (LONG)((*arg1).biXPelsPerMeter);
      gc_result.gc_res9 = (LONG)((*arg1).biYPelsPerMeter);
      gc_result.gc_res11 = (DWORD)((*arg1).biClrUsed);
      gc_result.gc_res13 = (DWORD)((*arg1).biClrImportant);
      
      return(&gc_result);} while(0);
}
DWORD access_prim_getBITMAPINFOHEADER__gc_res1(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res1);}
LONG access_prim_getBITMAPINFOHEADER__gc_res2(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res2);}
LONG access_prim_getBITMAPINFOHEADER__gc_res3(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res3);}
WORD access_prim_getBITMAPINFOHEADER__gc_res4(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res4);}
WORD access_prim_getBITMAPINFOHEADER__gc_res5(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res5);}
WORD access_prim_getBITMAPINFOHEADER__gc_res6(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res6);}
DWORD access_prim_getBITMAPINFOHEADER__gc_res7(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res7);}
LONG access_prim_getBITMAPINFOHEADER__gc_res8(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res8);}
LONG access_prim_getBITMAPINFOHEADER__gc_res9(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res9);}
DWORD access_prim_getBITMAPINFOHEADER__gc_res11(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res11);}
DWORD access_prim_getBITMAPINFOHEADER__gc_res13(void *ptr){ return(((struct {HsWord32 gc_res1;HsInt32 gc_res2;HsInt32 gc_res3;HsWord16 gc_res4;HsWord16 gc_res5;HsWord16 gc_res6;HsWord32 gc_res7;HsInt32 gc_res8;HsInt32 gc_res9;HsWord32 gc_res11;HsWord32 gc_res13;}*) ptr)->gc_res13);}
void* prim_getBITMAPFILEHEADER(BITMAPFILEHEADER * arg1)
{ static struct {HsWord16 gc_res1;HsWord32 gc_res2;HsWord16 gc_res3;HsWord16 gc_res4;HsWord32 gc_res5;} gc_result;
  do {gc_result.gc_res1 = (WORD)((*arg1).bfType);
      gc_result.gc_res2 = (DWORD)((*arg1).bfSize);
      gc_result.gc_res3 = (WORD)((*arg1).bfReserved1);
      gc_result.gc_res4 = (WORD)((*arg1).bfReserved2);
      gc_result.gc_res5 = (DWORD)((*arg1).bfOffBits);
      
      return(&gc_result);} while(0);
}
WORD access_prim_getBITMAPFILEHEADER_gc_res1(void *ptr){ return(((struct {HsWord16 gc_res1;HsWord32 gc_res2;HsWord16 gc_res3;HsWord16 gc_res4;HsWord32 gc_res5;}*) ptr)->gc_res1);}
DWORD access_prim_getBITMAPFILEHEADER_gc_res2(void *ptr){ return(((struct {HsWord16 gc_res1;HsWord32 gc_res2;HsWord16 gc_res3;HsWord16 gc_res4;HsWord32 gc_res5;}*) ptr)->gc_res2);}
WORD access_prim_getBITMAPFILEHEADER_gc_res3(void *ptr){ return(((struct {HsWord16 gc_res1;HsWord32 gc_res2;HsWord16 gc_res3;HsWord16 gc_res4;HsWord32 gc_res5;}*) ptr)->gc_res3);}
WORD access_prim_getBITMAPFILEHEADER_gc_res4(void *ptr){ return(((struct {HsWord16 gc_res1;HsWord32 gc_res2;HsWord16 gc_res3;HsWord16 gc_res4;HsWord32 gc_res5;}*) ptr)->gc_res4);}
DWORD access_prim_getBITMAPFILEHEADER_gc_res5(void *ptr){ return(((struct {HsWord16 gc_res1;HsWord32 gc_res2;HsWord16 gc_res3;HsWord16 gc_res4;HsWord32 gc_res5;}*) ptr)->gc_res5);}
unsigned int prim_sizeofBITMAP()
{ unsigned int res1;
  do {res1= sizeof(BITMAP)           ;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_sizeofBITMAPINFO()
{ unsigned int res1;
  do {res1= sizeof(BITMAPINFO)       ;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_sizeofBITMAPINFOHEADER()
{ unsigned int res1;
  do {res1= sizeof(BITMAPINFOHEADER) ;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_sizeofBITMAPFILEHEADER()
{ unsigned int res1;
  do {res1= sizeof(BITMAPFILEHEADER) ;
      
      return((unsigned int)(res1));} while(0);
}
unsigned int prim_sizeofLPBITMAPFILEHEADER()
{ unsigned int res1;
  do {res1= sizeof(BITMAPFILEHEADER) ;
      
      return((unsigned int)(res1));} while(0);
}
void prim_createBMPFile(char * arg1,HBITMAP arg2,HDC arg3)
{ do {CreateBMPFile(arg1, arg2, arg3);
      ;} while(0);
}
DWORD prim_cBM_INIT()
{ DWORD res1;
  do {res1=CBM_INIT;
      
      return((DWORD)(res1));} while(0);
}
void* prim_getDIBits(HDC arg1,HBITMAP arg2,INT arg3,INT arg4,LPVOID arg5,BITMAPINFO * arg6,DWORD arg7)
{ static struct {HsInt32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  INT res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = GetDIBits(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("GetDIBits")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (INT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
INT access_prim_getDIBits_res1(void *ptr){ return(((struct {HsInt32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getDIBits_gc_failed(void *ptr){ return(((struct {HsInt32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getDIBits_gc_failstring(void *ptr){ return(((struct {HsInt32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setDIBits(HDC arg1,HBITMAP arg2,INT arg3,INT arg4,LPVOID arg5,BITMAPINFO * arg6,DWORD arg7)
{ static struct {HsInt32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  INT res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = SetDIBits(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorWin("SetDIBits")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (INT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
INT access_prim_setDIBits_res1(void *ptr){ return(((struct {HsInt32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setDIBits_gc_failed(void *ptr){ return(((struct {HsInt32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setDIBits_gc_failstring(void *ptr){ return(((struct {HsInt32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createDIBitmap(HDC arg1,BITMAPINFOHEADER * arg2,DWORD arg3,LPVOID arg4,BITMAPINFO * arg5,DWORD arg6)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBITMAP res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = CreateDIBitmap(arg1, arg2, arg3, arg4, arg5, arg6);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("CreateDIBitmap")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBITMAP)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBITMAP access_prim_createDIBitmap_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createDIBitmap_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createDIBitmap_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
