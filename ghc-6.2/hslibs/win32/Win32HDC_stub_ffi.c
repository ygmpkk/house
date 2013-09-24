/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32HDC_stub_ffi.h"
void* prim_setArcDirection(HDC arg1,WORD arg2)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = SetArcDirection(arg1, arg2);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("SetArcDirection")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_setArcDirection_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setArcDirection_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setArcDirection_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getArcDirection(HDC arg1)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetArcDirection(arg1);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("GetArcDirection")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_getArcDirection_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getArcDirection_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getArcDirection_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setPolyFillMode(HDC arg1,WORD arg2)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = SetPolyFillMode(arg1, arg2);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("SetPolyFillMode")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_setPolyFillMode_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setPolyFillMode_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setPolyFillMode_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getPolyFillMode(HDC arg1)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetPolyFillMode(arg1);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("GetPolyFillMode")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_getPolyFillMode_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getPolyFillMode_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getPolyFillMode_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setGraphicsMode(HDC arg1,DWORD arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  DWORD res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = SetGraphicsMode(arg1, arg2);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("SetGraphicsMode")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (DWORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
DWORD access_prim_setGraphicsMode_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setGraphicsMode_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setGraphicsMode_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getGraphicsMode(HDC arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  DWORD res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = GetGraphicsMode(arg1);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("GetGraphicsMode")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (DWORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
DWORD access_prim_getGraphicsMode_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getGraphicsMode_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getGraphicsMode_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setStretchBltMode(HDC arg1,UINT arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = SetStretchBltMode(arg1, arg2);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("SetStretchBltMode")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_setStretchBltMode_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setStretchBltMode_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setStretchBltMode_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getStretchBltMode(HDC arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetStretchBltMode(arg1);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("GetStretchBltMode")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_getStretchBltMode_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getStretchBltMode_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getStretchBltMode_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setBkColor(HDC arg1,DWORD arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  DWORD res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = SetBkColor(arg1, arg2);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("SetBkColor")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (DWORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
DWORD access_prim_setBkColor_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setBkColor_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setBkColor_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getBkColor(HDC arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  DWORD res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = GetBkColor(arg1);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("GetBkColor")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (DWORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
DWORD access_prim_getBkColor_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getBkColor_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getBkColor_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setTextColor(HDC arg1,DWORD arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  DWORD res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = SetTextColor(arg1, arg2);
      if ((gc_failed = ( res1== CLR_INVALID  ))) {gc_failstring =  ErrorString("SetTextColor")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (DWORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
DWORD access_prim_setTextColor_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setTextColor_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setTextColor_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getTextColor(HDC arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  DWORD res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = GetTextColor(arg1);
      if ((gc_failed = ( res1== CLR_INVALID  ))) {gc_failstring =  ErrorString("GetTextColor")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (DWORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
DWORD access_prim_getTextColor_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getTextColor_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getTextColor_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setBkMode(HDC arg1,UINT arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = SetBkMode(arg1, arg2);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("SetBkMode")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_setBkMode_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setBkMode_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setBkMode_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getBkMode(HDC arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetBkMode(arg1);
      if ((gc_failed = ( res1== 0  ))) {gc_failstring =  ErrorString("GetBkMode")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_getBkMode_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getBkMode_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getBkMode_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setBrushOrgEx(HDC arg1,int arg2,int arg3)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { POINT res1;
     BOOL success = SetBrushOrgEx(arg1,arg2,arg3,&res1);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("SetBrushOrgEx")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((res1).x);
      gc_result.gc_res2 = (LONG)((res1).y);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_setBrushOrgEx_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_setBrushOrgEx_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_setBrushOrgEx_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setBrushOrgEx_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getBrushOrgEx(HDC arg1)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { POINT res1;
     BOOL success = GetBrushOrgEx(arg1,&res1);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("GetBrushOrgEx")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((res1).x);
      gc_result.gc_res2 = (LONG)((res1).y);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_getBrushOrgEx_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_getBrushOrgEx_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_getBrushOrgEx_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getBrushOrgEx_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setTextAlign(HDC arg1,UINT arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = SetTextAlign(arg1, arg2);
      if ((gc_failed = ( res1== GDI_ERROR  ))) {gc_failstring =  ErrorWin("Set" "TextAlign")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_setTextAlign_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setTextAlign_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setTextAlign_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getTextAlign(HDC arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetTextAlign(arg1);
      if ((gc_failed = ( res1== GDI_ERROR  ))) {gc_failstring =  ErrorWin("Get" "TextAlign")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_getTextAlign_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getTextAlign_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getTextAlign_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setTextCharacterExtra(HDC arg1,int arg2)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = SetTextCharacterExtra(arg1, arg2);
      if ((gc_failed = ( res1== 0x80000000  ))) {gc_failstring =  ErrorString("SetTextCharacterExtra")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_setTextCharacterExtra_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setTextCharacterExtra_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setTextCharacterExtra_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getTextCharacterExtra(HDC arg1)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = GetTextCharacterExtra(arg1);
      if ((gc_failed = ( res1== 0x80000000  ))) {gc_failstring =  ErrorString("GetTextCharacterExtra")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_getTextCharacterExtra_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getTextCharacterExtra_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getTextCharacterExtra_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getMiterLimit(HDC arg1)
{ static struct {HsFloat res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  float res1;int gc_failed;
	     char* gc_failstring;
  do { BOOL success = GetMiterLimit(arg1,&res1);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("GetMiterLimit")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (float)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
float access_prim_getMiterLimit_res1(void *ptr){ return(((struct {HsFloat res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getMiterLimit_gc_failed(void *ptr){ return(((struct {HsFloat res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getMiterLimit_gc_failstring(void *ptr){ return(((struct {HsFloat res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setMiterLimit(HDC arg1,float arg2)
{ static struct {HsFloat res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  float res1;int gc_failed;
	     char* gc_failstring;
  do { BOOL success = SetMiterLimit(arg1,arg2,&res1);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("SetMiterLimit")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (float)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
float access_prim_setMiterLimit_res1(void *ptr){ return(((struct {HsFloat res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setMiterLimit_gc_failed(void *ptr){ return(((struct {HsFloat res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setMiterLimit_gc_failstring(void *ptr){ return(((struct {HsFloat res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_saveDC(HDC arg1)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = SaveDC(arg1);
      if ((gc_failed = ( res1==0 ))) {gc_failstring =  ErrorString("SaveDC")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_saveDC_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_saveDC_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_saveDC_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_restoreDC(HDC arg1,int arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = RestoreDC(arg1,arg2);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorString("RestoreDC")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_restoreDC_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_restoreDC_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getCurrentBitmap(HDC arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBITMAP res1;int gc_failed;
	       char* gc_failstring;
  do { res1 = ( HBITMAP ) GetCurrentObject(arg1,  OBJ_BITMAP ) ;
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("GetCurrentBitmap")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBITMAP)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBITMAP access_prim_getCurrentBitmap_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getCurrentBitmap_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getCurrentBitmap_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getCurrentBrush(HDC arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBRUSH res1;int gc_failed;
	      char* gc_failstring;
  do { res1 = (  HBRUSH ) GetCurrentObject(arg1,   OBJ_BRUSH ) ;
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("GetCurrentBrush")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBRUSH)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBRUSH access_prim_getCurrentBrush_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getCurrentBrush_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getCurrentBrush_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getCurrentFont(HDC arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HFONT res1;int gc_failed;
	     char* gc_failstring;
  do { res1 = (   HFONT ) GetCurrentObject(arg1,    OBJ_FONT ) ;
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("GetCurrentFont")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HFONT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HFONT access_prim_getCurrentFont_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getCurrentFont_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getCurrentFont_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getCurrentPalette(HDC arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HPALETTE res1;int gc_failed;
		char* gc_failstring;
  do { res1 = (    HPALETTE ) GetCurrentObject(arg1, OBJ_PAL ) ;
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("GetCurrentPalette")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HPALETTE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HPALETTE access_prim_getCurrentPalette_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getCurrentPalette_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getCurrentPalette_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getCurrentPen(HDC arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HPEN res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = (    HPEN ) GetCurrentObject(arg1,     OBJ_PEN ) ;
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("GetCurrentPen")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HPEN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HPEN access_prim_getCurrentPen_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getCurrentPen_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getCurrentPen_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_selectBitmap(HDC arg1,HBITMAP arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBITMAP res1;int gc_failed;
	       char* gc_failstring;
  do { res1 = (  HBITMAP ) SelectObject(arg1,arg2) ;
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("SelectBitmap")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBITMAP)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBITMAP access_prim_selectBitmap_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_selectBitmap_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_selectBitmap_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_selectBrush(HDC arg1,HBRUSH arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HBRUSH res1;int gc_failed;
	      char* gc_failstring;
  do { res1 = (   HBRUSH ) SelectObject(arg1,arg2) ;
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("SelectBrush")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HBRUSH)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HBRUSH access_prim_selectBrush_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_selectBrush_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_selectBrush_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_selectFont(HDC arg1,HFONT arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HFONT res1;int gc_failed;
	     char* gc_failstring;
  do { res1 = (    HFONT ) SelectObject(arg1,arg2) ;
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("SelectFont")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HFONT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HFONT access_prim_selectFont_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_selectFont_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_selectFont_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_selectPen(HDC arg1,HPEN arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HPEN res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = (     HPEN ) SelectObject(arg1,arg2) ;
      if ((gc_failed = ( res1==NULL ))) {gc_failstring =  ErrorString("SelectPen")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HPEN)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HPEN access_prim_selectPen_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_selectPen_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_selectPen_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_selectPalette(HDC arg1,HPALETTE arg2,int arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HPALETTE res1;int gc_failed;
		char* gc_failstring;
  do {res1 = SelectPalette(arg1, arg2, arg3);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("SelectPalette") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HPALETTE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HPALETTE access_prim_selectPalette_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_selectPalette_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_selectPalette_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_selectRgn(HDC arg1,HRGN arg2)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = (int)SelectObject(arg1,arg2);
      if ((gc_failed = ( res1==GDI_ERROR ))) {gc_failstring =  ErrorString("SelectRgn")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_selectRgn_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_selectRgn_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_selectRgn_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_selectClipRgn(HDC arg1,HRGN arg2)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = SelectClipRgn(arg1, arg2);
      if ((gc_failed = ( res1==ERROR ))) {gc_failstring =  ErrorString("SelectClipRgn")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_selectClipRgn_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_selectClipRgn_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_selectClipRgn_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_extSelectClipRgn(HDC arg1,HRGN arg2,UINT arg3)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = ExtSelectClipRgn(arg1, arg2, arg3);
      if ((gc_failed = ( res1==ERROR ))) {gc_failstring =  ErrorString("ExtSelectClipRgn")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_extSelectClipRgn_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_extSelectClipRgn_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_extSelectClipRgn_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_selectClipPath(HDC arg1,UINT arg2)
{ static struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  WORD res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = SelectClipPath(arg1, arg2);
      if ((gc_failed = ( res1==0 ))) {gc_failstring =  ErrorWin("SelectClipPath")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (WORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
WORD access_prim_selectClipPath_res1(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_selectClipPath_gc_failed(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_selectClipPath_gc_failstring(void *ptr){ return(((struct {HsWord16 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_cancelDC(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = CancelDC(arg1);
      if ((gc_failed = ( !success ))) {gc_failstring = "CancelDC" ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_cancelDC_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_cancelDC_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createCompatibleDC(HDC arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HDC res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = CreateCompatibleDC(arg1);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = "CreateCompatibleDC" ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HDC)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HDC access_prim_createCompatibleDC_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createCompatibleDC_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createCompatibleDC_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_deleteDC(HDC arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = DeleteDC(arg1);
      if ((gc_failed = ( !success ))) {gc_failstring = "DeleteDC" ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_deleteDC_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_deleteDC_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
