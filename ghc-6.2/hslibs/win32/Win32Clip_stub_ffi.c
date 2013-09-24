/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Clip_stub_ffi.h"
UINT prim_cF_BITMAP()
{ UINT res1;
  do {res1=CF_BITMAP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_DIB()
{ UINT res1;
  do {res1=CF_DIB;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_DIF()
{ UINT res1;
  do {res1=CF_DIF;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_DSPBITMAP()
{ UINT res1;
  do {res1=CF_DSPBITMAP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_DSPENHMETAFILE()
{ UINT res1;
  do {res1=CF_DSPENHMETAFILE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_DSPMETAFILEPICT()
{ UINT res1;
  do {res1=CF_DSPMETAFILEPICT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_DSPTEXT()
{ UINT res1;
  do {res1=CF_DSPTEXT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_ENHMETAFILE()
{ UINT res1;
  do {res1=CF_ENHMETAFILE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_GDIOBJFIRST()
{ UINT res1;
  do {res1=CF_GDIOBJFIRST;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_HDROP()
{ UINT res1;
  do {res1=CF_HDROP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_LOCALE()
{ UINT res1;
  do {res1=CF_LOCALE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_METAFILEPICT()
{ UINT res1;
  do {res1=CF_METAFILEPICT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_OEMTEXT()
{ UINT res1;
  do {res1=CF_OEMTEXT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_OWNERDISPLAY()
{ UINT res1;
  do {res1=CF_OWNERDISPLAY;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_PALETTE()
{ UINT res1;
  do {res1=CF_PALETTE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_PENDATA()
{ UINT res1;
  do {res1=CF_PENDATA;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_PRIVATEFIRST()
{ UINT res1;
  do {res1=CF_PRIVATEFIRST;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_PRIVATELAST()
{ UINT res1;
  do {res1=CF_PRIVATELAST;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_RIFF()
{ UINT res1;
  do {res1=CF_RIFF;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_SYLK()
{ UINT res1;
  do {res1=CF_SYLK;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_TEXT()
{ UINT res1;
  do {res1=CF_TEXT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_WAVE()
{ UINT res1;
  do {res1=CF_WAVE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_cF_TIFF()
{ UINT res1;
  do {res1=CF_TIFF;
      
      return((UINT)(res1));} while(0);
}
int prim_changeClipboardChain(HWND arg1,HWND arg2)
{ int res1;
  do {res1 = ChangeClipboardChain(arg1, arg2);
      
      return((int)(res1));} while(0);
}
void* prim_closeClipboard()
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=CloseClipboard();
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("CloseClipboard") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_closeClipboard_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_closeClipboard_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_countClipboardFormats()
{ int res1;
  do {res1 = CountClipboardFormats();
      
      return((int)(res1));} while(0);
}
void* prim_emptyClipboard()
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=EmptyClipboard();
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("EmptyClipboard") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_emptyClipboard_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_emptyClipboard_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_enumClipboardFormats(UINT arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do { UINT res1=EnumClipboardFormats(arg1);
      if ((gc_failed = ( res1==0 && GetLastError() != NO_ERROR ))) {gc_failstring = ErrorWin("EnumClipboardFormats") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_enumClipboardFormats_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_enumClipboardFormats_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_enumClipboardFormats_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getClipboardData(UINT arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HANDLE res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = GetClipboardData(arg1);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("GetClipboardData") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HANDLE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HANDLE access_prim_getClipboardData_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getClipboardData_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getClipboardData_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getClipboardFormatName(UINT arg1,void * arg2,int arg3)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = GetClipboardFormatName(arg1, arg2, arg3);
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("GetClipboardFormatName") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_getClipboardFormatName_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getClipboardFormatName_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getClipboardFormatName_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getClipboardOwner()
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetClipboardOwner();
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("GetClipboardOwner") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_getClipboardOwner_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getClipboardOwner_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getClipboardOwner_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getClipboardViewer()
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetClipboardViewer();
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("GetClipboardViewer") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_getClipboardViewer_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getClipboardViewer_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getClipboardViewer_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getOpenClipboardWindow()
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetOpenClipboardWindow();
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("GetClipboardWindow") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_getOpenClipboardWindow_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getOpenClipboardWindow_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getOpenClipboardWindow_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getPriorityClipboardFormat(void * arg1,int arg2)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = GetPriorityClipboardFormat(arg1, arg2);
      if ((gc_failed = ( res1==-1 ))) {gc_failstring = ErrorWin("GetPriorityClipboardFormat") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_getPriorityClipboardFormat_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getPriorityClipboardFormat_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getPriorityClipboardFormat_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
BOOL prim_isClipboardFormatAvailable(UINT arg1)
{ BOOL res1;
  do {res1 = IsClipboardFormatAvailable(arg1);
      
      return((BOOL)(res1));} while(0);
}
void* prim_openClipboard(HWND arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1=OpenClipboard(arg1);
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("OpenClipboard") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_openClipboard_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_openClipboard_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_registerClipboardFormat(char * arg1)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = RegisterClipboardFormat(arg1);
      if ((gc_failed = ( res1==0 ))) {gc_failstring = ErrorWin("RegisterClipboardFormat") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_registerClipboardFormat_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_registerClipboardFormat_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_registerClipboardFormat_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setClipboardData(UINT arg1,HANDLE arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HANDLE res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = SetClipboardData(arg1, arg2);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("SetClipboardData") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HANDLE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HANDLE access_prim_setClipboardData_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setClipboardData_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setClipboardData_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setClipboardViewer(HWND arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = SetClipboardViewer(arg1);
      if ((gc_failed = ( res1==NULL ))) {gc_failstring = ErrorWin("SetClipboardViewer") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_setClipboardViewer_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setClipboardViewer_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setClipboardViewer_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
