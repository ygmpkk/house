/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Resource_stub_ffi.h"
void* prim_beginUpdateResource(char * arg1,int arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HANDLE res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = BeginUpdateResource(arg1, arg2);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("BeginUpdateResource") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HANDLE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HANDLE access_prim_beginUpdateResource_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_beginUpdateResource_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_beginUpdateResource_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
UINT prim_iMAGE_BITMAP()
{ UINT res1;
  do {res1=IMAGE_BITMAP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_iMAGE_ICON()
{ UINT res1;
  do {res1=IMAGE_ICON;
      
      return((UINT)(res1));} while(0);
}
UINT prim_iMAGE_CURSOR()
{ UINT res1;
  do {res1=IMAGE_CURSOR;
      
      return((UINT)(res1));} while(0);
}
void* prim_copyImage(HANDLE arg1,UINT arg2,int arg3,int arg4,UINT arg5)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HANDLE res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = CopyImage(arg1, arg2, arg3, arg4, arg5);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("CopyImage") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HANDLE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HANDLE access_prim_copyImage_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_copyImage_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_copyImage_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_endUpdateResource(HANDLE arg1,BOOL arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1 = EndUpdateResource(arg1,arg2);
      if ((gc_failed = ( res1 == FALSE ))) {gc_failstring = ErrorWin("EndUpdateResource") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_endUpdateResource_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_endUpdateResource_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void * prim_rT_ACCELERATOR()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_ACCELERATOR) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_ANICURSOR()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_ANICURSOR) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_ANIICON()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_ANIICON) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_BITMAP()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_BITMAP) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_CURSOR()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_CURSOR) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_DIALOG()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_DIALOG) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_FONT()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_FONT) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_FONTDIR()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_FONTDIR) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_GROUP_CURSOR()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_GROUP_CURSOR) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_GROUP_ICON()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_GROUP_ICON) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_HTML()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_HTML) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_ICON()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_ICON) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_MENU()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_MENU) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_MESSAGETABLE()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_MESSAGETABLE) ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_RCDATA()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_RCDATA)  ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_STRING()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_STRING)  ;
      
      return((void *)(res1));} while(0);
}
void * prim_rT_VERSION()
{ void * res1;
  do {res1= MAKEINTRESOURCE(RT_VERSION) ;
      
      return((void *)(res1));} while(0);
}
void* prim_findResource(HMODULE arg1,char * arg2,void * arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HRSRC res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = FindResource(arg1, arg2, arg3);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("FindResource") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HRSRC)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HRSRC access_prim_findResource_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_findResource_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_findResource_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_findResourceEx(HMODULE arg1,LPCTSTR arg2,void * arg3,WORD arg4)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HRSRC res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = FindResourceEx(arg1, arg2, arg3, arg4);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("FindResourceEx") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HRSRC)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HRSRC access_prim_findResourceEx_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_findResourceEx_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_findResourceEx_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_lR_DEFAULTSIZE()
{ int res1;
  do {res1=LR_DEFAULTSIZE;
      
      return((int)(res1));} while(0);
}
UINT prim_lR_DEFAULTCOLOR()
{ UINT res1;
  do {res1=LR_DEFAULTCOLOR;
      
      return((UINT)(res1));} while(0);
}
UINT prim_lR_CREATEDIBSECTION()
{ UINT res1;
  do {res1=LR_CREATEDIBSECTION;
      
      return((UINT)(res1));} while(0);
}
UINT prim_lR_LOADFROMFILE()
{ UINT res1;
  do {res1=LR_LOADFROMFILE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_lR_LOADMAP3DCOLORS()
{ UINT res1;
  do {res1=LR_LOADMAP3DCOLORS;
      
      return((UINT)(res1));} while(0);
}
UINT prim_lR_LOADTRANSPARENT()
{ UINT res1;
  do {res1=LR_LOADTRANSPARENT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_lR_MONOCHROME()
{ UINT res1;
  do {res1=LR_MONOCHROME;
      
      return((UINT)(res1));} while(0);
}
UINT prim_lR_SHARED()
{ UINT res1;
  do {res1=LR_SHARED;
      
      return((UINT)(res1));} while(0);
}
void* prim_loadImage(HINSTANCE arg1,LPCTSTR arg2,UINT arg3,int arg4,int arg5,UINT arg6)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HANDLE res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = LoadImage(arg1, arg2, arg3, arg4, arg5, arg6);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("LoadImage") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HANDLE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HANDLE access_prim_loadImage_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_loadImage_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_loadImage_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_loadResource(HMODULE arg1,HRSRC arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HGLOBAL res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = LoadResource(arg1, arg2);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("LoadResource") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HGLOBAL)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HGLOBAL access_prim_loadResource_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_loadResource_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_loadResource_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_lockResource(HGLOBAL arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  void * res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = LockResource(arg1);
      if ((gc_failed = ( res1 == NULL ))) {gc_failstring = ErrorWin("LockResource") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (void *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void * access_prim_lockResource_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_lockResource_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_lockResource_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_sizeofResource(HMODULE arg1,HRSRC arg2)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  DWORD res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = SizeofResource(arg1, arg2);
      if ((gc_failed = ( res1 == 0 ))) {gc_failstring = ErrorWin("SizeofResource") ;}
      else {gc_failed = 0;}
      gc_result.res1 = (DWORD)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
DWORD access_prim_sizeofResource_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_sizeofResource_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_sizeofResource_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_updateResource(HANDLE arg1,LPCTSTR arg2,void * arg3,WORD arg4,void * arg5,DWORD arg6)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1 = UpdateResource(arg1,arg2,arg3,arg4,arg5,arg6);
      if ((gc_failed = (  res1 == FALSE  ))) {gc_failstring = ErrorWin("UpdateResource") ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_updateResource_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_updateResource_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
