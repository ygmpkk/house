/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "gettime.h"
#include <math.h>
#include "Win32Misc_stub_ffi.h"
void* prim_loadAccelerators(HINSTANCE arg1,LPCSTR arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HACCEL res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = LoadAccelerators(arg1, arg2);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("LoadAccelerators")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HACCEL)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HACCEL access_prim_loadAccelerators_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_loadAccelerators_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_loadAccelerators_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_loadCursor(HINSTANCE arg1,LPSTR arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HCURSOR res1;int gc_failed;
	       char* gc_failstring;
  do {res1 = LoadCursor(arg1, arg2);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("LoadCursor")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HCURSOR)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HCURSOR access_prim_loadCursor_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_loadCursor_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_loadCursor_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_loadIcon(HINSTANCE arg1,LPSTR arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HICON res1;int gc_failed;
	     char* gc_failstring;
  do {res1 = LoadIcon(arg1, arg2);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("LoadIcon")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HICON)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HICON access_prim_loadIcon_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_loadIcon_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_loadIcon_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
LPSTR prim_iDC_ARROW()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDC_ARROW) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDC_IBEAM()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDC_IBEAM) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDC_WAIT()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDC_WAIT) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDC_CROSS()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDC_CROSS) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDC_UPARROW()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDC_UPARROW) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDC_SIZENWSE()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDC_SIZENWSE) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDC_SIZENESW()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDC_SIZENESW) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDC_SIZEWE()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDC_SIZEWE) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDC_SIZENS()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDC_SIZENS) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDI_APPLICATION()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDI_APPLICATION) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDI_HAND()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDI_HAND) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDI_QUESTION()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDI_QUESTION) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDI_EXCLAMATION()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDI_EXCLAMATION) ;
      
      return((LPSTR)(res1));} while(0);
}
LPSTR prim_iDI_ASTERISK()
{ LPSTR res1;
  do {res1= MAKEINTRESOURCE(IDI_ASTERISK) ;
      
      return((LPSTR)(res1));} while(0);
}
UINT prim_mB_OK()
{ UINT res1;
  do {res1=MB_OK;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_OKCANCEL()
{ UINT res1;
  do {res1=MB_OKCANCEL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_ABORTRETRYIGNORE()
{ UINT res1;
  do {res1=MB_ABORTRETRYIGNORE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_YESNOCANCEL()
{ UINT res1;
  do {res1=MB_YESNOCANCEL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_YESNO()
{ UINT res1;
  do {res1=MB_YESNO;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_RETRYCANCEL()
{ UINT res1;
  do {res1=MB_RETRYCANCEL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_ICONHAND()
{ UINT res1;
  do {res1=MB_ICONHAND;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_ICONQUESTION()
{ UINT res1;
  do {res1=MB_ICONQUESTION;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_ICONEXCLAMATION()
{ UINT res1;
  do {res1=MB_ICONEXCLAMATION;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_ICONASTERISK()
{ UINT res1;
  do {res1=MB_ICONASTERISK;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_ICONINFORMATION()
{ UINT res1;
  do {res1=MB_ICONINFORMATION;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_ICONSTOP()
{ UINT res1;
  do {res1=MB_ICONSTOP;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_DEFBUTTON1()
{ UINT res1;
  do {res1=MB_DEFBUTTON1;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_DEFBUTTON2()
{ UINT res1;
  do {res1=MB_DEFBUTTON2;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_DEFBUTTON3()
{ UINT res1;
  do {res1=MB_DEFBUTTON3;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_APPLMODAL()
{ UINT res1;
  do {res1=MB_APPLMODAL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_SYSTEMMODAL()
{ UINT res1;
  do {res1=MB_SYSTEMMODAL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_TASKMODAL()
{ UINT res1;
  do {res1=MB_TASKMODAL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_mB_SETFOREGROUND()
{ UINT res1;
  do {res1=MB_SETFOREGROUND;
      
      return((UINT)(res1));} while(0);
}
UINT prim_iDABORT()
{ UINT res1;
  do {res1=IDABORT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_iDCANCEL()
{ UINT res1;
  do {res1=IDCANCEL;
      
      return((UINT)(res1));} while(0);
}
UINT prim_iDIGNORE()
{ UINT res1;
  do {res1=IDIGNORE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_iDNO()
{ UINT res1;
  do {res1=IDNO;
      
      return((UINT)(res1));} while(0);
}
UINT prim_iDOK()
{ UINT res1;
  do {res1=IDOK;
      
      return((UINT)(res1));} while(0);
}
UINT prim_iDRETRY()
{ UINT res1;
  do {res1=IDRETRY;
      
      return((UINT)(res1));} while(0);
}
UINT prim_iDYES()
{ UINT res1;
  do {res1=IDYES;
      
      return((UINT)(res1));} while(0);
}
void* prim_messageBox(HWND arg1,char * arg2,char * arg3,UINT arg4)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = MessageBox(arg1, arg2, arg3, arg4);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorString("MessageBox")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_messageBox_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_messageBox_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_messageBox_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_sTD_INPUT_HANDLE()
{ DWORD res1;
  do {res1=STD_INPUT_HANDLE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sTD_OUTPUT_HANDLE()
{ DWORD res1;
  do {res1=STD_OUTPUT_HANDLE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_sTD_ERROR_HANDLE()
{ DWORD res1;
  do {res1=STD_ERROR_HANDLE;
      
      return((DWORD)(res1));} while(0);
}
void* prim_getStdHandle(DWORD arg1)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HANDLE res1;int gc_failed;
	      char* gc_failstring;
  do {res1 = GetStdHandle(arg1);
      if ((gc_failed = (  res1 == INVALID_HANDLE_VALUE  ))) {gc_failstring =  ErrorWin("GetStdHandle")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HANDLE)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HANDLE access_prim_getStdHandle_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getStdHandle_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getStdHandle_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_transformedEllipse(HDC hdc,LONG gc_arg2,LONG gc_arg3,LONG gc_arg6,LONG gc_arg7,LONG gc_arg10,LONG gc_arg11)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;POINT p0; POINT p1; POINT p2;
  p0.x = (LONG)gc_arg2; p0.y = (LONG)gc_arg3; p1.x = (LONG)gc_arg6; p1.y = (LONG)gc_arg7; p2.x = (LONG)gc_arg10; p2.y = (LONG)gc_arg11;
  do {
  static BOOL firstTime = 1;
  static double sins[20]; 
  static double coss[20];

  int   i;
  POINT pts[20];

  double x = (p1.x + p2.x) / 2;  /* centre of parallelogram */
  double y = (p1.y + p2.y) / 2;

  double dx1 = (p1.x - p0.x) / 2; /* distance to corners from centre */
  double dy1 = (p1.y - p0.y) / 2;
  double dx2 = (p2.x - p0.x) / 2;
  double dy2 = (p2.y - p0.y) / 2;

  BOOL  errcode;

  if (firstTime) {
    double a  = 0.0;
    double da = 2.0*3.14159 / 20;
    for (i=0; i < 20; ++i, a+=da) {
	sins[i] = sin(a);
	coss[i] = cos(a);
    }
    firstTime = 0;
  }
  for(i=0; i < 20; ++i) {
    double c = coss[i];
    double s = sins[i];
    pts[i].x = x + c*dx1 + s*dx2;
    pts[i].y = y + c*dy1 + s*dy2;
  }
  errcode = Polygon(hdc,pts,20);
      if ((gc_failed = ( !errcode ))) {gc_failstring =  ErrorString("TransformedEllipse")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_transformedEllipse_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_transformedEllipse_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
UINT prim_eWX_FORCE()
{ UINT res1;
  do {res1=EWX_FORCE;
      
      return((UINT)(res1));} while(0);
}
UINT prim_eWX_LOGOFF()
{ UINT res1;
  do {res1=EWX_LOGOFF;
      
      return((UINT)(res1));} while(0);
}
UINT prim_eWX_POWEROFF()
{ UINT res1;
  do {res1=EWX_POWEROFF;
      
      return((UINT)(res1));} while(0);
}
UINT prim_eWX_REBOOT()
{ UINT res1;
  do {res1=EWX_REBOOT;
      
      return((UINT)(res1));} while(0);
}
UINT prim_eWX_SHUTDOWN()
{ UINT res1;
  do {res1=EWX_SHUTDOWN;
      
      return((UINT)(res1));} while(0);
}
void prim_messageBeep(UINT arg1)
{ do {MessageBeep(arg1);
      ;} while(0);
}
void* prim_beep(WORD arg1,int arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = Beep(arg1, arg2);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("Beep")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_beep_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_beep_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setWinTimer(HWND arg1,UINT arg2,UINT arg3)
{ static struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  UINT res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = SetTimer(arg1, arg2, arg3, NULL);
      if ((gc_failed = ( res1 == 0 ))) {gc_failstring =  ErrorString("setWinTimer")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (UINT)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
UINT access_prim_setWinTimer_res1(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_setWinTimer_gc_failed(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setWinTimer_gc_failstring(void *ptr){ return(((struct {HsWord32 res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_killTimer(HWND arg1,UINT arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = KillTimer(arg1,arg2);
      if ((gc_failed = ( !success ))) {gc_failstring =  ErrorWin("KillTimer")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_killTimer_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_killTimer_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_timeGetTime()
{ DWORD res1;
  do {res1 = timeGetTime();
      
      return((DWORD)(res1));} while(0);
}
