/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "diatemp.h"
#include "Win32Dialogue_stub_ffi.h"
void * prim_mkResource(int arg1)
{ void * res1;
  do { res1=MAKEINTRESOURCE(arg1);
      
      return((void *)(res1));} while(0);
}
void* prim_dialogBox(HINSTANCE hInst,LPCTSTR lpTemp,HWND hWndParent,void * diaFun)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do { res1 = DialogBox(hInst,lpTemp,hWndParent,diaFun);
      if ((gc_failed = (  res1 == (-1)  ))) {gc_failstring =  ErrorWin("DialogBox")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_dialogBox_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_dialogBox_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_dialogBox_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_dialogBoxParam(HINSTANCE hInst,LPCTSTR lpTemp,HWND hWndParent,void * diaFun,LPARAM dwInit)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do { res1 = DialogBoxParam(hInst,lpTemp,hWndParent,diaFun,dwInit);
      if ((gc_failed = (  res1 == (-1)  ))) {gc_failstring =  ErrorWin("DialogBoxParam")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_dialogBoxParam_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_dialogBoxParam_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_dialogBoxParam_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_dialogBoxIndirect(HINSTANCE hInst,void * lpTemp,HWND hWndParent,void * diaFun)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do { res1 = DialogBoxIndirect(hInst,lpTemp,hWndParent,diaFun);
      if ((gc_failed = (  res1 == (-1)  ))) {gc_failstring =  ErrorWin("DialogBoxIndirect")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_dialogBoxIndirect_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_dialogBoxIndirect_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_dialogBoxIndirect_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_dialogBoxIndirectParam(HINSTANCE hInst,void * lpTemp,HWND hWndParent,void * diaFun,LPARAM dwInit)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do { res1 = DialogBoxIndirectParam(hInst,lpTemp,hWndParent,diaFun,dwInit);
      if ((gc_failed = (  res1 == (-1)  ))) {gc_failstring =  ErrorWin("DialogBoxIndirectParam")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_dialogBoxIndirectParam_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_dialogBoxIndirectParam_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_dialogBoxIndirectParam_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void * prim_getFinalDialog(void * arg1)
{ void * res1;
  do {res1 = getFinalDialog(arg1);
      
      return((void *)(res1));} while(0);
}
void * prim_mkDiaTemplate(int arg1,int arg2,int arg3,int arg4,int arg5,DWORD arg6,DWORD arg7,void * arg8,void * arg9,void * arg10,void * arg11,int arg12)
{ void * res1;
  do {res1 = mkDiaTemplate(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
      
      return((void *)(res1));} while(0);
}
void * prim_addDiaControl(void * arg1,void * arg2,int arg3,void * arg4,DWORD arg5,int arg6,int arg7,int arg8,int arg9,DWORD arg10)
{ void * res1;
  do {res1 = addDiaControl(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
      
      return((void *)(res1));} while(0);
}
void * prim_toUnicodeStr(char * arg1)
{ do { int wlen;
     LPWSTR wstr;
     wlen = MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED,arg1,-1,NULL,0);
     wstr = malloc(sizeof(wchar_t) * wlen);
     MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED,arg1,-1,wstr,wlen);
      
      return((void *)(wstr));} while(0);
}
void* prim_createDialog(HINSTANCE hInst,LPCTSTR lpTemp,HWND hWndParent,void * diaFun)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = CreateDialog(hInst,lpTemp,hWndParent,diaFun);
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring =  ErrorWin("CreateDialog")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createDialog_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createDialog_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createDialog_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createDialogParam(HINSTANCE hInst,LPCTSTR lpTemp,HWND hWndParent,void * diaFun,LPARAM dwInit)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = CreateDialogParam(hInst,lpTemp,hWndParent,diaFun,dwInit);
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring =  ErrorWin("CreateDialogParam")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createDialogParam_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createDialogParam_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createDialogParam_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createDialogIndirect(HINSTANCE hInst,void * lpTemp,HWND hWndParent,void * diaFun)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = CreateDialogIndirect(hInst,lpTemp,hWndParent,diaFun);
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring =  ErrorWin("CreateDialogIndirect")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createDialogIndirect_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createDialogIndirect_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createDialogIndirect_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_createDialogIndirectParam(HINSTANCE hInst,void * lpTemp,HWND hWndParent,void * diaFun,LPARAM dwInit)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = CreateDialogIndirectParam(hInst,lpTemp,hWndParent,diaFun,dwInit);
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring =  ErrorWin("CreateDialogIndirectParam")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_createDialogIndirectParam_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_createDialogIndirectParam_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_createDialogIndirectParam_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
LRESULT prim_defDlgProc(HWND arg1,DWORD arg2,WPARAM arg3,LPARAM arg4)
{ LRESULT res1;
  do { res1 = DefDlgProc(arg1,arg2,arg3,arg4);
      
      return((LRESULT)(res1));} while(0);
}
void* prim_endDialog(HWND arg1,int arg2)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  BOOL res1;int gc_failed;
	    char* gc_failstring;
  do { res1 = EndDialog(arg1,arg2);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorWin("EndDialog")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (BOOL)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
BOOL access_prim_endDialog_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_endDialog_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_endDialog_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
LONG prim_getDialogBaseUnits()
{ LONG res1;
  do {res1 = GetDialogBaseUnits();
      
      return((LONG)(res1));} while(0);
}
void* prim_getDlgCtrlID(HWND arg1)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do {res1 = GetDlgCtrlID(arg1);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorWin("GetDlgCtrlID")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_getDlgCtrlID_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getDlgCtrlID_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getDlgCtrlID_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getDlgItem(HWND arg1,int arg2)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetDlgItem(arg1, arg2);
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring =  ErrorWin("GetDlgItem")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_getDlgItem_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getDlgItem_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getDlgItem_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getDlgItemInt(HWND arg1,int arg2,int arg3)
{ static struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int res1;int gc_failed;
	   char* gc_failstring;
  do { BOOL lpTranslated;
         res1 = GetDlgItemInt(arg1,arg2,&lpTranslated,arg3);
      if ((gc_failed = (  lpTranslated != TRUE  ))) {gc_failstring =  ErrorWin("GetDlgItemInt")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (int)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_getDlgItemInt_res1(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getDlgItemInt_gc_failed(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getDlgItemInt_gc_failstring(void *ptr){ return(((struct {HsInt res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getDlgItemText(HWND arg1,int arg2,int arg3)
{ static struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LPTSTR buf=malloc(sizeof(TCHAR)*arg3); int res1;
         if (buf == NULL) { 
            res1 = 0;
         } else {
            res1 = GetDlgItemText(arg1,arg2,buf,arg3);
        };
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorWin("GetDlgItemInt")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res2 = (char *)(buf);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
char * access_prim_getDlgItemText_gc_res2(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_getDlgItemText_gc_failed(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getDlgItemText_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getNextDlgGroupItem(HWND arg1,HWND arg2,BOOL arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetNextDlgGroupItem(arg1, arg2, arg3);
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring =  ErrorWin("GetNextDlgGroupItem")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_getNextDlgGroupItem_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getNextDlgGroupItem_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getNextDlgGroupItem_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getNextDlgTabItem(HWND arg1,HWND arg2,BOOL arg3)
{ static struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  HWND res1;int gc_failed;
	    char* gc_failstring;
  do {res1 = GetNextDlgTabItem(arg1, arg2, arg3);
      if ((gc_failed = (  res1 == NULL  ))) {gc_failstring =  ErrorWin("GetNextDlgTabItem")  ;}
      else {gc_failed = 0;}
      gc_result.res1 = (HWND)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
HWND access_prim_getNextDlgTabItem_res1(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->res1);}
int access_prim_getNextDlgTabItem_gc_failed(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getNextDlgTabItem_gc_failstring(void *ptr){ return(((struct {HsPtr res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
BOOL prim_isDialogMessage(HWND arg1,MSG * arg2)
{ BOOL res1;
  do { res1=IsDialogMessage(arg1,arg2);
      
      return((BOOL)(res1));} while(0);
}
void* prim_mapDialogRect(HWND arg1,LPRECT arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1; 
     res1 = MapDialogRect(arg1,arg2);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorWin("MapDialogRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_mapDialogRect_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_mapDialogRect_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
LONG prim_sendDlgItemMessage(HWND arg1,int arg2,DWORD arg3,WPARAM arg4,LPARAM arg5)
{ LONG res1;
  do { res1 = SendDlgItemMessage(arg1,arg2,arg3,arg4,arg5);
      
      return((LONG)(res1));} while(0);
}
void* prim_setDlgItemInt(HWND arg1,int arg2,UINT arg3,BOOL arg4)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1;
     res1=SetDlgItemInt(arg1,arg2,arg3,arg4);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorWin("SetDlgItemInt")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_setDlgItemInt_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setDlgItemInt_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_setDlgItemText(HWND arg1,int arg2,char * arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL res1;
         res1 = SetDlgItemText(arg1,arg2,arg3);
      if ((gc_failed = (  res1 == 0  ))) {gc_failstring =  ErrorWin("SetDlgItemText")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_setDlgItemText_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_setDlgItemText_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_dS_3DLOOK()
{ DWORD res1;
  do {res1=DS_3DLOOK;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_ABSALIGN()
{ DWORD res1;
  do {res1=DS_ABSALIGN;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_CENTER()
{ DWORD res1;
  do {res1=DS_CENTER;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_CENTERMOUSE()
{ DWORD res1;
  do {res1=DS_CENTERMOUSE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_CONTEXTHELP()
{ DWORD res1;
  do {res1=DS_CONTEXTHELP;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_CONTROL()
{ DWORD res1;
  do {res1=DS_CONTROL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_FIXEDSYS()
{ DWORD res1;
  do {res1=DS_FIXEDSYS;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_LOCALEDIT()
{ DWORD res1;
  do {res1=DS_LOCALEDIT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_MODALFRAME()
{ DWORD res1;
  do {res1=DS_MODALFRAME;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_NOFAILCREATE()
{ DWORD res1;
  do {res1=DS_NOFAILCREATE;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_NOIDLEMSG()
{ DWORD res1;
  do {res1=DS_NOIDLEMSG;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_SETFONT()
{ DWORD res1;
  do {res1=DS_SETFONT;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_SETFOREGROUND()
{ DWORD res1;
  do {res1=DS_SETFOREGROUND;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dS_SYSMODAL()
{ DWORD res1;
  do {res1=DS_SYSMODAL;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dM_GETDEFID()
{ DWORD res1;
  do {res1=DM_GETDEFID;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dM_REPOSITION()
{ DWORD res1;
  do {res1=DM_REPOSITION;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_dM_SETDEFID()
{ DWORD res1;
  do {res1=DM_SETDEFID;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wM_CTLCOLORDLG()
{ DWORD res1;
  do {res1=WM_CTLCOLORDLG;
      
      return((DWORD)(res1));} while(0);
}
DWORD prim_wM_CTLCOLORMSGBOX()
{ DWORD res1;
  do {res1=WM_CTLCOLORMSGBOX;
      
      return((DWORD)(res1));} while(0);
}
