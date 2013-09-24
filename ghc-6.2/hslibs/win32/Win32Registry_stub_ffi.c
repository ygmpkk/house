/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "finalizers.h"
#include "Win32Registry_stub_ffi.h"
void* prim_hKEY_CLASSES_ROOT()
{ static struct {HsPtr gc_res3;HsPtr gc_res1;} gc_result;
  HKEY res1;
  do {res1=HKEY_CLASSES_ROOT;
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(res1);
      
      return(&gc_result);} while(0);
}
void* access_prim_hKEY_CLASSES_ROOT_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res3);}
HKEY access_prim_hKEY_CLASSES_ROOT_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res1);}
void* prim_hKEY_CURRENT_CONFIG()
{ static struct {HsPtr gc_res3;HsPtr gc_res1;} gc_result;
  HKEY res1;
  do {res1=HKEY_CURRENT_CONFIG;
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(res1);
      
      return(&gc_result);} while(0);
}
void* access_prim_hKEY_CURRENT_CONFIG_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res3);}
HKEY access_prim_hKEY_CURRENT_CONFIG_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res1);}
void* prim_hKEY_CURRENT_USER()
{ static struct {HsPtr gc_res3;HsPtr gc_res1;} gc_result;
  HKEY res1;
  do {res1=HKEY_CURRENT_USER;
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(res1);
      
      return(&gc_result);} while(0);
}
void* access_prim_hKEY_CURRENT_USER_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res3);}
HKEY access_prim_hKEY_CURRENT_USER_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res1);}
void* prim_hKEY_LOCAL_MACHINE()
{ static struct {HsPtr gc_res3;HsPtr gc_res1;} gc_result;
  HKEY res1;
  do {res1=HKEY_LOCAL_MACHINE;
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(res1);
      
      return(&gc_result);} while(0);
}
void* access_prim_hKEY_LOCAL_MACHINE_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res3);}
HKEY access_prim_hKEY_LOCAL_MACHINE_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res1);}
void* prim_hKEY_USERS()
{ static struct {HsPtr gc_res3;HsPtr gc_res1;} gc_result;
  HKEY res1;
  do {res1=HKEY_USERS;
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(res1);
      
      return(&gc_result);} while(0);
}
void* access_prim_hKEY_USERS_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res3);}
HKEY access_prim_hKEY_USERS_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;}*) ptr)->gc_res1);}
void* prim_regCloseKey(HKEY arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r = RegCloseKey(arg1);
      if ((gc_failed = ( r != ERROR_SUCCESS ))) {gc_failstring = ErrorWithCode("RegCloseKey",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regCloseKey_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regCloseKey_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regConnectRegistry(char * arg1,HKEY arg2)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { HKEY hk;
     LONG r;
     r = RegConnectRegistry((LPTSTR)arg1,
                            (HKEY)arg2,
                            &hk);
      if ((gc_failed = (  r != ERROR_SUCCESS ))) {gc_failstring = ErrorWithCode("RegConnectRegistry",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(hk);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_regConnectRegistry_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HKEY access_prim_regConnectRegistry_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_regConnectRegistry_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regConnectRegistry_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regCreateKey(HKEY arg1,char * arg2)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { HKEY res;
     LONG r;
     r = RegCreateKey((HKEY)arg1,(LPCTSTR)arg2,&res);
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring = ErrorWithCode("RegCreateKey",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(res);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_regCreateKey_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HKEY access_prim_regCreateKey_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_regCreateKey_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regCreateKey_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_rEG_OPTION_NON_VOLATILE()
{ int res1;
  do {res1=REG_OPTION_NON_VOLATILE;
      
      return((int)(res1));} while(0);
}
int prim_rEG_OPTION_VOLATILE()
{ int res1;
  do {res1=REG_OPTION_VOLATILE;
      
      return((int)(res1));} while(0);
}
int prim_kEY_ALL_ACCESS()
{ int res1;
  do {res1=KEY_ALL_ACCESS;
      
      return((int)(res1));} while(0);
}
int prim_kEY_CREATE_LINK()
{ int res1;
  do {res1=KEY_CREATE_LINK;
      
      return((int)(res1));} while(0);
}
int prim_kEY_CREATE_SUB_KEY()
{ int res1;
  do {res1=KEY_CREATE_SUB_KEY;
      
      return((int)(res1));} while(0);
}
int prim_kEY_ENUMERATE_SUB_KEYS()
{ int res1;
  do {res1=KEY_ENUMERATE_SUB_KEYS;
      
      return((int)(res1));} while(0);
}
int prim_kEY_EXECUTE()
{ int res1;
  do {res1=KEY_EXECUTE;
      
      return((int)(res1));} while(0);
}
int prim_kEY_NOTIFY()
{ int res1;
  do {res1=KEY_NOTIFY;
      
      return((int)(res1));} while(0);
}
int prim_kEY_QUERY_VALUE()
{ int res1;
  do {res1=KEY_QUERY_VALUE;
      
      return((int)(res1));} while(0);
}
int prim_kEY_READ()
{ int res1;
  do {res1=KEY_READ;
      
      return((int)(res1));} while(0);
}
int prim_kEY_SET_VALUE()
{ int res1;
  do {res1=KEY_SET_VALUE;
      
      return((int)(res1));} while(0);
}
int prim_kEY_WRITE()
{ int res1;
  do {res1=KEY_WRITE;
      
      return((int)(res1));} while(0);
}
void* prim_regCreateKeyEx(HKEY arg1,char * arg2,char * arg3,int arg4,int arg5,void * arg6)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_res5;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { HKEY hkey;
     DWORD disp;
     LONG r;
     r = RegCreateKeyEx((HKEY)arg1,(LPCTSTR)arg2,(DWORD)0,
                        (LPTSTR)arg3,(DWORD)arg4,(REGSAM)arg5,
                        (LPSECURITY_ATTRIBUTES)arg6,&hkey,&disp);
      if ((gc_failed = ( r != ERROR_SUCCESS ))) {gc_failstring = ErrorWithCode("RegCreateKeyEx",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(hkey);
      gc_result.gc_res5 = (int)(disp == REG_CREATED_NEW_KEY);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_regCreateKeyEx_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_res5;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HKEY access_prim_regCreateKeyEx_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_res5;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_regCreateKeyEx_gc_res5(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_res5;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res5);}
int access_prim_regCreateKeyEx_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_res5;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regCreateKeyEx_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_res5;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regDeleteKey(HKEY arg1,char * arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r;
     r = RegDeleteKey((HKEY)arg1,(LPCTSTR)arg2);
      if ((gc_failed = ( r != ERROR_SUCCESS ))) {gc_failstring = ErrorWithCode("RegDeleteKey",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regDeleteKey_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regDeleteKey_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regDeleteValue(HKEY arg1,char * arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r;
     r = RegDeleteValue((HKEY)arg1,(LPCTSTR)arg2);
      if ((gc_failed = ( r != ERROR_SUCCESS ))) {gc_failstring = ErrorWithCode("RegDeleteValue",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regDeleteValue_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regDeleteValue_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
DWORD prim_readWord32Addr(char* s,int i)
{ DWORD res1;
  do { res1 = s[i]; ;
      
      return((DWORD)(res1));} while(0);
}
void* prim_regEnumKey(HKEY arg1,DWORD arg2,void * arg3,DWORD arg4)
{ static struct {HsPtr gc_res2;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG res;
     res = RegEnumKey((HKEY)arg1,(DWORD)arg2,(LPTSTR)arg3,(DWORD)arg4);
      if ((gc_failed = (  res != ERROR_SUCCESS && res != ERROR_NO_MORE_ITEMS  ))) {gc_failstring =  ErrorWithCode("RegEnumKey",res)  ;}
      else {gc_failed = 0;}
      gc_result.gc_res2 = (char *)(arg3);
      gc_result.gc_res3 = (int)(res == ERROR_NO_MORE_ITEMS);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
char * access_prim_regEnumKey_gc_res2(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_regEnumKey_gc_res3(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
int access_prim_regEnumKey_gc_failed(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regEnumKey_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regEnumValue(HKEY arg1,DWORD arg2,void * arg3,DWORD arg4,void * arg5,DWORD arg6)
{ static struct {HsInt32 gc_res1;HsPtr gc_res3;HsInt gc_res4;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG res=0; DWORD regTy;
     res = RegEnumValue((HKEY)arg1,(DWORD)arg2,(LPSTR)arg3,(LPDWORD)&arg4,
                        NULL, &regTy, (LPBYTE)arg5, (LPDWORD)&arg6);
      if ((gc_failed = (  res != ERROR_SUCCESS && res != ERROR_NO_MORE_ITEMS  ))) {gc_failstring =  ErrorWithCode("RegEnumValue",res)  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (int)(regTy);
      gc_result.gc_res3 = (char *)(arg3);
      gc_result.gc_res4 = (int)(res == ERROR_NO_MORE_ITEMS);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regEnumValue_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsPtr gc_res3;HsInt gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
char * access_prim_regEnumValue_gc_res3(void *ptr){ return(((struct {HsInt32 gc_res1;HsPtr gc_res3;HsInt gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
int access_prim_regEnumValue_gc_res4(void *ptr){ return(((struct {HsInt32 gc_res1;HsPtr gc_res3;HsInt gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res4);}
int access_prim_regEnumValue_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsPtr gc_res3;HsInt gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regEnumValue_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsPtr gc_res3;HsInt gc_res4;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regFlushKey(HKEY arg1)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r = RegFlushKey((HKEY)arg1);
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegFlushKey",r)  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regFlushKey_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regFlushKey_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regLoadKey(HKEY arg1,char * arg2,char * arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r;
     r = RegLoadKey((HKEY)arg1,(LPCTSTR)arg2,(LPCTSTR)arg3);
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring = ErrorWithCode("RegLoadKey",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regLoadKey_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regLoadKey_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_rEG_NOTIFY_CHANGE_NAME()
{ int res1;
  do {res1=REG_NOTIFY_CHANGE_NAME;
      
      return((int)(res1));} while(0);
}
int prim_rEG_NOTIFY_CHANGE_ATTRIBUTES()
{ int res1;
  do {res1=REG_NOTIFY_CHANGE_ATTRIBUTES;
      
      return((int)(res1));} while(0);
}
int prim_rEG_NOTIFY_CHANGE_LAST_SET()
{ int res1;
  do {res1=REG_NOTIFY_CHANGE_LAST_SET;
      
      return((int)(res1));} while(0);
}
int prim_rEG_NOTIFY_CHANGE_SECURITY()
{ int res1;
  do {res1=REG_NOTIFY_CHANGE_SECURITY;
      
      return((int)(res1));} while(0);
}
void* prim_regNotifyChangeKeyValue(HKEY arg1,int arg2,int arg3,HANDLE arg4,int arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r;
     r = RegNotifyChangeKeyValue((HKEY)arg1,  (BOOL)arg2,
                                 (DWORD)arg3, (HANDLE)arg4,
                                 (BOOL)arg5);
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegNotifyChangeKeyValue",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regNotifyChangeKeyValue_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regNotifyChangeKeyValue_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regOpenKey(HKEY arg1,char * arg2)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { HKEY ret;
     LONG r;
     r=RegOpenKey((HKEY)arg1,(LPCTSTR)arg2,&ret);
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring = ErrorWithCode("RegOpenKey",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(ret);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_regOpenKey_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HKEY access_prim_regOpenKey_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_regOpenKey_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regOpenKey_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regOpenKeyEx(HKEY arg1,char * arg2,int arg3)
{ static struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { HKEY ret;
     LONG r;
     r=RegOpenKeyEx((HKEY)arg1,(LPCTSTR)arg2,(DWORD)0,
                    (REGSAM)arg3,&ret);
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegOpenKeyEx",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_res3 = &deleteObj;
      gc_result.gc_res1 = (HKEY)(ret);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
void* access_prim_regOpenKeyEx_gc_res3(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
HKEY access_prim_regOpenKeyEx_gc_res1(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_regOpenKeyEx_gc_failed(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regOpenKeyEx_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res3;HsPtr gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regQueryInfoKey(HKEY arg1)
{ static struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  char * str; int cbClass; DWORD cSubKeys; DWORD cbMaxSubKeyLen; DWORD cbMaxClassLen; DWORD cValues; DWORD cbMaxValueNameLen; DWORD cbMaxValueLen; int cbSecurityDescriptor;int gc_failed;
																					    char* gc_failstring;
  do { LONG r;
     char* str;
     DWORD cbClass;
     DWORD cSubKeys;
     DWORD cbMaxSubKeyLen;
     DWORD cbMaxClassLen;
     DWORD cValues;
     DWORD cbMaxValueNameLen;
     DWORD cbMaxValueLen;
     DWORD cbSecurityDescriptor;
     FILETIME ftLastWriteTime;

     cbClass = 100;
     str=(char*)malloc(sizeof(char)*cbClass);
     
     r=RegQueryInfoKey((HKEY)arg1, str, &cbClass, NULL,
                       &cSubKeys, &cbMaxSubKeyLen, &cbMaxClassLen,
			&cValues, &cbMaxValueNameLen, &cbMaxValueLen,
                       &cbSecurityDescriptor, &ftLastWriteTime
                      );
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegQueryInfoKey",r)  ;}
      else {gc_failed = 0;}
      gc_result.str = (char *)(str);
      gc_result.cbClass = (int)(cbClass);
      gc_result.cSubKeys = (DWORD)(cSubKeys);
      gc_result.cbMaxSubKeyLen = (DWORD)(cbMaxSubKeyLen);
      gc_result.cbMaxClassLen = (DWORD)(cbMaxClassLen);
      gc_result.cValues = (DWORD)(cValues);
      gc_result.cbMaxValueNameLen = (DWORD)(cbMaxValueNameLen);
      gc_result.cbMaxValueLen = (DWORD)(cbMaxValueLen);
      gc_result.cbSecurityDescriptor = (int)(cbSecurityDescriptor);
      gc_result.gc_res2 = (DWORD)(ftLastWriteTime.dwLowDateTime);
      gc_result.gc_res3 = (DWORD)(ftLastWriteTime.dwHighDateTime);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
char * access_prim_regQueryInfoKey_str(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->str);}
int access_prim_regQueryInfoKey_cbClass(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->cbClass);}
DWORD access_prim_regQueryInfoKey_cSubKeys(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->cSubKeys);}
DWORD access_prim_regQueryInfoKey_cbMaxSubKeyLen(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->cbMaxSubKeyLen);}
DWORD access_prim_regQueryInfoKey_cbMaxClassLen(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->cbMaxClassLen);}
DWORD access_prim_regQueryInfoKey_cValues(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->cValues);}
DWORD access_prim_regQueryInfoKey_cbMaxValueNameLen(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->cbMaxValueNameLen);}
DWORD access_prim_regQueryInfoKey_cbMaxValueLen(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->cbMaxValueLen);}
int access_prim_regQueryInfoKey_cbSecurityDescriptor(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->cbSecurityDescriptor);}
DWORD access_prim_regQueryInfoKey_gc_res2(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
DWORD access_prim_regQueryInfoKey_gc_res3(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res3);}
int access_prim_regQueryInfoKey_gc_failed(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regQueryInfoKey_gc_failstring(void *ptr){ return(((struct {HsPtr str;HsInt cbClass;HsWord32 cSubKeys;HsWord32 cbMaxSubKeyLen;HsWord32 cbMaxClassLen;HsWord32 cValues;HsWord32 cbMaxValueNameLen;HsWord32 cbMaxValueLen;HsInt cbSecurityDescriptor;HsWord32 gc_res2;HsWord32 gc_res3;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regQueryValueKey(HKEY arg1,char * arg2)
{ static struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { char* szValue; long valueLen;
     long res;
     res = RegQueryValue ((HKEY)arg1,(char*)arg2, NULL, &valueLen);
     if ( res == ERROR_SUCCESS ) {
        szValue=(LPTSTR)malloc(sizeof(char)*(valueLen + 1));
        res = RegQueryValue((HKEY)arg1,(char*)arg2, szValue, &valueLen);
     };
      if ((gc_failed = (  res != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegQueryValue",res)  ;}
      else {gc_failed = 0;}
      gc_result.gc_res2 = (char *)(szValue);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
char * access_prim_regQueryValueKey_gc_res2(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_regQueryValueKey_gc_failed(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regQueryValueKey_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regQueryValue(HKEY arg1,char * arg2)
{ static struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { char* res1; DWORD valueLen;
     long res; DWORD ty;
     res = RegQueryValueEx ((HKEY)arg1,(char*)arg2, NULL, &ty, NULL, &valueLen);
     if ( ty== REG_SZ && res == ERROR_SUCCESS ) {
        res1=(LPTSTR)malloc(sizeof(char)*(valueLen + 1));
        res = RegQueryValueEx((HKEY)arg1,(char*)arg2, NULL, &ty, res1, &valueLen);
     } else {
        if ( ty != REG_SZ ) res = 1;
     };
      if ((gc_failed = (  res != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegQueryValue",res)  ;}
      else {gc_failed = 0;}
      gc_result.gc_res2 = (char *)(res1);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
char * access_prim_regQueryValue_gc_res2(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_regQueryValue_gc_failed(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regQueryValue_gc_failstring(void *ptr){ return(((struct {HsPtr gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regQueryValueEx(HKEY arg1,char * arg2,void * arg3,int arg4)
{ static struct {HsInt32 gc_res1;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG res;
     DWORD ty;
     
     res = RegQueryValueEx ((HKEY)arg1, (LPTSTR)arg2,
                            NULL, &ty, (LPBYTE)arg3, (LPDWORD)arg4);
      if ((gc_failed = (  res != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegQueryValueEx",res)  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (int)(res);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regQueryValueEx_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
int access_prim_regQueryValueEx_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regQueryValueEx_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regReplaceKey(HKEY arg1,char * arg2,char * arg3,char * arg4)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r;
     r= RegReplaceKey((HKEY)arg1,(LPCTSTR)arg2,
                      (LPCTSTR)arg3,(LPCTSTR)arg4);
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegReplaceKey",r)  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regReplaceKey_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regReplaceKey_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_rEG_WHOLE_HIVE_VOLATILE()
{ int res1;
  do {res1=REG_WHOLE_HIVE_VOLATILE;
      
      return((int)(res1));} while(0);
}
int prim_rEG_REFRESH_HIVE()
{ int res1;
  do {res1=REG_REFRESH_HIVE;
      
      return((int)(res1));} while(0);
}
int prim_rEG_NO_LAZY_FLUSH()
{ int res1;
  do {res1=REG_NO_LAZY_FLUSH;
      
      return((int)(res1));} while(0);
}
void* prim_regRestoreKey(HKEY arg1,char * arg2,int arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r;
     r = RegRestoreKey((HKEY)arg1,(LPCTSTR)arg2,
                       (DWORD)arg3);
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegRestoreKey",r)  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regRestoreKey_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regRestoreKey_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regSaveKey(HKEY arg1,char * arg2,void * arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r;
     r = RegSaveKey((HKEY)arg1,(LPCTSTR)arg2,
                    (LPSECURITY_ATTRIBUTES)arg3);
      if ((gc_failed = (  r != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegSaveKey",r)  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regSaveKey_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regSaveKey_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regSetValue(HKEY arg1,char * arg2,char * arg3,int arg4)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG res = RegSetValue(arg1,arg2,REG_SZ,arg3,arg4);
      if ((gc_failed = (  res != ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegSetValue",res)  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regSetValue_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regSetValue_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
int prim_rEG_BINARY()
{ int res1;
  do {res1=REG_BINARY;
      
      return((int)(res1));} while(0);
}
int prim_rEG_DWORD()
{ int res1;
  do {res1=REG_DWORD;
      
      return((int)(res1));} while(0);
}
int prim_rEG_DWORD_LITTLE_ENDIAN()
{ int res1;
  do {res1=REG_DWORD_LITTLE_ENDIAN;
      
      return((int)(res1));} while(0);
}
int prim_rEG_DWORD_BIG_ENDIAN()
{ int res1;
  do {res1=REG_DWORD_BIG_ENDIAN;
      
      return((int)(res1));} while(0);
}
int prim_rEG_EXPAND_SZ()
{ int res1;
  do {res1=REG_EXPAND_SZ;
      
      return((int)(res1));} while(0);
}
int prim_rEG_LINK()
{ int res1;
  do {res1=REG_LINK;
      
      return((int)(res1));} while(0);
}
int prim_rEG_MULTI_SZ()
{ int res1;
  do {res1=REG_MULTI_SZ;
      
      return((int)(res1));} while(0);
}
int prim_rEG_NONE()
{ int res1;
  do {res1=REG_NONE;
      
      return((int)(res1));} while(0);
}
int prim_rEG_RESOURCE_LIST()
{ int res1;
  do {res1=REG_RESOURCE_LIST;
      
      return((int)(res1));} while(0);
}
int prim_rEG_SZ()
{ int res1;
  do {res1=REG_SZ;
      
      return((int)(res1));} while(0);
}
void* prim_regSetValueEx(HKEY arg1,char * arg2,int arg3,void * arg4,int arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r;
     r = RegSetValueEx((HKEY)arg1,(LPCTSTR)arg2,
                       0,arg3,arg4,arg5);
      if ((gc_failed = (  r!=ERROR_SUCCESS  ))) {gc_failstring =  ErrorWithCode("RegSetValueEx",r)  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regSetValueEx_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regSetValueEx_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_regUnLoadKey(HKEY arg1,char * arg2)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { LONG r;
     r = RegUnLoadKey((HKEY)arg1,(LPCTSTR)arg2);
      if ((gc_failed = ( r != ERROR_SUCCESS ))) {gc_failstring = ErrorWithCode("RegUnLoadKey",r) ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_regUnLoadKey_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_regUnLoadKey_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
