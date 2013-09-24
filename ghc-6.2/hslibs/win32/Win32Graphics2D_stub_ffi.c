/* Auto generated GreenCard 2 code for FFI */
#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "Win32Graphics2D_stub_ffi.h"
void* prim_moveToEx(HDC arg1,int arg2,int arg3)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { POINT res1;
     BOOL success = MoveToEx(arg1,arg2,arg3,&res1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("MoveToEx")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((res1).x);
      gc_result.gc_res2 = (LONG)((res1).y);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
LONG access_prim_moveToEx_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_moveToEx_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_moveToEx_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_moveToEx_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_lineTo(HDC arg1,int arg2,int arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = LineTo(arg1,arg2,arg3);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("LineTo")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_lineTo_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_lineTo_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_polyline(HDC hdc,POINT * ps,int num_ps)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = Polyline(hdc,ps,num_ps);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("Polyline")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
       free(ps);
      return(&gc_result);} while(0);
}
int access_prim_polyline_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_polyline_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_polylineTo(HDC hdc,POINT * ps,int num_ps)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = PolylineTo(hdc,ps,num_ps);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("PolylineTo")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
       free(ps);
      return(&gc_result);} while(0);
}
int access_prim_polylineTo_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_polylineTo_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_polygon(HDC hdc,POINT * ps,int num_ps)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = Polygon(hdc,ps,num_ps);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("Polygon")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
       free(ps);
      return(&gc_result);} while(0);
}
int access_prim_polygon_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_polygon_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_polyBezier(HDC hdc,POINT * ps,int num_ps)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = PolyBezier(hdc, ps, num_ps);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("PolyBezier")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
       free(ps);
      return(&gc_result);} while(0);
}
int access_prim_polyBezier_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_polyBezier_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_polyBezierTo(HDC hdc,POINT * ps,int num_ps)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = PolyBezierTo(hdc, ps, num_ps);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorString("PolyBezierTo")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
       free(ps);
      return(&gc_result);} while(0);
}
int access_prim_polyBezierTo_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_polyBezierTo_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_fillRect(HDC arg1,LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5,HBRUSH arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;RECT arg2;
  (arg2).left = (LONG)gc_arg2; (arg2).top = (LONG)gc_arg3; (arg2).right = (LONG)gc_arg4; (arg2).bottom = (LONG)gc_arg5;
  do { BOOL success = FillRect(arg1,&arg2,arg3);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("FillRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_fillRect_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_fillRect_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_frameRect(HDC arg1,LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5,HBRUSH arg3)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;RECT arg2;
  (arg2).left = (LONG)gc_arg2; (arg2).top = (LONG)gc_arg3; (arg2).right = (LONG)gc_arg4; (arg2).bottom = (LONG)gc_arg5;
  do { BOOL success = FrameRect(arg1,&arg2,arg3);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("FrameRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_frameRect_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_frameRect_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_invertRect(HDC arg1,LONG gc_arg2,LONG gc_arg3,LONG gc_arg4,LONG gc_arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;RECT arg2;
  (arg2).left = (LONG)gc_arg2; (arg2).top = (LONG)gc_arg3; (arg2).right = (LONG)gc_arg4; (arg2).bottom = (LONG)gc_arg5;
  do { BOOL success = InvertRect(arg1,&arg2);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("InvertRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_invertRect_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_invertRect_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_rectangle(HDC arg1,int arg2,int arg3,int arg4,int arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = Rectangle(arg1,arg2,arg3,arg4,arg5);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("Rectangle")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_rectangle_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_rectangle_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_roundRect(HDC arg1,int arg2,int arg3,int arg4,int arg5,int arg6,int arg7)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = RoundRect(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("RoundRect")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_roundRect_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_roundRect_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_ellipse(HDC arg1,int arg2,int arg3,int arg4,int arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = Ellipse(arg1,arg2,arg3,arg4,arg5);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("Ellipse")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_ellipse_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_ellipse_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_arc(HDC arg1,int arg2,int arg3,int arg4,int arg5,int arg6,int arg7,int arg8,int arg9)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = Arc(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("Arc")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_arc_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_arc_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_arcTo(HDC arg1,int arg2,int arg3,int arg4,int arg5,int arg6,int arg7,int arg8,int arg9)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = ArcTo(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("ArcTo")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_arcTo_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_arcTo_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_angleArc(HDC arg1,int arg2,int arg3,WORD arg4,double arg5,double arg6)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = AngleArc(arg1,arg2,arg3,arg4,arg5,arg6);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("AngleArc")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_angleArc_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_angleArc_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_chord(HDC arg1,int arg2,int arg3,int arg4,int arg5,int arg6,int arg7,int arg8,int arg9)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = Chord(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("Chord")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_chord_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_chord_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_pie(HDC arg1,int arg2,int arg3,int arg4,int arg5,int arg6,int arg7,int arg8,int arg9)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = Pie(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("Pie")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_pie_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_pie_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_bitBlt(HDC arg1,INT arg2,INT arg3,INT arg4,INT arg5,HDC arg6,INT arg7,INT arg8,unsigned int arg9)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = BitBlt(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("BitBlt")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_bitBlt_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bitBlt_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_maskBlt(HDC arg1,INT arg2,INT arg3,INT arg4,INT arg5,HDC arg6,INT arg7,INT arg8,HBITMAP arg9,INT arg10,INT arg11,unsigned int arg12)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = MaskBlt(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11, arg12);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("MaskBlt")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_maskBlt_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_maskBlt_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_stretchBlt(HDC arg1,INT arg2,INT arg3,INT arg4,INT arg5,HDC arg6,INT arg7,INT arg8,INT arg9,INT arg10,unsigned int arg11)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = StretchBlt(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("StretchBlt")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_stretchBlt_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_stretchBlt_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_plgBlt(HDC arg1,LONG gc_arg2,LONG gc_arg3,LONG gc_arg6,LONG gc_arg7,LONG gc_arg9,LONG gc_arg10,HDC arg2,INT x,INT y,INT w,INT h,HBITMAP bm,INT sx,INT sy)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;typedef POINT ThreePts[3]; ThreePts vertices;
  (vertices[0]).x = (LONG)gc_arg2; (vertices[0]).y = (LONG)gc_arg3; (vertices[1]).x = (LONG)gc_arg6; (vertices[1]).y = (LONG)gc_arg7; (vertices[2]).x = (LONG)gc_arg9; (vertices[2]).y = (LONG)gc_arg10;
  do { BOOL success = PlgBlt(arg1,vertices,arg2,x,y,w,h,bm,sx,sy);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("PlgBlt")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
int access_prim_plgBlt_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_plgBlt_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_textOut(HDC arg1,INT arg2,INT arg3,char * arg4,int arg5)
{ static struct {HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { BOOL success = TextOut(arg1,arg2,arg3,arg4,arg5);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("TextOut")  ;}
      else {gc_failed = 0;}
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
        free(arg4);
      return(&gc_result);} while(0);
}
int access_prim_textOut_gc_failed(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_textOut_gc_failstring(void *ptr){ return(((struct {HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
void* prim_getTextExtentPoint32(HDC hdc,char * str,int l)
{ static struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  int gc_failed;
  char* gc_failstring;
  do { SIZE res1;
     BOOL success = GetTextExtentPoint32(hdc,str,l,&res1);
      if ((gc_failed = (  !success  ))) {gc_failstring =  ErrorWin("TextOut")  ;}
      else {gc_failed = 0;}
      gc_result.gc_res1 = (LONG)((res1).cx);
      gc_result.gc_res2 = (LONG)((res1).cy);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
       free(str);
      return(&gc_result);} while(0);
}
LONG access_prim_getTextExtentPoint32_gc_res1(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res1);}
LONG access_prim_getTextExtentPoint32_gc_res2(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_res2);}
int access_prim_getTextExtentPoint32_gc_failed(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_getTextExtentPoint32_gc_failstring(void *ptr){ return(((struct {HsInt32 gc_res1;HsInt32 gc_res2;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
