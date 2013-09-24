#include "util_121.h"
#include "intrface_121.h"
#include <math.h>


/* PA: these prototypes have no implementation
void InitPicture( HDC hdc );
void DonePicture( HDC hdc );
*/

/* PA: PointsToPix is local to cpicture_121.c
static int PointsToPix(HDC hdc, int size);
*/

extern HDC WinGetDC (int);
extern void WinReleaseDC (int,HDC);
extern int WinGetVertResolution (void);
extern int WinGetHorzResolution (void);

extern void WinInitPicture (int,int,int,int,int,int,int,int,int,int,char *,int,int,int,int,HDC);
extern void WinDonePicture (HDC,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,char **,int*,int*);

extern void WinClipPicture (int,int,int,int,HDC);
extern void WinClipRgnPicture(HRGN,HDC);
extern void WinSetClipRgnPicture (HRGN,HDC);
extern HRGN WinGetClipRgnPicture (HDC);

/*	Operations to create, modify, and destroy polygon shapes.
*/
extern POINT *WinAllocPolyShape (int);
extern void   WinSetPolyPoint (int,int,int,POINT*);
extern void   WinFreePolyShape (POINT*);

/*	Operations to create, modify and destroy regions.
*/
extern HRGN WinCreateRectRgn (int,int,int,int);
extern HRGN WinCreatePolygonRgn (POINT*,int,int);
extern HRGN WinSetRgnToRect (int,int,int,int,HRGN);
extern HRGN WinCombineRgn (HRGN,HRGN,HRGN,int);
extern void WinGetRgnBox (HRGN,int*,int*,int*,int*,BOOL*,BOOL*);
extern void WinDeleteObject (HGDIOBJ);

extern void WinSetPenSize (int,HDC);
extern void WinSetPenColor (int,int,int,HDC);
extern void WinSetBackColor (int,int,int,HDC);
extern void WinSetMode (int,HDC);
extern void WinSetPattern (int,HDC);

extern void WinGetPenPos( HDC, int*, int*);
extern void WinMovePenTo (int,int,HDC);
extern void WinMovePen (int,int,HDC);
extern void WinLinePenTo (int,int,HDC);
extern void WinLinePen (int,int,HDC);

extern void WinDrawPoint (int,int,HDC);
extern void WinDrawLine (int,int,int,int,HDC);
extern void WinDrawCurve (int,int,int,int,int,int,int,int,HDC);

extern void WinDrawCPoint (int,int,int,int,int,HDC);
extern void WinDrawCLine (int,int,int,int,int,int,int,HDC);
extern void WinDrawCCurve (int,int,int,int,int,int,int,int,int,int,int,HDC);

extern void WinDrawChar (char,HDC);
extern void WinDrawString (char *,HDC);

extern void WinDrawRectangle (int,int,int,int,HDC);
extern void WinFillRectangle (int,int,int,int,HDC);
extern void WinEraseRectangle (int,int,int,int,HDC);
extern void WinInvertRectangle (int,int,int,int,HDC);
extern void WinMoveRectangleTo (int,int,int,int,int,int,HDC);
extern void WinMoveRectangle (int,int,int,int,int,int,HDC);
extern void WinCopyRectangleTo (int,int,int,int,int,int,HDC);
extern void WinCopyRectangle (int,int,int,int,int,int,HDC);
extern void WinScrollRectangle (int,int,int,int,int,int,HDC,int*,int*,int*,int*);

extern void WinDrawRoundRectangle (int,int,int,int,int,int,HDC);
extern void WinFillRoundRectangle (int,int,int,int,int,int,HDC);
extern void WinEraseRoundRectangle (int,int,int,int,int,int,HDC);
extern void WinInvertRoundRectangle (int,int,int,int,int,int,HDC);

extern void WinDrawOval (int,int,int,int,HDC);
extern void WinFillOval (int,int,int,int,HDC);
extern void WinEraseOval (int,int,int,int,HDC);
extern void WinInvertOval (int,int,int,int,HDC);

extern void WinDrawCircle (int,int,int,HDC);
extern void WinFillCircle (int,int,int,HDC);
extern void WinEraseCircle (int,int,int,HDC);
extern void WinInvertCircle (int,int,int,HDC);

extern void WinDrawWedge (int,int,int,int,int,int,int,int,HDC);
extern void WinFillWedge (int,int,int,int,int,int,int,int,HDC);
extern void WinEraseWedge (int,int,int,int,int,int,int,int,HDC);
extern void WinInvertWedge (int,int,int,int,int,int,int,int,HDC);

extern void WinStartPolygon (int);
extern void WinEndPolygon ();
extern void WinAddPolygonPoint (int,int);
extern void WinDrawPolygon (HDC);
extern void WinFillPolygon (HDC);
extern void WinErasePolygon (HDC);
extern void WinInvertPolygon (HDC);

//	Routines that temporarily create and destroy a DISPLAY HDC. Use this HDC only locally.
extern HDC WinCreateScreenHDC ();
extern void WinDestroyScreenHDC (HDC);

extern void WinDrawResizedBitmap (int,int,int,int,int,int,HBITMAP,HDC);
extern void WinDrawBitmap (int,int,int,int,HBITMAP,HDC);
extern HBITMAP WinCreateBitmap (HDC, LPCTSTR, int *, int *);

extern void WinSetFont (char *,int,int,HDC);
extern void WinSetFontName (char *,HDC);
extern void WinSetFontStyle (int,HDC);
extern void WinSetFontSize (int,HDC);
extern void WinGetFontInfo (char *,int,int,BOOL,HDC,int*,int*,int*,int*);
extern void WinGetPicFontInfo (HDC,int*,int*,int*,int*);

extern int WinGetPicStringWidth (char *,HDC);
extern int WinGetPicCharWidth (char,HDC);
extern int WinGetStringWidth (char *,char *,int,int,BOOL,HDC);
extern int WinGetCharWidth (char,char *,int,int,BOOL,HDC);

//	Get the resolution of a picture
extern void getResolutionC(HDC,int*,int*);

//	Get scaling factors, which have to be applied to coordinates for clipping regions in case
//	of emulating the screen resolution for printing (MM_ISOTROPIC)
extern void WinGetPictureScaleFactor(HDC,int*,int*,int*,int*);
