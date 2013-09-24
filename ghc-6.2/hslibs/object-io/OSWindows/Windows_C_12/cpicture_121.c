/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to drawing.
********************************************************************************************/
#include "cpicture_121.h"
#include "cCrossCall_121.h"
#include "cCrossCallWindows_121.h"

HDC WinGetDC (int hwnd)
{
	HDC hdc;

	hdc = GetDC ((HWND) hwnd);
	if (hdc==NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinGetDC","GetDC returned NULL");

	return hdc;
}	/* WinGetDC */

void WinReleaseDC (int hwnd, HDC hdc)
{
	int returncode;

	returncode = ReleaseDC ((HWND) hwnd, hdc);
	if (returncode==0)
		rMessageBox (NULL,MB_APPLMODAL,"WinReleaseDC","ReleaseDC returned zero");
}	/* WinReleaseDC */

int WinGetVertResolution (void)
{
	static int res = 0;

	if (res == 0)
	{
		HDC screen;
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
		if (screen==NULL)
			rMessageBox (NULL,MB_APPLMODAL,"WinGetVertResolution","CreateDC returned NULL.");
		res = GetDeviceCaps (screen, LOGPIXELSY);
		DeleteDC (screen);
	};

	return res;
}	/* WinGetVertResolution */

int WinGetHorzResolution (void)
{
	static int res = 0;

	if (res == 0)
	{
		HDC screen;
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
		res = GetDeviceCaps (screen, LOGPIXELSX);
		DeleteDC (screen);
	};

	return res;
}	/* WinGetHorzResolution */


/*------------------------------------*\
|									   |
|	   Helper functions 			   |
|									   |
\*------------------------------------*/

static HPEN thePen;
static HBRUSH theNormalBrush;
static HBRUSH theBackBrush;
static HFONT theFont;

static int penSize;
static int penPat;
static int penMode;
static COLORREF penColor;
static COLORREF backColor;
static char curFont[LF_FACESIZE];
static int fontstyle;
static int fontsize;

static POINT *thePolygon;
static int thePolygonIndex;

#define DRAWING    0
#define FILLING    1
#define INVERTING  2
#define ERASING    3

static int lastActivity;

// MW...
static int PointsToPix(HDC hdc, int size)
{
	// convert font size in points to pixels (which depends on the device resolution)

    int vRes;
	int mapMode = GetMapMode(hdc);
	if (mapMode==MM_ISOTROPIC)
		vRes = WinGetVertResolution();
	  else
		vRes = GetDeviceCaps(hdc, LOGPIXELSY);
	return (size * vRes) / 72;
/*	MW: currently, the MM_ISOTROPIC mapping mode is only used for printing with the emulation
	of the screen resolution. For that purpose, points are not subject to the scaling, which
	MM_ISOTROPIC performs.
*/
}
// ...MW

void WinInitPicture (int size, int mode,
					 int pr, int pg, int pb,
					 int br, int bg, int bb,
					 int x, int y,
					 char *fname, int fstyle, int fsize,
					 int ox, int oy,
					 HDC hdc
				    )
{
	LOGBRUSH lb;
	LOGFONT lf;

	penSize = size;
	penPat = iBlackPattern;
	penMode = mode;
	penColor = RGB (pr, pg, pb);
	backColor = RGB (br, bg, bb);
	lastActivity = FILLING;

	thePolygon = NULL;
	SetPolyFillMode (hdc, WINDING);

	strcpy (curFont, fname);
	fontstyle = fstyle;
	fontsize = PointsToPix(hdc,fsize);
				// PointsToPix by MW

	SetLogFontData (&lf, curFont, fontstyle, fontsize);

	lb.lbStyle = BS_SOLID;
	lb.lbColor = penColor;
	lb.lbHatch = 0;
/*	thePen = ExtCreatePen (penSize == 1 ? PS_COSMETIC | PS_SOLID : PS_GEOMETRIC | PS_INSIDEFRAME, penSize, &lb, 0, NULL); */
	thePen = ExtCreatePen (PS_GEOMETRIC | PS_INSIDEFRAME, penSize, &lb, 0, NULL);

	/*SetBrushOrgEx (hdc, ox % 8, oy % 8, NULL);	PA: changed to zero because origin of gui components is always zero. */
	SetBrushOrgEx (hdc,0,0,NULL);
	theNormalBrush = CreateSolidBrush (penColor);
	theBackBrush = CreateSolidBrush (backColor);
	theFont = CreateFontIndirect (&lf);

	SaveDC (hdc);

/*	SetWindowOrgEx (hdc, ox, oy, NULL);	PA: the origin of every gui component is always zero.*/
	SetWindowOrgEx (hdc, 0,0, NULL);

	SelectObject (hdc, GetStockObject (NULL_PEN));
	SelectObject (hdc, theNormalBrush);
	SelectObject (hdc, theFont);

	SetBkMode (hdc, TRANSPARENT);
	SetTextColor (hdc, penColor);
	SetBkColor (hdc, backColor);
	MoveToEx (hdc, x, y, NULL);
	SetTextAlign (hdc, TA_LEFT | TA_BASELINE | TA_UPDATECP);
	WinSetMode (penMode, hdc);
	SetStretchBltMode (hdc,COLORONCOLOR);		/* PA: when stretching bitmaps, use COLORONCOLOR mode. */

	if (ghCaretWnd)
	{
		int mess, p1, p2, p3, p4, p5, p6;
		WinKickOsThread (CcRqHIDECARET, (int) ghCaretWnd, 0, 0, 0, 0, 0,
						 &mess, &p1, &p2, &p3, &p4, &p5, &p6);
	}
}	/* WinInitPicture */

void WinDonePicture (HDC hdc,
					 int *size, int *mode,
					 int *pr, int *pg, int *pb,
					 int *br, int *bg, int *bb,
					 int *x, int *y,
					 char ** fname, int *fstyle, int *fsize
				    )
{
	POINT p;

	GetCurrentPositionEx (hdc, &p);

	RestoreDC (hdc, -1);
	DeleteObject (thePen);
	DeleteObject (theNormalBrush);
	DeleteObject (theBackBrush);
	DeleteObject (theFont);

	if (ghCaretWnd)
	{
		int mess, p1, p2, p3, p4, p5, p6;
		WinKickOsThread (CcRqSHOWCARET, (int) ghCaretWnd, 0, 0, 0, 0, 0,
						 &mess, &p1, &p2, &p3, &p4, &p5, &p6);
	}

	*size = penSize;
	*mode = penMode;

	*pr = GetRValue (penColor);
	*pg = GetGValue (penColor);
	*pb = GetBValue (penColor);

	*br = GetRValue (backColor);
	*bg = GetGValue (backColor);
	*bb = GetBValue (backColor);

	*x = p.x;
	*y = p.y;

	*fname = strcpy(malloc(strlen(curFont)+1), curFont);
	*fstyle= fontstyle;
	*fsize = fontsize;
}	/* WinDonePicture */

void WinClipPicture (int left, int top, int right, int bot, HDC ihdc)
{
	int error;
	HRGN region;

	region = CreateRectRgn (left, top, right + 1, bot + 1);
	if (region==NULL)
		ErrorExit ("Fatal error in WinClipPicture: CreateRectRgn returned NULL.");

	error = ExtSelectClipRgn (ihdc, region, RGN_AND);
	if (error==ERROR)
		ErrorExit ("Fatal error in WinClipPicture: ExtSelectClipRgn returned ERROR.");

	DeleteObject (region);
}	/* WinClipPicture */

/*	PA: Set and get the clipping region of a picture:
		WinClipRgnPicture    takes the intersection of the argument clipRgn with the current clipping region.
		WinSetClipRgnPicture sets the argument clipRgn as the new clipping region.
		WinGetClipRgnPicture gets the current clipping region.
*/
void WinClipRgnPicture (HRGN cliprgn, HDC ihdc)
{
	int error;

	error = ExtSelectClipRgn (ihdc, cliprgn, RGN_AND);

	if (error==ERROR)
		ErrorExit ("Fatal error in WinClipRgnPicture: ExtSelectClipRgn returned ERROR.");
}	/* WinClipRgnPicture */

void WinSetClipRgnPicture (HRGN cliprgn, HDC ihdc)
{
	int error;

	error = SelectClipRgn (ihdc, cliprgn);

	if (error==ERROR)
		ErrorExit ("Fatal error in WinSetClipRgnPicture: SelectClipRgn returned ERROR.");
}	/* WinSetClipRgnPicture */

HRGN WinGetClipRgnPicture (HDC ihdc)
{
	HRGN theRegion;
	int error;

	theRegion = CreateRectRgn (0,0, 1,1);
	if (theRegion==NULL)
		ErrorExit ("Fatal error in WinGetClipRgnPicture: CreateRectRgn returned NULL.");

	error = GetClipRgn (ihdc, theRegion);

	if (error==0)
	{
		DeleteObject (theRegion);
		theRegion = NULL;
	}
	if (error==-1)
		ErrorExit ("Fatal error in WinGetClipRgnPicture: GetClipRgn returned -1.");

	return theRegion;
}	/* WinGetClipRgnPicture */


/*	Operations to create, modify, and destroy polygon shapes.
*/
typedef POINT *PolyShape;

PolyShape WinAllocPolyShape (int size)
{
	return (POINT *) rmalloc (size * sizeof (POINT));
}	/* WinAllocPolyShape */

void WinSetPolyPoint (int i, int x, int y, PolyShape shape)
{
	shape[i].x = x;
	shape[i].y = y;
}	/* WinSetPolyPoint */

void WinFreePolyShape (POINT * shape)
{
	rfree (shape);
}	/* WinFreePolyShape */


/*	Operations to create, modify and destroy regions.
*/
HRGN WinCreateRectRgn (int nLeftRect, int nTopRect, int nRightRect, int nBottomRect)
{
	HRGN theRegion;

	theRegion = CreateRectRgn (nLeftRect, nTopRect, nRightRect, nBottomRect);
	if (theRegion==NULL)
		ErrorExit ("Fatal error in WinCreateRectRgn: CreateRectRgn returned NULL.");

	return theRegion;
}	/* WinCreateRectRgn */

HRGN WinCreatePolygonRgn (POINT * lppt, int cPoints, int fnPolyFillMode)
{
	HRGN theRegion;

	theRegion = CreatePolygonRgn (lppt, cPoints, fnPolyFillMode);
	if (theRegion==NULL)
		ErrorExit ("Fatal error in WinCreatePolygonRgn: CreatePolygonRgn returned NULL.");

	return theRegion;
}	/* WinCreatePolygonRgn */

HRGN WinSetRgnToRect (int left, int top, int right, int bottom, HRGN rgn)
{
	HRGN region;
	int error;

	region = CreateRectRgn (left, top, right, bottom);
	if (region==NULL)
		ErrorExit ("Fatal error in WinSetRgnToRect: CreateRectRgn returned NULL.");

	error = CombineRgn (rgn, region, region, RGN_COPY);
	if (error==ERROR)
		ErrorExit ("Fatal error in WinSetRgnToRect: CombineRgn returned ERROR.");

	DeleteObject (region);
	return rgn;
}	/* WinSetRgnToRect */

HRGN WinCombineRgn (HRGN hrgnDest, HRGN hrgnSrc1, HRGN hrgnSrc2, int fnCombineMode)
{
	int error;

	error = CombineRgn (hrgnDest, hrgnSrc1, hrgnSrc2, fnCombineMode);
	if (error==ERROR)
		ErrorExit ("Fatal error in WinCombineRgn: CombineRgn returned ERROR.");
	return hrgnDest;
}	/* WinCombineRgn */

void WinGetRgnBox (HRGN hrgn, int *left, int *top, int *right, int *bottom, BOOL * isrect, BOOL * isempty)
{
	int result;
	RECT boundbox;

	result = GetRgnBox (hrgn, &boundbox);

	*left   = boundbox.left;
	*right  = boundbox.right;
	*top    = boundbox.top;
	*bottom = boundbox.bottom;

	*isrect  = result == SIMPLEREGION;
	*isempty = result == NULLREGION;
}	/* WinGetRgnBox */

void WinDeleteObject (HGDIOBJ hObject)
{
	if (!DeleteObject (hObject))
		ErrorExit ("Fatal error in WinDeleteObject: DeleteObject returned FALSE.");
}


/*
lastActivity:
		DRAWING:   pen=thePen	brush=NULL_BRUSH	 ROP2=if (penMode==iModeXor) R2_NOT R2_COPYPEN
		FILLING:   pen=NULL_PEN brush=theNormalBrush ROP2=if (penMode==iModeXor) R2_NOT R2_COPYPEN
		INVERTING: pen=NULL_PEN brush=theNormalBrush ROP2=R2_NOT
		ERASING:   pen=NULL_PEN brush=theBackBrush	 ROP2=if (penMode==iModeXor) R2_NOT R2_COPYPEN
*/

static void StartDrawing (HDC hdc)
{
	switch (lastActivity)
	{
		case DRAWING:
			break;
		case FILLING:
			SelectObject (hdc, thePen);
			SelectObject (hdc, GetStockObject (NULL_BRUSH));
			break;
		case INVERTING:
			SelectObject (hdc, thePen);
			SelectObject (hdc, GetStockObject (NULL_BRUSH));
			if (penMode != iModeXor)
				SetROP2 (hdc, R2_COPYPEN);
			break;
		case ERASING:
			SelectObject (hdc, thePen);
			SelectObject (hdc, GetStockObject (NULL_BRUSH));
			/* JVG */
			if (penMode == iModeXor)
				SetROP2 (hdc, R2_NOT);
			/**/
			break;
	}
	lastActivity = DRAWING;
}	/* StartDrawing */

static void StartFilling (HDC hdc)
{
	switch (lastActivity)
	{
		case DRAWING:
			SelectObject (hdc, GetStockObject (NULL_PEN));
			SelectObject (hdc, theNormalBrush);
			break;
		case FILLING:
			break;
		case INVERTING:
			if (penMode != iModeXor)
				SetROP2 (hdc, R2_COPYPEN);
			break;
		case ERASING:
			SelectObject (hdc, theNormalBrush);
			/* JVG */
			if (penMode == iModeXor)
				SetROP2 (hdc, R2_NOT);
			/**/
			break;
	}
	lastActivity = FILLING;
}	/* StartFilling */

static void StartInverting (HDC hdc)
{
	switch (lastActivity)
	{
			case DRAWING:
			SelectObject (hdc, GetStockObject (NULL_PEN));
			SelectObject (hdc, theNormalBrush);
			SetROP2 (hdc, R2_NOT);
			break;
		case FILLING:
			SetROP2 (hdc, R2_NOT);
			break;
		case INVERTING:
			break;
		case ERASING:
			SelectObject (hdc, theNormalBrush);
			SetROP2 (hdc, R2_NOT);
			break;
	}
	lastActivity = INVERTING;
}	/* StartInverting */

static void StartErasing (HDC hdc)
{
	switch (lastActivity)
	{
		case DRAWING:
			SelectObject (hdc, GetStockObject (NULL_PEN));
			SelectObject (hdc, theBackBrush);
			/* JVG */
			if (penMode == iModeXor)
				SetROP2 (hdc, R2_COPYPEN);
			/**/
			break;
		case FILLING:
			SelectObject (hdc, theBackBrush);
			/* JVG */
			if (penMode == iModeXor)
				SetROP2 (hdc, R2_COPYPEN);
			/**/
			break;
		case INVERTING:
			SelectObject (hdc, theBackBrush);
/* JVG			if( penMode != iModeXor ) */
			SetROP2 (hdc, R2_COPYPEN);
			break;
		case ERASING:
			break;
	}
	lastActivity = ERASING;
}	/* StartErasing */

static void ChangeThePen (HDC hdc)
{
	HPEN hp;
	LOGBRUSH lb;
	DWORD style;

	lb.lbStyle = BS_SOLID;
	lb.lbColor = penColor;
	lb.lbHatch = 0;

	if (penSize == 1)
		style = PS_COSMETIC | PS_SOLID;
	else
		style = PS_GEOMETRIC | PS_INSIDEFRAME;

	hp = ExtCreatePen (style, penSize, &lb, 0, NULL);

	if (lastActivity == DRAWING)
		SelectObject (hdc, hp);
	DeleteObject (thePen);

	thePen = hp;
}	/* ChangeThePen */

static void ChangeNormalBrush (HDC hdc)
{
	HBRUSH hb;

	hb = CreateSolidBrush (penColor);

	if (lastActivity == FILLING || lastActivity == INVERTING)
		SelectObject (hdc, hb);
	DeleteObject (theNormalBrush);

	theNormalBrush = hb;
}	/* ChangeNormalBrush */

static void ChangeBackBrush (HDC hdc)
{
	HBRUSH hb;

	hb = CreateSolidBrush (backColor);

	if (lastActivity == ERASING)
		SelectObject (hdc, hb);
	DeleteObject (theBackBrush);

	theBackBrush = hb;
}	/* ChangeBackBrush */


/*------------------------------------*\
|	   Interface functions			   |
\*------------------------------------*/

void WinSetPenSize (int size, HDC ihdc)
{
	penSize = size;
	ChangeThePen (ihdc);
}	/* WinSetPenSize */

void WinSetPenColor (int red, int green, int blue, HDC ihdc)
{
	penColor = RGB (red, green, blue);

	ChangeThePen (ihdc);
	ChangeNormalBrush (ihdc);

	SetTextColor (ihdc, penColor);
}	/* WinSetPenColor */

void WinSetBackColor (int red, int green, int blue, HDC ihdc)
{
	backColor = RGB (red, green, blue);

	ChangeBackBrush (ihdc);

	SetBkColor (ihdc, backColor);
}	/* WinSetBackColor */

void WinSetMode (int mode, HDC ihdc)
{
	switch (mode)
	{
		case iModeCopy:
			penMode = iModeCopy;
			 /*JVG*/ SetROP2 (ihdc, lastActivity == INVERTING ? R2_NOT : R2_COPYPEN);	/**/
			SetBkMode (ihdc, OPAQUE);
			break;
		case iModeXor:
			penMode = iModeXor;
			 /*JVG*/ SetROP2 (ihdc, lastActivity == ERASING ? R2_COPYPEN : R2_NOT); 	/**/
			SetBkMode (ihdc, TRANSPARENT);
			break;
		case iModeOr:
		default:
			penMode = iModeOr;
			 /*JVG*/ SetROP2 (ihdc, lastActivity == INVERTING ? R2_NOT : R2_COPYPEN);	/**/
			SetBkMode (ihdc, TRANSPARENT);
			break;
	}
}	/* WinSetMode */

void WinSetPattern (int pattern, HDC ihdc)
{
}	/* WinSetPattern */


void WinGetPenPos (HDC ihdc, int *x, int *y)
{
	POINT p;

	GetCurrentPositionEx (ihdc, &p);

	*x = p.x;
	*y = p.y;
}	/* WinGetPenPos */

void WinMovePenTo (int x, int y, HDC ihdc)
{
	MoveToEx (ihdc, x, y, NULL);
}	/* WinMovePenTo */

void WinMovePen (int dx, int dy, HDC ihdc)
{
	POINT p;

	GetCurrentPositionEx (ihdc, &p);
	MoveToEx (ihdc, p.x + dx, p.y + dy, NULL);
}	/* WinMovePen */

void WinLinePenTo (int x, int y, HDC ihdc)
{
	StartDrawing (ihdc);
	LineTo (ihdc, x, y);
	SetPixelV (ihdc, x, y, penColor);
}	/* WinLinePenTo */

void WinLinePen (int dx, int dy, HDC ihdc)
{
	POINT p;
	int end_x, end_y;

	StartDrawing (ihdc);
	GetCurrentPositionEx (ihdc, &p);

	end_x = p.x + dx;
	end_y = p.y + dy;
	LineTo (ihdc, end_x, end_y);
	SetPixelV (ihdc, end_x, end_y, penColor);
}	/* WinLinePen */

// changed by MW
void WinDrawPoint (int x, int y, HDC ihdc)
{
	if (penSize==1 && GetMapMode(ihdc)==MM_TEXT)	// mapping mode can also be MM_ISOTROPIC
		SetPixelV (ihdc, x, y, penColor);			// (for printing)
	else
		{	POINT p;
			StartDrawing (ihdc);
			MoveToEx (ihdc, x, y, &p);
			LineTo (ihdc, x, y);
			MoveToEx (ihdc, p.x, p.y, NULL);
		};
}	/* WinDrawPoint */

void WinDrawLine (int startx, int starty, int endx, int endy, HDC ihdc)
{
	POINT p;

	StartDrawing (ihdc);
	MoveToEx (ihdc, startx, starty, &p);
	LineTo (ihdc, endx, endy);

	MoveToEx (ihdc, p.x, p.y, NULL);
}	/* WinDrawLine */

void WinDrawCurve (int left, int top, int right, int bot,
				   int startradx, int startrady,
				   int endradx, int endrady,
				   HDC ihdc
				  )
{
	StartDrawing (ihdc);

	Arc (ihdc, left, top, right, bot, startradx, startrady, endradx, endrady);
}	/* WinDrawCurve */

// changed by MW
void WinDrawCPoint (int x, int y,
					int red, int green, int blue,
					HDC ihdc
				   )
{
	if (penSize==1 && GetMapMode(ihdc)==MM_TEXT)	// mapping mode can also be MM_ISOTROPIC
		{	SetPixelV (ihdc, x, y, RGB (red, green, blue));
		}
	else
		WinDrawCLine(x,y,x,y,red,green,blue,ihdc);
}	/* WinDrawCPoint */

void WinDrawCLine (int startx, int starty, int endx, int endy,
				   int red, int green, int blue,
				   HDC ihdc
				  )
{
	POINT p;
	HPEN hp;
	LOGBRUSH lb;

	StartDrawing (ihdc);

	lb.lbStyle = BS_SOLID;
	lb.lbColor = RGB (red, green, blue);
	lb.lbHatch = 0;
	hp = SelectObject (ihdc, ExtCreatePen (PS_GEOMETRIC | PS_INSIDEFRAME, penSize, &lb, 0, NULL));

	MoveToEx (ihdc, startx, starty, &p);
	LineTo (ihdc, endx, endy);

	MoveToEx (ihdc, p.x, p.y, NULL);

	DeleteObject (SelectObject (ihdc, hp));
}	/* WinDrawCLine */

void WinDrawCCurve (int left, int top, int right, int bot,
					int startradx, int startrady,
					int endradx, int endrady,
					int red, int green, int blue,
					HDC ihdc
				   )
{
	HPEN hp;
	LOGBRUSH lb;

	lb.lbStyle = BS_SOLID;
	lb.lbColor = RGB (red, green, blue);
	lb.lbHatch = 0;
	hp = SelectObject (ihdc, ExtCreatePen (PS_GEOMETRIC | PS_INSIDEFRAME, penSize, &lb, 0, NULL));

	StartDrawing (ihdc);

	Arc (ihdc, left, top, right, bot, startradx, startrady, endradx, endrady);

	DeleteObject (SelectObject (ihdc, hp));
}	/* WinDrawCCurve */


void WinDrawChar (char c, HDC ihdc)
{
	int oldmode;

	oldmode = GetBkMode (ihdc);
	SetBkMode (ihdc, TRANSPARENT);
	TextOut (ihdc, 0, 0, &c, 1);
	SetBkMode (ihdc, oldmode);
}	/* WinDrawChar */

void WinDrawString (char *string, HDC ihdc)
{
	int oldmode;

	oldmode = GetBkMode (ihdc);
	if (penMode==iModeXor)					/* Check if currently in XOR mode */
	{
		SetBkMode (ihdc, OPAQUE);			/* in that case background should be OPAQUE. */
	}
	else
	{
		SetBkMode (ihdc, TRANSPARENT);		/* otherwise it should be TRANSPARENT. */
	}
	if (!TextOut (ihdc, 0, 0, string, strlen(string)))
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawString","TextOut failed.");
	SetBkMode (ihdc, oldmode);
}	/* WinDrawString */


void WinDrawRectangle (int left, int top, int right, int bot, HDC ihdc)
{
	StartDrawing (ihdc);
	Rectangle (ihdc, left, top, right, bot);
}	/* WinDrawRectangle */

void WinFillRectangle (int left, int top, int right, int bot, HDC ihdc)
{
	StartFilling (ihdc);
	Rectangle (ihdc, left, top, right + 1, bot + 1);
}	/* WinFillRectangle */

void WinEraseRectangle (int left, int top, int right, int bot, HDC ihdc)
{
	StartErasing (ihdc);
	Rectangle (ihdc, left, top, right + 1, bot + 1);
}	/* WinEraseRectangle */

void WinInvertRectangle (int left, int top, int right, int bot, HDC ihdc)
{
	StartInverting (ihdc);
	Rectangle (ihdc, left, top, right + 1, bot + 1);
}	/* WinInvertRectangle */

void WinMoveRectangleTo (int left, int top, int right, int bot, int x, int y, HDC ihdc)
{
	WinMoveRectangle (left,top, right,bot, x-left, y-top, ihdc);
}	/* WinMoveRectangleTo */

void WinMoveRectangle (int left, int top, int right, int bot, int dx, int dy, HDC ihdc)
{
	int w, h;
	HWND hwnd;

	hwnd = WindowFromDC (ihdc);
	if (hwnd != NULL)
	{
		RECT r;
		POINT p;

		GetClientRect (hwnd, &r);
		GetWindowOrgEx (ihdc, &p);
		left = max (left, r.left + p.x);
		top = max (top, r.top + p.y);
		right = min (right, r.right + p.x);
		bot = min (bot, r.bottom + p.y);
	}

	w = right - left;
	h = bot - top;

	WinCopyRectangle (left, top, right, bot, dx, dy, ihdc);

	StartErasing (ihdc);

	if (dx > w || dy > h)
	{
		Rectangle (ihdc, left, top, right + 1, bot + 1);
		return;
	}

	if (dx < 0)
		Rectangle (ihdc, right - dx, top, right + 1, bot + 1);
	else
		Rectangle (ihdc, left, top, left + dx + 1, bot + 1);

	if (dy < 0)
		Rectangle (ihdc, left, bot - dy, right + 1, bot + 1);
	else
		Rectangle (ihdc, left, top, right + 1, top + dy + 1);
}	/* WinMoveRectangle */

void WinCopyRectangleTo (int left, int top, int right, int bot, int x, int y, HDC ihdc)
{
	WinCopyRectangle (left,top, right,bot, x-left,y-top, ihdc);
}	/* WinCopyRectangleTo */

void WinCopyRectangle (int left, int top, int right, int bottom, int dx, int dy, HDC ihdc)
{
	RECT scrollRect;

	scrollRect.left   = left;
	scrollRect.top    = top;
	scrollRect.right  = right;
	scrollRect.bottom = bottom;

	if (!ScrollDC (ihdc, dx,dy, &scrollRect, &scrollRect, NULL, NULL))
	{
		rMessageBox (NULL,MB_APPLMODAL,"WinCopyRectangle","ScrollDC failed");
	}
}	/* WinCopyRectangle */

/*	PA: new routine to scroll part of the content of a window.
		It is assumed that scrolling happens in one direction only (dx<>0 && dy==0 || dx==0 && dy<>0).
		The result rect (oleft,otop,oright,obottom) is the bounding box of the update area that
		remains to be updated. If all are zero, then nothing needs to be updated.
*/
void WinScrollRectangle (int left, int top, int right, int bottom, int dx, int dy, HDC ihdc,
						 int * oleft, int * otop, int * oright, int * obottom
					    )
{
	RECT scrollRect;
	HRGN hrgnUpdate, hrgnRect;

	scrollRect.left   = left;
	scrollRect.top    = top;
	scrollRect.right  = right;
	scrollRect.bottom = bottom;

	if (dx<0)
	{
		hrgnRect   = CreateRectRgn (right+dx-1,top-1,right+1,bottom+1);
	}
	else if (dx>0)
	{
		hrgnRect   = CreateRectRgn (left-1,top-1,left+dx+1,bottom+1);
	}
	else if (dy<0)
	{
		hrgnRect   = CreateRectRgn (left-1,bottom+dy-1,right+1,bottom+1);
	}
	else if (dy>0)
	{
		hrgnRect   = CreateRectRgn (left-1,top-1,right+1,top+dy+1);
	}
	else
	{
		hrgnRect   = CreateRectRgn (0,0,0,0);
	}
	hrgnUpdate = CreateRectRgn (0,0,1,1);

	if (!ScrollDC (ihdc, dx,dy, &scrollRect, &scrollRect, hrgnUpdate, NULL))
	{
		rMessageBox (NULL,MB_APPLMODAL,"WinScrollRectangle","ScrollDC failed");
	}
	else
	{
		if (CombineRgn (hrgnUpdate, hrgnUpdate, hrgnRect, RGN_DIFF) == NULLREGION)
		{
			*oleft   = 0;
			*otop    = 0;
			*oright  = 0;
			*obottom = 0;
		}
		else
		{
			RECT box;
			GetRgnBox (hrgnUpdate,&box);
			*oleft   = box.left;
			*otop    = box.top;
			*oright  = box.right;
			*obottom = box.bottom;
		}
	}
	DeleteObject (hrgnUpdate);
	DeleteObject (hrgnRect);
}	/* WinScrollRectangle */


void WinDrawRoundRectangle (int left, int top, int right, int bot, int width, int height, HDC ihdc)
{
	StartDrawing (ihdc);
	RoundRect (ihdc, left, top, right, bot, width, height);
}	/* WinDrawRoundRectangle */

void WinFillRoundRectangle (int left, int top, int right, int bot, int width, int height, HDC ihdc)
{
	StartFilling (ihdc);
	RoundRect (ihdc, left, top, right + 1, bot + 1, width, height);
}	/* WinFillRoundRectangle */

void WinEraseRoundRectangle (int left, int top, int right, int bot, int width, int height, HDC ihdc)
{
	StartErasing (ihdc);
	RoundRect (ihdc, left, top, right + 1, bot + 1, width, height);
}	/* WinEraseRoundRectangle */

void WinInvertRoundRectangle (int left, int top, int right, int bot, int width, int height, HDC ihdc)
{
	StartInverting (ihdc);
	RoundRect (ihdc, left, top, right + 1, bot + 1, width, height);
}	/* WinInvertRoundRectangle */


void WinDrawOval (int left, int top, int right, int bot, HDC ihdc)
{
	StartDrawing (ihdc);
	Ellipse (ihdc, left, top, right + 0, bot + 0);
}	/* WinDrawOval */

void WinFillOval (int left, int top, int right, int bot, HDC ihdc)
{
	StartFilling (ihdc);
	Ellipse (ihdc, left, top, right + 1, bot + 1);
}	/* WinFillOval */

void WinEraseOval (int left, int top, int right, int bot, HDC ihdc)
{
	StartErasing (ihdc);
	Ellipse (ihdc, left, top, right + 1, bot + 1);
}	/* WinEraseOval */

void WinInvertOval (int left, int top, int right, int bot, HDC ihdc)
{
	StartInverting (ihdc);
	Ellipse (ihdc, left, top, right + 1, bot + 1);
}	/* WinInvertOval */


void WinDrawCircle (int centerx, int centery, int radius, HDC ihdc)
{
	StartDrawing (ihdc);
	Ellipse (ihdc, centerx - radius, centery - radius, centerx + radius + 0, centery + radius + 0);
}	/* WinDrawCircle */

void WinFillCircle (int centerx, int centery, int radius, HDC ihdc)
{
	StartFilling (ihdc);
	Ellipse (ihdc, centerx - radius, centery - radius, centerx + radius + 1, centery + radius + 1);
}	/* WinFillCircle */

void WinEraseCircle (int centerx, int centery, int radius, HDC ihdc)
{
	StartErasing (ihdc);
	Ellipse (ihdc, centerx - radius, centery - radius, centerx + radius + 1, centery + radius + 1);
}	/* WinEraseCircle */

void WinInvertCircle (int centerx, int centery, int radius, HDC ihdc)
{
	StartInverting (ihdc);
	Ellipse (ihdc, centerx - radius, centery - radius, centerx + radius + 1, centery + radius + 1);
}	/* WinInvertCircle */


void WinDrawWedge (int left, int top, int right, int bot,
				   int startradx, int startrady,
				   int endradx, int endrady,
				   HDC ihdc
				  )
{
	int theCorrection;

	StartDrawing (ihdc);

	if (penSize > 1)
		theCorrection = 1;
	else
		theCorrection = 0;

	Pie (ihdc, left, top, right + theCorrection, bot + theCorrection, startradx, startrady, endradx, endrady);
}	/* WinDrawWedge */

void WinFillWedge (int left, int top, int right, int bot,
				   int startradx, int startrady,
				   int endradx, int endrady,
				   HDC ihdc
				  )
{
	StartFilling (ihdc);
	Pie (ihdc, left, top, right + 1, bot + 1, startradx, startrady, endradx, endrady);
}	/* WinFillWedge */

void WinEraseWedge (int left, int top, int right, int bot,
					int startradx, int startrady,
					int endradx, int endrady,
					HDC ihdc
				   )
{
	StartErasing (ihdc);
	Pie (ihdc, left, top, right + 1, bot + 1, startradx, startrady, endradx, endrady);
}	/* WinEraseWedge */

void WinInvertWedge (int left, int top, int right, int bot,
					 int startradx, int startrady,
					 int endradx, int endrady,
					 HDC ihdc
				    )
{
	StartInverting (ihdc);
	Pie (ihdc, left, top, right + 1, bot + 1, startradx, startrady, endradx, endrady);
}	/* WinInvertWedge */


void WinStartPolygon (int size)
{
	thePolygon = rmalloc (size * sizeof (POINT));
	thePolygonIndex = 0;
}	/* WinStartPolygon */

void WinEndPolygon ()
{
	rfree (thePolygon);
	thePolygon = NULL;
}	/* WinEndPolygon */

void WinAddPolygonPoint (int x, int y)
{
	thePolygon[thePolygonIndex].x = x;
	thePolygon[thePolygonIndex].y = y;
	thePolygonIndex++;
}	/* WinAddPolygonPoint */

void WinDrawPolygon (HDC ihdc)
{
	StartDrawing (ihdc);
	Polygon (ihdc, thePolygon, thePolygonIndex);
}	/* WinDrawPolygon */

void WinFillPolygon (HDC ihdc)
{
	StartFilling (ihdc);
	Polygon (ihdc, thePolygon, thePolygonIndex);
}	/* WinFillPolygon */

void WinErasePolygon (HDC ihdc)
{
	StartErasing (ihdc);
	Polygon (ihdc, thePolygon, thePolygonIndex);
}	/* WinErasePolygon */

void WinInvertPolygon (HDC ihdc)
{
	StartInverting (ihdc);
	Polygon (ihdc, thePolygon, thePolygonIndex);
}	/* WinInvertPolygon */


/*	PA: two new routines that temporarily create and destroy a DISPLAY HDC.
		Use this HDC only for local use.
*/
HDC WinCreateScreenHDC()
{
	return CreateDC ("DISPLAY",NULL,NULL,NULL);
}	/* WinCreateScreenHDC */

void WinDestroyScreenHDC (HDC ihdc)
{
	DeleteDC (ihdc);
}	/* WinDestroyScreenHDC */


static void getBitmapData(/*input:*/ char *startOfFile,	// points to beginning of bitmap
						  /*output*/ char **startOfData,char **startOfHeader, int *height)
{
	int  dataOffset     = *((int*)(startOfFile + 10));
	*startOfData		= startOfFile + dataOffset;
	*startOfHeader		= startOfFile + 14;
	*height		        = *((int*)(*startOfHeader + 8));
}

/*	WinDrawResizedBitmap draws a bitmap on screen. For reasons of efficiency it uses an
	already created bitmap handle.
*/
void WinDrawResizedBitmap (int sourcew, int sourceh, int destx, int desty, int destw, int desth,
						   HBITMAP hbmp, HDC hdc
						  )
{
	HDC compatibleDC;
	POINT sourcesize, destsize, dest, origin;
	HGDIOBJ prevObj;

	sourcesize.x = sourcew;
	sourcesize.y = sourceh;
	origin.x     = 0;
	origin.y     = 0;
	destsize.x   = destw;
	destsize.y   = desth;
	dest.x       = destx;
	dest.y       = desty;

	//	Create a compatible device context
	compatibleDC = CreateCompatibleDC (hdc);
	if (compatibleDC == NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawResizedBitmap","CreateCompatibleDC failed");

	//	Select bitmap into compatible device context
	prevObj = SelectObject (compatibleDC, hbmp);
	SetMapMode (compatibleDC, GetMapMode (hdc));
	DPtoLP (hdc, &destsize, 1);
	DPtoLP (hdc, &dest, 1);
	DPtoLP (compatibleDC, &sourcesize, 1);
	DPtoLP (compatibleDC, &origin, 1);

	if (!StretchBlt (hdc, dest.x, dest.y, destsize.x, destsize.y, compatibleDC, origin.x, origin.y, sourcesize.x, sourcesize.y, SRCCOPY))
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawResizedBitmap","StretchBlt failed");

	SelectObject (compatibleDC, prevObj);
	DeleteDC (compatibleDC);
}	/* WinDrawResizedBitmap */

// ... MW


/*	WinDrawBitmap must be used for drawing bitmaps on screen.
	For reasons of efficiency it uses memory device context, BitBlt, and bitmap handle.
*/
void WinDrawBitmap (int w, int h, int destx, int desty,
					HBITMAP hbmp, HDC hdc
				   )
{
	HDC compatibleDC;
	POINT size, origin, dest;
	HGDIOBJ prevObj;

	size.x   = w;
	size.y   = h;
	origin.x = 0;
	origin.y = 0;
	dest.x   = destx;
	dest.y   = desty;

	//	Create a compatible device context
	compatibleDC = CreateCompatibleDC (hdc);
	if (compatibleDC == NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawBitmap","CreateCompatibleDC failed");

	//	Select bitmap into compatible device context
	prevObj = SelectObject (compatibleDC, hbmp);
	SetMapMode (compatibleDC, GetMapMode (hdc));
	DPtoLP (hdc, &size, 1);
	DPtoLP (hdc, &dest, 1);
	DPtoLP (compatibleDC, &origin, 1);

	BitBlt (hdc, dest.x, dest.y, size.x, size.y, compatibleDC, origin.x, origin.y, SRCCOPY);

	SelectObject (compatibleDC, prevObj);
	DeleteDC (compatibleDC);
}	/* WinDrawBitmap */

HBITMAP WinCreateBitmap (HDC hdc, LPCTSTR lpszName, int *pWidth, int *pHeight)
{
	HBITMAP hbmp;
	BITMAPINFO bi;

	*pWidth  = 0;
	*pHeight = 0;

	hbmp = (HBITMAP) LoadImage(ghInst, lpszName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);

	if (!hbmp)
		return NULL;

	memset(&bi, 0, sizeof(bi));
	bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
	if (!GetDIBits(hdc, hbmp, 0, 0, NULL, &bi, DIB_RGB_COLORS))
		return NULL;

	*pWidth  = bi.bmiHeader.biWidth;
	*pHeight = bi.bmiHeader.biHeight;

	return hbmp;
}	/* WinCreateBitmap */


/*-----------------------------
	   Font stuff
  -----------------------------*/

void WinSetFont (char *lfname, int style, int size, HDC ihdc)
{
	LOGFONT lf;
	HFONT hf;

	strcpy (curFont, lfname);
	fontstyle = style;
	fontsize = PointsToPix(ihdc,size);
				// MW: PointsToPix

	SetLogFontData (&lf, curFont, fontstyle, fontsize);
	hf = CreateFontIndirect (&lf);

	if (hf==NULL)
	{
		ErrorExit ("Fatal error in WinSetFont: CreateFontIndirect returned NULL.");
	}

	SelectObject (ihdc, hf);
	DeleteObject (theFont);

	theFont = hf;
}	/* WinSetFont */

void WinSetFontName (char *lfname, HDC ihdc)
{
	LOGFONT lf;
	HFONT hf;
	strcpy (curFont, lfname);

	SetLogFontData (&lf, curFont, fontstyle, fontsize);
	hf = CreateFontIndirect (&lf);

	SelectObject (ihdc, hf);
	DeleteObject (theFont);

	theFont = hf;
}	/* WinSetFontName */

void WinSetFontStyle (int style, HDC ihdc)
{
	LOGFONT lf;
	HFONT hf;

	fontstyle = style;

	SetLogFontData (&lf, curFont, fontstyle, fontsize);
	hf = CreateFontIndirect (&lf);

	SelectObject (ihdc, hf);
	DeleteObject (theFont);

	theFont = hf;
}	/* WinSetFontStyle */

void WinSetFontSize (int size, HDC ihdc)
{
	LOGFONT lf;
	HFONT hf;

	fontsize = PointsToPix(ihdc,size);
	//			MW: PointsToPix

	SetLogFontData (&lf, curFont, fontstyle, fontsize);
	hf = CreateFontIndirect (&lf);

	SelectObject (ihdc, hf);
	DeleteObject (theFont);

	theFont = hf;
}	/* WinSetFontSize */

void WinGetFontInfo (char *fontName, int style, int size, BOOL hdcPassed, HDC maybeHdc,
					 int *ascent, int *descent, int *maxwidth, int *leading
				    )
{
	LOGFONT lf;
	HDC screen, dummy;
	HFONT of;
	int	pixSize;

	// MW: I added the hdcPassed and maybeHdc parameters. If (hdcPassed), then the font info
	// will depend on the hdc, that was passed via maybeHdc. In this way font info in a printer hdc can
	// be retrieved. If (!hdcPassed) the default hdc is a DISPLAY.
	// MW
	if (hdcPassed)
		screen = maybeHdc;
	  else
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);

	pixSize = PointsToPix(screen,size);
	// end MW

	SetLogFontData (&lf, "", style, pixSize);

	strcpy(lf.lfFaceName, fontName);

	of = SelectObject (screen, CreateFontIndirect (&lf));

	WinGetPicFontInfo (screen, ascent, descent, maxwidth, leading);

	DeleteObject (SelectObject (screen, of));

	// MW
	if (!hdcPassed)
	// MW
		DeleteDC(screen);
}	/* WinGetFontInfo */

void WinGetPicFontInfo (HDC ihdc, int *ascent, int *descent, int *maxwidth, int *leading)
{
	TEXTMETRIC tm;

	GetTextMetrics (ihdc, &tm);

	*ascent = tm.tmAscent - tm.tmInternalLeading;
	*descent = tm.tmDescent;
	*maxwidth = tm.tmMaxCharWidth;
	*leading = tm.tmInternalLeading + tm.tmExternalLeading;
}	/* WinGetPicFontInfo */

int WinGetPicStringWidth (char *string, HDC hdc)
{
	SIZE sz;
	if (!GetTextExtentPoint32 (hdc, string, strlen(string), &sz))
		rMessageBox (NULL,MB_APPLMODAL,"WinGetPicStringWidth","GetTextExtentPoint32 failed");
	return sz.cx;
}	/* WinGetPicStringWidth */

int WinGetPicCharWidth (char ch, HDC ihdc)
{
	SIZE sz;
	char str[1];

	str[0] = ch;
	GetTextExtentPoint32 (ihdc, str, 1, &sz);

	return sz.cx;
}	/* WinGetPicCharWidth */

int WinGetStringWidth (char *string, char *fontName, int style, int size, BOOL hdcPassed, HDC maybeHdc)
{
	LOGFONT lf;
	HDC screen;
	HFONT of,hf;
	int pixSize;
	int width;

	// MW: I added the hdcPassed and maybeHdc parameters. If (hdcPassed), then the font info
	// will depend on the hdc, that was passed via maybeHdc. In this way font info in a printer hdc can
	// be retrieved. If (!hdcPassed) the default hdc is a DISPLAY.
	// MW
	if (hdcPassed)
	{
		screen = maybeHdc;
	}
	else
	{
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
		if (screen==NULL)
			rMessageBox (NULL,MB_APPLMODAL,"WinGetStringWidth","CreateDC returned NULL");
	}

	pixSize = PointsToPix(screen,size);
	// end MW

	SetLogFontData (&lf, "", style, pixSize);

	strcpy (lf.lfFaceName, fontName);

	hf = CreateFontIndirect (&lf);	// PA+++: hf added to test for NULL
	if (hf==NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinGetStringWidth","CreateFontIndirect returned NULL");
	of = SelectObject (screen, hf);	//CreateFontIndirect (&lf));
	if (of==NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinGetStringWidth","SelectObject of HFONT returned NULL");

	width = WinGetPicStringWidth (string, screen);

	DeleteObject (SelectObject (screen, of));

	// MW
	if (!hdcPassed)
	// MW
		DeleteDC(screen);

	return width;
}	/* WinGetStringWidth */

int WinGetCharWidth (char ch, char *fontName, int style, int size, BOOL hdcPassed, HDC maybeHdc)
{
	LOGFONT lf;
	HDC screen;
	HFONT of;
	int pixSize;
	int width;

	// MW: I added the hdcPassed and maybeHdc parameters. If (hdcPassed), then the font info
	// will depend on the hdc, that was passed via maybeHdc. In this way font info in a printer hdc can
	// be retrieved. If (!hdcPassed) the default hdc is a DISPLAY.
	// MW
	if (hdcPassed)
		screen = maybeHdc;
	  else
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);

	pixSize = PointsToPix(screen,size);
	// end MW

	SetLogFontData (&lf, "", style, pixSize);

	strcpy(lf.lfFaceName, fontName);

	of = SelectObject (screen, CreateFontIndirect (&lf));

	width = WinGetPicCharWidth (ch, screen);

	DeleteObject (SelectObject (screen, of));

	// MW
	if (!hdcPassed)
	// MW
		DeleteDC(screen);

	return width;
}	/* WinGetCharWidth */


// MW...

void getResolutionC(HDC hdc, int *xResP, int *yResP)
{
	int mapMode = GetMapMode(hdc);
	if (mapMode==MM_ISOTROPIC)
		{	*xResP = WinGetHorzResolution();
			*yResP = WinGetVertResolution();
		}
	  else
		{	*xResP = GetDeviceCaps(hdc,LOGPIXELSX);
			*yResP = GetDeviceCaps(hdc,LOGPIXELSY);
		};
/*	MW: currently, the MM_ISOTROPIC mapping mode is only used for printing with the emulation
	of the screen resolution. In that case the screen resolution will be returned.
*/
}	/* getResolutionC */

void WinGetPictureScaleFactor(HDC hdc, int *nh, int *dh, int *nv, int *dv)
{
	if (GetMapMode(hdc)==MM_TEXT)
	    {	*nh = 1;
			*dh = 1;
			*nv = 1;
			*dv = 1;
		}
	  else
		{	SIZE sRes,pRes;
			GetWindowExtEx  (hdc,&sRes);
			GetViewportExtEx(hdc,&pRes);
			*nh	= pRes.cx;
			*dh	= sRes.cx;
			*nv	= pRes.cy;
			*dv	= sRes.cy;
		};

	/*	MW: Microsoft decided, that most drawing operations should work well with the
		MM_ISOTROPIC mapping mode, but not the clipping operations. For these, the clipping
		coordinates have to be scaled
	*/
}	/* WinGetPictureScaleFactor */
// .. MW
