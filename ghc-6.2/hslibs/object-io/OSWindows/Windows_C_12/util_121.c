/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Generally applicable utility routines.
********************************************************************************************/
#include "util_121.h"
#include <stdarg.h>


//	Convenience procedure to fill in LOGFONT struct.
void SetLogFontData (LOGFONT * plf, char *fname, int style, int size)
{
	plf->lfHeight    = -size;
	plf->lfWeight    = (style & iBold) ? 700 : 400;
	plf->lfItalic    = (style & iItalic) ? TRUE : FALSE;
	plf->lfUnderline = (style & iUnderline) ? TRUE : FALSE;
	plf->lfStrikeOut = (style & iStrikeOut) ? TRUE : FALSE;

	strcpy(plf->lfFaceName, fname);

	plf->lfWidth          = 0;
	plf->lfEscapement     = 0;
	plf->lfOrientation    = 0;
	plf->lfCharSet        = DEFAULT_CHARSET;
	plf->lfOutPrecision   = OUT_DEFAULT_PRECIS;
	plf->lfClipPrecision  = CLIP_DEFAULT_PRECIS;
	plf->lfQuality        = DEFAULT_QUALITY;
	plf->lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
}	/* SetLogFontData */


/*	since we don't use the C runtime library, here are some simple
	routines that would normally come from the C runtime lib.
*/

void *rmalloc (DWORD bytes)
{
	void *ptr = malloc (bytes);

	if (!ptr)
	{
		MessageBeep (0xFFFFFFFF);
		ExitProcess (255);
	}

	return ptr;
}

void rfree (HGLOBAL ptr)
{
	free(ptr);
}

int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn)
{
	int nChar = 0;

	do
	{
		*lpWCStr++ = (WORD) * lpAnsiIn;
		nChar++;
	} while (*lpAnsiIn++);

	return nChar;
}	/* nCopyAnsiToWideChar */


/*	The following routines are used to write to the console, or convey runtime errors
	with message boxes.
*/

static char mbuff[_RPRINTBUFSIZE];
static HANDLE hLogFile = NULL;
static BOOL LogFileInited = FALSE;

#ifdef LOGFILE
void rprintf (char *format,...)
{
	va_list arglist;
	int len;
	int cWritten;

	if (!LogFileInited)
	{
 		hLogFile = CreateFile (LOGFILE, /* filename 	   */
							   GENERIC_WRITE,	/* acces mode	   */
							   0,		/* share mode	   */
							   NULL,	/* security 	   */
							   CREATE_ALWAYS,	/* how to create   */
							   FILE_ATTRIBUTE_NORMAL,	/* file attributes */
							   NULL);	/* template file   */
		if (hLogFile == INVALID_HANDLE_VALUE)
		{
			MessageBox (NULL, "Could not open logfile.", NULL, MB_OK | MB_ICONSTOP);
			ExitProcess (1);
		};
		LogFileInited = TRUE;
	}

	va_start (arglist, format);
	len = vsprintf (mbuff, format, arglist);
	va_end (arglist);

	if (!WriteFile (hLogFile,	/* output handle  */
					mbuff,		/* prompt string  */
					len,		/* string length  */
					&cWritten,	/* bytes written  */
					NULL))		/* not overlapped */
	{
		MessageBox (NULL, "Cannot write to stdout --write error.", NULL, MB_OK | MB_ICONSTOP);
		return;
	};
}	/* rprintf */

#endif

void rMessageBox (HWND owner, UINT style, char *title, char *format,...)
{
	va_list arglist;

	va_start (arglist, format);
	wvsprintf (mbuff, format, arglist);
	va_end (arglist);

	MessageBox (owner, mbuff, title, style);
}	/* rMessageBox */

void CheckF (BOOL theCheck, char *checkText, char *checkMess,
		char *filename, int linenum)
{
	if (!theCheck)
	{
		rMessageBox (NULL, MB_OK | MB_ICONSTOP,
			 "Internal check failed", "%s\n\ncheck: %s\nfile: %s\nline: %d",
					 checkMess, checkText, filename, linenum);
		ExitProcess (1);
	}
}	/* CheckF */

void ErrorExit (char *format,...)
{
	va_list arglist;

	va_start (arglist, format);
	wvsprintf (mbuff, format, arglist);
	va_end (arglist);

	MessageBox (NULL, mbuff, NULL, MB_OK | MB_ICONSTOP);
	ExitProcess (1);
}	/* ErrorExit */

/*-----------------------------------/*
/*	support for printing messages	 /*
/*-----------------------------------*/

char * BOOLstring (BOOL b)
{
	if (b)
		return "TRUE";
	else
		return "FALSE";
}	/* BOOLstring */

#ifdef LOGFILE
void printCCI (CrossCallInfo * pcci)
{
	switch (pcci->mess)
	{
			case CcRETURN0:
			{
				rprintf ("CcRETURN0");
			} break;
		case CcRETURN1:
			{
				rprintf ("CcRETURN1");
			} break;
		case CcRETURN2:
			{
				rprintf ("CcRETURN2");
			} break;
		case CcRETURN3:
			{
				rprintf ("CcRETURN3");
			} break;
		case CcRETURN4:
			{
				rprintf ("CcRETURN4");
			} break;
		case CcRETURN5:
			{
				rprintf ("CcRETURN5");
			} break;
		case CcWmPAINT: /* hwnd, t,l,r,b; no return value. */
			{
				rprintf ("CcWmPAINT");
			} break;
		case CcWmCREATE:		/* hwnd; no return value. */
			{
				rprintf ("CcWmCREATE");
			} break;
		case CcWmCHAR:
			{
				rprintf ("CcWmCHAR");
			} break;
		case CcWmCOMMAND:		/* HITEM;		no return value. */
			{
				rprintf ("CcWmCOMMAND");
			} break;
		case CcWmCLOSE: /* hwnd; no return value. */
			{
				rprintf ("CcWmCLOSE");
			} break;
		case CcWmACTIVATE:		/* hwnd; no return value. */
			{
				rprintf ("CcWmACTIVATE");
			} break;
		case CcWmDEACTIVATE:	/* hwnd; no return value. */
			{
				rprintf ("CcWmDEACTIVATE");
			} break;
		case CcWmKEYBOARD:		/* hwnd, charcode, keystate, mods; no return
								   value. */
			{
				rprintf ("CcWmKEYBOARD");
			} break;
		case CcWmMOUSE: /* hwnd, mousestate, x, y, mods; no return
								   value. */
			{
				rprintf ("CcWmMOUSE");
			} break;
		case CcWmSIZE:	/* width, heigth;	*/
			{
				rprintf ("CcWmSIZE");
			} break;
		case CcWmGETHSCROLLVAL: /* hwnd; scroll value return. */
			{
				rprintf ("CcWmGETHSCROLLVAL");
			} break;
		case CcWmGETVSCROLLVAL: /* hwnd; scroll value return. */
			{
				rprintf ("CcWmGETVSCROLLVAL");
			} break;
		case CcWmNEWHTHUMB: 	/* hwnd, hthumb; no return value. */
			{
				rprintf ("CcWmNEWHTHUMB");
			} break;
		case CcWmNEWVTHUMB: 	/* hwnd, vthumb; no return value. */
			{
				rprintf ("CcWmNEWVTHUMB");
			} break;
		case CcWmTIMER: /* HITEM, tickcount; no return value. */
			{
				rprintf ("CcWmTIMER");
			} break;
		case CcWmIDLETIMER: 	/* no params; no return value. */
			{
				rprintf ("CcWmIDLETIMER");
			} break;
		case CcWmINITDIALOG:	/* hdlg; x y w h hwnd result. */
			{
				rprintf ("CcWmINITDIALOG");
			} break;
		case CcWmBUTTONCLICKED: /* hdlg, hbut; no return value. */
			{
				rprintf ("CcWmBUTTONCLICKED");
			} break;
		case CcWmCOMBOSELECT:	/* hwnd, combo, newsel; no return value. */
			{
				rprintf ("CcWmCOMBOSELECT");
			} break;
		case CcWmDRAWCONTROL:	/* hdlog, hctrl, hdc, x,y, enabled; no return
								   value. */
			{
				rprintf ("CcWmDRAWCONTROL");
			} break;
		case CcWmSETCURSOR: 	/* hwnd; cursor code return. */
			{
				rprintf ("CcWmSETCURSOR");
			} break;
		case CcWmLOSEMODELESSDLOG:		/* hwnd;		bool return value. */
			{
				rprintf ("CcWmLOSEMODELESSDLOG");
			} break;
		case CcRqBEEP:	/* no params; no result. */
			{
				rprintf ("CcRqBEEP");
			} break;
		case CcRqDOMESSAGE: 	/* no params; no result */
			{
				rprintf ("CcRqDOMESSAGE");
			} break;
		case CcRqINSERTMENUITEM:		/* on/off, hmenu, textptr, marked,
										   pos; HITEM result. */
			{
				rprintf ("CcRqINSERTMENUITEM");
			} break;
		case CcRqCHECKMENUITEM: /* menu, HITEM, on/off; no result. */
			{
				rprintf ("CcRqCHECKMENUITEM");
			} break;
		case CcRqREMOVEMENUITEM:		/* menu, HITEM; no result. */
			{
				rprintf ("CcRqREMOVEMENUITEM");
			} break;
		case CcRqMODIFYMENUITEM:		/* HITEM, on/off, hmenu, textptr,
										   marked; no result. */
			{
				rprintf ("CcRqMODIFYMENUITEM");
			} break;
		case CcRqITEMENABLE:	/* parent, HITEM, onoff; no result. */
			{
				rprintf ("CcRqITEMENABLE");
			} break;
		case CcRqMODIFYMENU:	/* on/off, hmenu, textptr, hsubmenu, pos; no
								   result. */
			{
				rprintf ("CcRqMODIFYMENU");
			} break;
		case CcRqMENUENABLE:	/* parent, pos, onoff; no result. */
			{
				rprintf ("CcRqMENUENABLE");
			} break;
		case CcRqINSERTSEPARATOR:		/* hmenu, pos; no result. */
			{
				rprintf ("CcRqINSERTSEPARATOR");
			} break;
		case CcRqCREATEPOPMENU: /* no params; HMENU result. */
			{
				rprintf ("CcRqCREATEPOPMENU");
			} break;
		case CcRqDRAWMBAR:		/* no params; no result. */
			{
				rprintf ("CcRqDRAWMBAR");
			} break;
		case CcRqDESTROYWINDOW: /* hwnd; no result. */
			{
				rprintf ("CcRqDESTROYWINDOW");
			} break;
		case CcRqBEGINPAINT:	/* hwnd; HDC result. */
			{
				rprintf ("CcRqBEGINPAINT");
			} break;
		case CcRqENDPAINT:		/* hwnd, hdc; no result. */
			{
				rprintf ("CcRqENDPAINT");
			} break;
		case CcRqGETDC: /* hwnd; HDC result. */
			{
				rprintf ("CcRqGETDC");
			} break;
		case CcRqRELEASEDC: 	/* hwnd, hdc; no result. */
			{
				rprintf ("CcRqRELEASEDC");
			} break;
		case CcRqINVALIDATEWINDOW:		/* hwnd; no result. */
			{
				rprintf ("CcRqINVALIDATEWINDOW");
			} break;
		case CcRqSETWINDOWTITLE:		/* hwnd, textptr; no result. */
			{
				rprintf ("CcRqSETWINDOWTITLE");
			} break;
		case CcRqGETWINDOWTEXT: /* hwnd; textptr result. */
			{
				rprintf ("CcRqGETWINDOWTEXT");
			} break;
		case CcRqGETCLIENTSIZE: /* hwnd; width, height result. */
			{
				rprintf ("CcRqGETCLIENTSIZE");
			} break;
		case CcRqGETWINDOWPOS:	/* hwnd; left, top result. */
			{
				rprintf ("CcRqGETWINDOWPOS");
			} break;
		case CcRqCHANGEWINDOWCURSOR:	/* hwnd, cursor code; no result. */
			{
				rprintf ("CcRqCHANGEWINDOWCURSOR");
			} break;
		case CcRqOBSCURECURSOR: /* no params; no result.  */
			{
				rprintf ("CcRqOBSCURECURSOR");
			} break;
		case CcRqSETGLOBALCURSOR:		/* cursorcode; no result.  */
			{
				rprintf ("CcRqSETGLOBALCURSOR");
			} break;
		case CcRqRESETCURSOR:	/* no params; no result. */
			{
				rprintf ("CcRqRESETCURSOR");
			} break;
		case CcRqGETFONTNAMES:	/* no params; no result. */
			{
				rprintf ("CcRqGETFONTNAMES");
			} break;
		case CcRqGETFONTSIZES:	/* textptr; no result. */
			{
				rprintf ("CcRqGETFONTSIZES");
			} break;
		case CcCbFONTNAME:		/* textptr; no result. */
			{
				rprintf ("CcCbFONTNAME");
			} break;
		case CcCbFONTSIZE:		/* size, isTrueType; no result. */
			{
				rprintf ("CcCbFONTSIZE");
			} break;
		case CcRqGETCURTIME:	/* no params; hours, minutes, seconds. */
			{
				rprintf ("CcRqGETCURTIME");
			} break;
		case CcRqGETCURDATE:	/* no params; year, month, day, weekday. */
			{
				rprintf ("CcRqGETCURDATE");
			} break;
		case CcRqWAIT:	/* milliseconds; no result. */
			{
				rprintf ("CcRqWAIT");
			} break;
		case CcRqGETBLINKTIME:	/* no params; millisec result. */
			{
				rprintf ("CcRqGETBLINKTIME");
			} break;
		case CcRqCREATEDIALOG:	/* textptr; HWND result. */
			{
				rprintf ("CcRqCREATEDIALOG");
			} break;
		case CcRqCREATEBUTTON:	/* hwnd, x,y,w,h, isdefbut; HWND result. */
			{
				rprintf ("CcRqCREATEBUTTON");
			} break;
		case CcRqCREATESTATICTXT:		/* hwnd, x,y,w,h; HWND result. */
			{
				rprintf ("CcRqCREATESTATICTXT");
			} break;
		case CcRqCREATEEDITTXT: /* hwnd, x,y,w,h, ismultiline; HWND
										   result. */
			{
				rprintf ("CcRqCREATEEDITTXT");
			} break;
		case CcRqCREATERADIOBUT:		/* hwnd, x,y,w,h, isselected; HWND
										   result. */
			{
				rprintf ("CcRqCREATERADIOBUT");
			} break;
		case CcRqCREATECHECKBOX:		/* hwnd, x,y,w,h, isselected; HWND
										   result. */
			{
				rprintf ("CcRqCREATECHECKBOX");
			} break;
		case CcRqCREATEPOPUP:	/* hwnd, x,y,w,h; HWND result. */
			{
				rprintf ("CcRqCREATEPOPUP");
			} break;
		case CcRqCREATEICONBUT: /* hwnd, x,y,w,h; HWND result. */
			{
				rprintf ("CcRqCREATEICONBUT");
			} break;
		case CcRqCREATECUSTOM:	/* hwnd, x,y,w,h; HWND result. */
			{
				rprintf ("CcRqCREATECUSTOM");
			} break;
		case CcRqENABLECONTROL: /* hwnd, bool; no result. */
			{
				rprintf ("CcRqENABLECONTROL");
			} break;
		case CcRqSETITEMCHECK:	/* hwnd, bool; no result. */
			{
				rprintf ("CcRqSETITEMCHECK");
			} break;
		case CcRqADDTOPOPUP:	/* hwnd, textptr, enabled, selected; Pos
								   result. */
			{
				rprintf ("CcRqADDTOPOPUP");
			} break;
		case CcRqENABLEPOPUPITEM:		/* hwnd, pos, enabled; no result. */
			{
				rprintf ("CcRqENABLEPOPUPITEM");
			} break;
		case CcRqSELECTPOPUPITEM:		/* hwnd, pos; no result. */
			{
				rprintf ("CcRqSELECTPOPUPITEM");
			} break;
		case CcRqFILEOPENDIALOG:		/* no params; bool, textptr result; */
			{
				rprintf ("CcRqFILEOPENDIALOG");
			} break;
		case CcRqFILESAVEDIALOG:		/* promptptr, nameptr; bool, textptr
										   result; */
			{
				rprintf ("CcRqFILESAVEDIALOG");
			} break;
		case CcRqSETCLIPBOARDTEXT:		/* textptr;   no result. */
			{
				rprintf ("CcRqSETCLIPBOARDTEXT");
			} break;
		case CcRqGETCLIPBOARDTEXT:		/* no params; textptr result. */
			{
				rprintf ("CcRqGETCLIPBOARDTEXT");
			} break;
		case CcRqCLIPBOARDHASTEXT:		/* no params; bool result. */
			{
				rprintf ("CcRqCLIPBOARDHASTEXT");
			} break;
		default:
			{
				rprintf ("Unknown CCI: %d", pcci->mess);
			} break;
	}
}	/* printCCI */

void printMessage (char *fname, HWND hWin, UINT uMess, WPARAM wPara, LPARAM lPara)
{
	switch (uMess)
	{
			case WM_ACTIVATE:
			{
				rprintf ("== %s got %s, hwnd = %d, ", fname, "WM_ACTIVATE", hWin);
				switch (LOWORD (wPara)) /* activation flag */
				{
					case WA_ACTIVE:
						rprintf ("fActive = WA_ACTIVE, ");
						break;
					case WA_CLICKACTIVE:
						rprintf ("fActive = WA_CLICKACTIVE, ");
						break;
					case WA_INACTIVE:
						rprintf ("fActive = WA_INACTIVE, ");
						break;
				}
				rprintf ("fMinimized = %s, ", BOOLstring ((BOOL) HIWORD (wPara)));		/* minimized flag  */
				rprintf ("other_hwnd = %d\n", lPara);	/* window handle  */
			} break;
		case WM_ACTIVATEAPP:
			{
				rprintf ("== %s got %s, hwnd = %d, fActive = %s, other_thread = %d\n", fname, "WM_ACTIVATEAPP", hWin, BOOLstring ((BOOL) wPara), lPara);
			} break;
		case WM_NCHITTEST:
			{
			} break;
		case WM_SETCURSOR:
			{
			} break;
		case WM_MOVE:
			{
				rprintf ("== %s got %s, hwnd = %d, x = %d, y = %d\n", fname, "WM_MOVE", hWin, LOWORD (lPara), HIWORD (lPara));
			} break;
		case WM_SIZE:
			{
				rprintf ("== %s got %s, hwnd = %d, wPara = ", fname, "WM_SIZE", hWin);
				switch (wPara)
				{
					case SIZE_MAXHIDE:
						rprintf ("SIZE_MAXHIDE");
						break;
					case SIZE_MAXIMIZED:
						rprintf ("SIZE_MAXIMIZED");
						break;
					case SIZE_MAXSHOW:
						rprintf ("SIZE_MAXSHOW");
						break;
					case SIZE_MINIMIZED:
						rprintf ("SIZE_MINIMIZED");
						break;
					case SIZE_RESTORED:
						rprintf ("SIZE_RESTORED");
						break;
					default:
						rprintf ("unknown");
						break;
				}
				rprintf (", width =%d, height = %d\n", LOWORD (lPara), HIWORD (lPara));
			} break;
		case WM_HSCROLL:
			{
				rprintf ("== %s got %s, hwnd = %d, ", fname, "WM_HSCROLL", hWin);
				switch ((int) LOWORD (wPara))
				{
					case SB_BOTTOM:
						rprintf ("scrollcode = SB_BOTTOM\n");
						break;
					case SB_ENDSCROLL:
						rprintf ("scrollcode = SB_ENDSCROLL\n");
						break;
					case SB_LINELEFT:
						rprintf ("scrollcode = SB_LINELEFT\n");
						break;
					case SB_LINERIGHT:
						rprintf ("scrollcode = SB_LINERIGHT\n");
						break;
					case SB_PAGELEFT:
						rprintf ("scrollcode = SB_PAGELEFT\n");
						break;
					case SB_PAGERIGHT:
						rprintf ("scrollcode = SB_PAGERIGHT\n");
						break;
					case SB_THUMBPOSITION:
						rprintf ("scrollcode = SB_THUMBPOSITION, nPos = %d\n", HIWORD (wPara));
						break;
					case SB_THUMBTRACK:
						rprintf ("scrollcode = SB_THUMBTRACK, nPos = %d\n", HIWORD (wPara));
						break;
					case SB_TOP:
						rprintf ("scrollcode = SB_TOP\n");
						break;
				}
			} break;
		case WM_VSCROLL:
			{
				rprintf ("== %s got %s, hwnd = %d, ", fname, "WM_VSCROLL", hWin);

				switch (LOWORD (wPara))
				{
					case SB_BOTTOM:
						rprintf ("scrollcode = SB_BOTTOM\n");
						break;
					case SB_ENDSCROLL:
						rprintf ("scrollcode = SB_ENDSCROLL\n");
						break;
					case SB_LINEDOWN:
						rprintf ("scrollcode = SB_LINEDOWN\n");
						break;
					case SB_LINEUP:
						rprintf ("scrollcode = SB_LINEUP\n");
						break;
					case SB_PAGEDOWN:
						rprintf ("scrollcode = SB_PAGEDOWN\n");
						break;
					case SB_PAGEUP:
						rprintf ("scrollcode = SB_PAGEUP\n");
						break;
					case SB_THUMBPOSITION:
						rprintf ("scrollcode = SB_THUMBPOSITION, nPos = %d\n", HIWORD (wPara));
						break;
					case SB_THUMBTRACK:
						rprintf ("scrollcode = SB_THUMBTRACK, nPos = %d\n", HIWORD (wPara));
						break;
					case SB_TOP:
						rprintf ("scrollcode = SB_TOP\n");
						break;
				}
			} break;
		case WM_TIMER:
			{	/* rprintf("== %s got %s, hwnd = %d, wParam = %d\n", fname,
				   "WM_TIMER", hWin, wPara); */
			} break;
		case WM_ENABLE:
			{
				rprintf ("== %s got %s, hwnd = %d, wParam = %s\n", fname, "WM_ENABLE", hWin, BOOLstring ((BOOL) wPara));
			} break;
		case WM_ENTERIDLE:
			{	/* rprintf("== %s got %s, hwnd = %d\n", fname,
				   "WM_ENTERIDLE", hWin); */
			} break;
		case WM_CHAR:
			{
				rprintf ("== %s got %s, hwnd = %d, char = \'%c\'[%d]\n", fname, "WM_CHAR", hWin, wPara, wPara);
			} break;
/*--------------------------------------------- */
		case WM_NULL:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NULL", hWin);
			} break;
		case WM_CREATE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CREATE", hWin);
			} break;
		case WM_DESTROY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DESTROY", hWin);
			} break;
		case WM_SETFOCUS:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SETFOCUS", hWin);
			} break;
		case WM_KILLFOCUS:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_KILLFOCUS", hWin);
			} break;
		case WM_SETREDRAW:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SETREDRAW", hWin);
			} break;
		case WM_SETTEXT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SETTEXT", hWin);
			} break;
		case WM_GETTEXT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_GETTEXT", hWin);
			} break;
		case WM_GETTEXTLENGTH:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_GETTEXTLENGTH", hWin);
			} break;
		case WM_PAINT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PAINT", hWin);
			} break;
		case WM_CLOSE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CLOSE", hWin);
			} break;
		case WM_QUERYENDSESSION:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_QUERYENDSESSION", hWin);
			} break;
		case WM_QUIT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_QUIT", hWin);
			} break;
		case WM_QUERYOPEN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_QUERYOPEN", hWin);
			} break;
		case WM_ERASEBKGND:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_ERASEBKGND", hWin);
			} break;
		case WM_SYSCOLORCHANGE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SYSCOLORCHANGE", hWin);
			} break;
		case WM_ENDSESSION:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_ENDSESSION", hWin);
			} break;
		case WM_SHOWWINDOW:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SHOWWINDOW", hWin);
			} break;
		case WM_SETTINGCHANGE:	/* WM_WININICHANGE on NT */
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SETTINGCHANGE", hWin);
			} break;
		case WM_DEVMODECHANGE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DEVMODECHANGE", hWin);
			} break;
		case WM_FONTCHANGE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_FONTCHANGE", hWin);
			} break;
		case WM_TIMECHANGE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_TIMECHANGE", hWin);
			} break;
		case WM_CANCELMODE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CANCELMODE", hWin);
			} break;
		case WM_MOUSEACTIVATE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MOUSEACTIVATE", hWin);
			} break;
		case WM_CHILDACTIVATE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CHILDACTIVATE", hWin);
			} break;
		case WM_QUEUESYNC:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_QUEUESYNC", hWin);
			} break;
		case WM_GETMINMAXINFO:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_GETMINMAXINFO", hWin);
			} break;
		case WM_PAINTICON:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PAINTICON", hWin);
			} break;
		case WM_ICONERASEBKGND:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_ICONERASEBKGND", hWin);
			} break;
		case WM_NEXTDLGCTL:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NEXTDLGCTL", hWin);
			} break;
		case WM_SPOOLERSTATUS:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SPOOLERSTATUS", hWin);
			} break;
		case WM_DRAWITEM:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DRAWITEM", hWin);
			} break;
		case WM_MEASUREITEM:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MEASUREITEM", hWin);
			} break;
		case WM_DELETEITEM:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DELETEITEM", hWin);
			} break;
		case WM_VKEYTOITEM:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_VKEYTOITEM", hWin);
			} break;
		case WM_CHARTOITEM:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CHARTOITEM", hWin);
			} break;
		case WM_SETFONT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SETFONT", hWin);
			} break;
		case WM_GETFONT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_GETFONT", hWin);
			} break;
		case WM_SETHOTKEY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SETHOTKEY", hWin);
			} break;
		case WM_GETHOTKEY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_GETHOTKEY", hWin);
			} break;
		case WM_QUERYDRAGICON:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_QUERYDRAGICON", hWin);
			} break;
		case WM_COMPAREITEM:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_COMPAREITEM", hWin);
			} break;
		case WM_COMPACTING:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_COMPACTING", hWin);
			} break;
		case WM_COMMNOTIFY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_COMMNOTIFY", hWin);
			} break;
		case WM_WINDOWPOSCHANGING:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_WINDOWPOSCHANGING", hWin);
			} break;
		case WM_WINDOWPOSCHANGED:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_WINDOWPOSCHANGED", hWin);
			} break;
		case WM_POWER:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_POWER", hWin);
			} break;
		case WM_COPYDATA:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_COPYDATA", hWin);
			} break;
		case WM_CANCELJOURNAL:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CANCELJOURNAL", hWin);
			} break;
		case WM_NOTIFY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NOTIFY", hWin);
			} break;
		case WM_INPUTLANGCHANGEREQUEST:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_INPUTLANGCHANGEREQUEST", hWin);
			} break;
		case WM_INPUTLANGCHANGE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_INPUTLANGCHANGE", hWin);
			} break;
		case WM_TCARD:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_TCARD", hWin);
			} break;
		case WM_HELP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_HELP", hWin);
			} break;
		case WM_USERCHANGED:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_USERCHANGED", hWin);
			} break;
		case WM_NOTIFYFORMAT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NOTIFYFORMAT", hWin);
			} break;
		case WM_CONTEXTMENU:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CONTEXTMENU", hWin);
			} break;
		case WM_STYLECHANGING:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_STYLECHANGING", hWin);
			} break;
		case WM_STYLECHANGED:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_STYLECHANGED", hWin);
			} break;
		case WM_DISPLAYCHANGE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DISPLAYCHANGE", hWin);
			} break;
		case WM_GETICON:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_GETICON", hWin);
			} break;
		case WM_SETICON:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SETICON", hWin);
			} break;
		case WM_NCCREATE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCCREATE", hWin);
			} break;
		case WM_NCDESTROY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCDESTROY", hWin);
			} break;
		case WM_NCCALCSIZE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCCALCSIZE", hWin);
			} break;
		case WM_NCPAINT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCPAINT", hWin);
			} break;
		case WM_NCACTIVATE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCACTIVATE", hWin);
			} break;
		case WM_GETDLGCODE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_GETDLGCODE", hWin);
			} break;
		case WM_NCMOUSEMOVE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCMOUSEMOVE", hWin);
			} break;
		case WM_NCLBUTTONDOWN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCLBUTTONDOWN", hWin);
			} break;
		case WM_NCLBUTTONUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCLBUTTONUP", hWin);
			} break;
		case WM_NCLBUTTONDBLCLK:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCLBUTTONDBLCLK", hWin);
			} break;
		case WM_NCRBUTTONDOWN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCRBUTTONDOWN", hWin);
			} break;
		case WM_NCRBUTTONUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCRBUTTONUP", hWin);
			} break;
		case WM_NCRBUTTONDBLCLK:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCRBUTTONDBLCLK", hWin);
			} break;
		case WM_NCMBUTTONDOWN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCMBUTTONDOWN", hWin);
			} break;
		case WM_NCMBUTTONUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCMBUTTONUP", hWin);
			} break;
		case WM_NCMBUTTONDBLCLK:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NCMBUTTONDBLCLK", hWin);
			} break;
		case WM_KEYDOWN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_KEYDOWN", hWin);
			} break;
		case WM_KEYUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_KEYUP", hWin);
			} break;
		case WM_DEADCHAR:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DEADCHAR", hWin);
			} break;
		case WM_SYSKEYDOWN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SYSKEYDOWN", hWin);
			} break;
		case WM_SYSKEYUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SYSKEYUP", hWin);
			} break;
		case WM_SYSCHAR:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SYSCHAR", hWin);
			} break;
		case WM_SYSDEADCHAR:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SYSDEADCHAR", hWin);
			} break;
		case WM_IME_STARTCOMPOSITION:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_STARTCOMPOSITION", hWin);
			} break;
		case WM_IME_ENDCOMPOSITION:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_ENDCOMPOSITION", hWin);
			} break;
		case WM_IME_COMPOSITION:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_COMPOSITION", hWin);
			} break;
		case WM_INITDIALOG:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_INITDIALOG", hWin);
			} break;
		case WM_COMMAND:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_COMMAND", hWin);
			} break;
		case WM_SYSCOMMAND:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SYSCOMMAND", hWin);
			} break;
		case WM_INITMENU:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_INITMENU", hWin);
			} break;
		case WM_INITMENUPOPUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_INITMENUPOPUP", hWin);
			} break;
		case WM_MENUSELECT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MENUSELECT", hWin);
			} break;
		case WM_MENUCHAR:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MENUCHAR", hWin);
			} break;
		case WM_CTLCOLORMSGBOX:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CTLCOLORMSGBOX", hWin);
			} break;
		case WM_CTLCOLOREDIT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CTLCOLOREDIT", hWin);
			} break;
		case WM_CTLCOLORLISTBOX:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CTLCOLORLISTBOX", hWin);
			} break;
		case WM_CTLCOLORBTN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CTLCOLORBTN", hWin);
			} break;
		case WM_CTLCOLORDLG:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CTLCOLORDLG", hWin);
			} break;
		case WM_CTLCOLORSCROLLBAR:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CTLCOLORSCROLLBAR", hWin);
			} break;
		case WM_CTLCOLORSTATIC:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CTLCOLORSTATIC", hWin);
			} break;
		case WM_MOUSEMOVE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MOUSEMOVE", hWin);
			} break;
		case WM_LBUTTONDOWN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_LBUTTONDOWN", hWin);
			} break;
		case WM_LBUTTONUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_LBUTTONUP", hWin);
			} break;
		case WM_LBUTTONDBLCLK:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_LBUTTONDBLCLK", hWin);
			} break;
		case WM_RBUTTONDOWN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_RBUTTONDOWN", hWin);
			} break;
		case WM_RBUTTONUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_RBUTTONUP", hWin);
			} break;
		case WM_RBUTTONDBLCLK:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_RBUTTONDBLCLK", hWin);
			} break;
		case WM_MBUTTONDOWN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MBUTTONDOWN", hWin);
			} break;
		case WM_MBUTTONUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MBUTTONUP", hWin);
			} break;
		case WM_MBUTTONDBLCLK:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MBUTTONDBLCLK", hWin);
			} break;
		case WM_PARENTNOTIFY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PARENTNOTIFY", hWin);
			} break;
		case WM_ENTERMENULOOP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_ENTERMENULOOP", hWin);
			} break;
		case WM_EXITMENULOOP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_EXITMENULOOP", hWin);
			} break;
		case WM_NEXTMENU:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_NEXTMENU", hWin);
			} break;
		case WM_SIZING:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SIZING", hWin);
			} break;
		case WM_CAPTURECHANGED:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CAPTURECHANGED", hWin);
			} break;
		case WM_MOVING:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MOVING", hWin);
			} break;
		case WM_POWERBROADCAST:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_POWERBROADCAST", hWin);
			} break;
		case WM_DEVICECHANGE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DEVICECHANGE", hWin);
			} break;
		case WM_IME_SETCONTEXT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_SETCONTEXT", hWin);
			} break;
		case WM_IME_NOTIFY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_NOTIFY", hWin);
			} break;
		case WM_IME_CONTROL:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_CONTROL", hWin);
			} break;
		case WM_IME_COMPOSITIONFULL:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_COMPOSITIONFULL", hWin);
			} break;
		case WM_IME_SELECT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_SELECT", hWin);
			} break;
		case WM_IME_CHAR:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_CHAR", hWin);
			} break;
		case WM_IME_KEYDOWN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_KEYDOWN", hWin);
			} break;
		case WM_IME_KEYUP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_IME_KEYUP", hWin);
			} break;
		case WM_MDICREATE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDICREATE", hWin);
			} break;
		case WM_MDIDESTROY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDIDESTROY", hWin);
			} break;
		case WM_MDIACTIVATE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDIACTIVATE", hWin);
			} break;
		case WM_MDIRESTORE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDIRESTORE", hWin);
			} break;
		case WM_MDINEXT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDINEXT", hWin);
			} break;
		case WM_MDIMAXIMIZE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDIMAXIMIZE", hWin);
			} break;
		case WM_MDITILE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDITILE", hWin);
			} break;
		case WM_MDICASCADE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDICASCADE", hWin);
			} break;
		case WM_MDIICONARRANGE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDIICONARRANGE", hWin);
			} break;
		case WM_MDIGETACTIVE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDIGETACTIVE", hWin);
			} break;
		case WM_MDISETMENU:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDISETMENU", hWin);
			} break;
		case WM_ENTERSIZEMOVE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_ENTERSIZEMOVE", hWin);
			} break;
		case WM_EXITSIZEMOVE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_EXITSIZEMOVE", hWin);
			} break;
		case WM_DROPFILES:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DROPFILES", hWin);
			} break;
		case WM_MDIREFRESHMENU:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_MDIREFRESHMENU", hWin);
			} break;
		case WM_CUT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CUT", hWin);
			} break;
		case WM_COPY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_COPY", hWin);
			} break;
		case WM_PASTE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PASTE", hWin);
			} break;
		case WM_CLEAR:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CLEAR", hWin);
			} break;
		case WM_UNDO:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_UNDO", hWin);
			} break;
		case WM_RENDERFORMAT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_RENDERFORMAT", hWin);
			} break;
		case WM_RENDERALLFORMATS:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_RENDERALLFORMATS", hWin);
			} break;
		case WM_DESTROYCLIPBOARD:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DESTROYCLIPBOARD", hWin);
			} break;
		case WM_DRAWCLIPBOARD:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_DRAWCLIPBOARD", hWin);
			} break;
		case WM_PAINTCLIPBOARD:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PAINTCLIPBOARD", hWin);
			} break;
		case WM_VSCROLLCLIPBOARD:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_VSCROLLCLIPBOARD", hWin);
			} break;
		case WM_SIZECLIPBOARD:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_SIZECLIPBOARD", hWin);
			} break;
		case WM_ASKCBFORMATNAME:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_ASKCBFORMATNAME", hWin);
			} break;
		case WM_CHANGECBCHAIN:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_CHANGECBCHAIN", hWin);
			} break;
		case WM_HSCROLLCLIPBOARD:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_HSCROLLCLIPBOARD", hWin);
			} break;
		case WM_QUERYNEWPALETTE:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_QUERYNEWPALETTE", hWin);
			} break;
		case WM_PALETTEISCHANGING:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PALETTEISCHANGING", hWin);
			} break;
		case WM_PALETTECHANGED:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PALETTECHANGED", hWin);
			} break;
		case WM_HOTKEY:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_HOTKEY", hWin);
			} break;
		case WM_PRINT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PRINT", hWin);
			} break;
		case WM_PRINTCLIENT:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PRINTCLIENT", hWin);
			} break;
		case WM_HANDHELDFIRST:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_HANDHELDFIRST", hWin);
			} break;
		case WM_HANDHELDLAST:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_HANDHELDLAST", hWin);
			} break;
		case WM_AFXFIRST:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_AFXFIRST", hWin);
			} break;
		case WM_AFXLAST:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_AFXLAST", hWin);
			} break;
		case WM_PENWINFIRST:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PENWINFIRST", hWin);
			} break;
		case WM_PENWINLAST:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_PENWINLAST", hWin);
			} break;
		case WM_APP:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_APP", hWin);
			} break;
		case WM_USER:
			{
				rprintf ("== %s got %s, hwnd = %d\n", fname, "WM_USER", hWin);
			} break;
		default:
			{
				rprintf ("== %s got UNKOWN MESSAGE %d, hwin = %d\n", fname, uMess, hWin);
			} break;
	}
}	/* printMessage */

#endif
