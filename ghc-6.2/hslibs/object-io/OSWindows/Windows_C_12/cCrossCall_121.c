/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	cCrossCall_121 defines the infrastructure required by the Object I/O library to call
	system procedures that interact with the Windows callback mechanism.

	The basic principle in cCrossCall_121 is to have a minimal cross call kernel. If Clean
	code requires extension of the functionality of the OS thread, then this functionality
	must be	registered before being applicable.

	In this version the request codes are still statically fixed and are assumed to be
	globally available both in the OS thread and the Clean thread. In a future version this
	will probably be replaced by a dynamic allocation of cross call request codes.
********************************************************************************************/


/********************************************************************************************
	Include section.
********************************************************************************************/

#include "cCrossCall_121.h"
#include "cAcceleratorTable_121.h"					// Contains the implementation of accelerator tables.
#include "cCrossCallCursor_121.h"					// Contains the implementation of cursors.
#include <commctrl.h>


/**********************************************************************************************
	External global data section.
**********************************************************************************************/
CrossCallInfo gCci;									/* The global cross call information struct. */
char *gAppName;										/* The application name. */
HINSTANCE ghInst;									/* The handle to the instance of the OS thread. */
HWND ghMainWindow                = NULL;			/* The handle to the main HWND of the OS thread. */
HACCEL gAcceleratorTable         = NULL;			/* Refers to the accelerator table of the active frame. */
BOOL gAcceleratorTableIsUpToDate = TRUE;			/* Flag: TRUE iff accelerator table corresponds with active frame. */
HWND ghActiveFrameWindow         = NULL;			/* The currently active frame window (MDI/SDI). */
HWND ghActiveClientWindow        = NULL;			/* The currently active client window (MDI). */
HWND gActiveDialog               = NULL;			/* The currently active dialogue. */
HWND ghwndLastModalDialog        = NULL;			/* Keeps track of last modal dialog. */
HFONT gDlogFont                  = NULL;			/* The handle to the logical FONT that is used in dialogs. */
HFONT gControlFont;									/* The handle to the logical FONT that is used in all controls. */
HWND ghwndTT                     = NULL;			/* The tooltip control. */
HWND ghTCPWindow                 = NULL;			/* The handle to the TCP HWND of the OS thread. */
int gClipboardCount              = 0;				/* Keeps track of changes of clipboard. */
CrossCallProcedureTable gCrossCallProcedureTable;

/**********************************************************************************************
	Internal global data section.
**********************************************************************************************/

static BOOL gAppRunning          = FALSE;
static HANDLE gCLEAN_DONE;
static HANDLE gOS_DONE;
static DWORD gCleanThreadId;
static DWORD gOsThreadId;
static HANDLE ghOSThreadHandle   = NULL;
static HWND gNextClipboardViewer = NULL;		/* PA: stores next clipboardviewer. */
static LONG gPrevMsgTime         = -1;
static BOOL gMessStored          = FALSE;
static MSG gStoredMess;

static CrossCallInfo *MakeQuitCci (CrossCallInfo * pcci);


/*	Menu(item)IDs are not allowed to exceed OSMenuIDEnd.
	This is because window ids start at (OSMenuIDEnd+5), and need to be distinct from menu ids
	in case of MDI processes.
	The global gMenuItemID (initially 0) is incremented by NextMenuItemID each time a new
	menu(item)ID is required.
	This implementation does not reuse freed ids and is therefore not adequate!!
*/
static int gMenuItemID = 0;

UINT NextMenuItemID (void)
{
	if (gMenuItemID>=OSMenuIDEnd)
		ErrorExit ("NextMenuItemID exceeded number of internal menu(item)IDs: %d",OSMenuIDEnd);

	gMenuItemID++;

	return gMenuItemID;
}


/*	GetModifiers returns the modifiers that are currently pressed.
*/
int GetModifiers (void)
{
	int mods;
	mods = 0;

	if (GetKeyState (VK_SHIFT) < 0)
		mods |= SHIFTBIT;
	if (GetKeyState (VK_CONTROL) < 0)
		mods |= CTRLBIT;
	if (GetKeyState (VK_MENU) < 0)
		mods |= ALTBIT;

	return mods;
}


/*	Translate virtual key codes to the codes shared with Clean.
	This procedure has been filtered from TranslateKeyboardMessage.
	If the keycode could not be translated, zero is returned.
*/
int CheckVirtualKeyCode (int keycode)
{
	int c = 0;
	switch (keycode)
	{
		case VK_UP:
			c = WinUpKey;
			break;
		case VK_DOWN:
			c = WinDownKey;
			break;
		case VK_LEFT:
			c = WinLeftKey;
			break;
		case VK_RIGHT:
			c = WinRightKey;
			break;
		case VK_PRIOR:
			c = WinPgUpKey;
			break;
		case VK_NEXT:
			c = WinPgDownKey;
			break;
		case VK_END:
			c = WinEndKey;
			break;
		case VK_HOME:
			c = WinBeginKey;
			break;
		case VK_BACK:
			c = WinBackSpKey;
			break;
		case VK_DELETE:
			c = WinDelKey;
			break;
		case VK_TAB:
			c = WinTabKey;
			break;
		case VK_RETURN:
			c = WinReturnKey;
			break;
		case VK_ESCAPE:
			c = WinEscapeKey;
			break;
		case VK_HELP:
			c = WinHelpKey;
			break;
		case VK_F1:
			c = WinF1Key;
			break;
		case VK_F2:
			c = WinF2Key;
			break;
		case VK_F3:
			c = WinF3Key;
			break;
		case VK_F4:
			c = WinF4Key;
			break;
		case VK_F5:
			c = WinF5Key;
			break;
		case VK_F6:
			c = WinF6Key;
			break;
		case VK_F7:
			c = WinF7Key;
			break;
		case VK_F8:
			c = WinF8Key;
			break;
		case VK_F9:
			c = WinF9Key;
			break;
		case VK_F10:
			c = WinF10Key;
			break;
		case VK_F11:
			c = WinF11Key;
			break;
		case VK_F12:
			c = WinF12Key;
			break;
	}
	return c;
}


/*	EndSuspendTimerProc is the parameter of the SetTimer procedure used
	in GetMessageQuickly to set a positive timer interval.
	The WaitMessage routine suspends the OS thread until an interesting
	event has occurred or the timer has been triggered. In that case
	EndSuspendTimerProc is called, which kills the timer and informs
	Clean about the timer event.
*/
static VOID CALLBACK EndSuspendTimerProc (HWND hwnd, UINT uMsg, UINT idEvent, DWORD dwTime )
{
	KillTimer (ghMainWindow, (UINT) -2);
	SendMessage0ToClean (CcWmIDLETIMER);
};

static int GetMessageQuickly (BOOL gIdleTimerOn, int gSleeptime, MSG * pmsg)
{
	if (gMessStored)
	{
		*pmsg = gStoredMess;
		gMessStored = FALSE;
		return TRUE;
	}

	if (gSleeptime==0 && !PeekMessage (pmsg, NULL, 0, 0, PM_NOREMOVE))
	{
		POINT p;
		GetCursorPos (&p);
		pmsg->hwnd    = ghMainWindow;
		pmsg->message = WM_ENTERIDLE;
		pmsg->wParam  = MSGF_USER;
		pmsg->lParam  = (LPARAM) ghMainWindow;
		pmsg->time    = GetTickCount ();
		pmsg->pt      = p;
		return TRUE;
	}
	if (!gIdleTimerOn)
	{
		return GetMessage (pmsg, NULL, 0, 0);
	}
	else
	{
		if (PeekMessage (pmsg, NULL, 0, 0, PM_REMOVE))
			return (pmsg->message != WM_QUIT);
		else
		{
			POINT p;

			/*	The following code has been inserted to reduce the crosscall traffic.
				A timer is set to suspend this thread until atleast the timer interval
				has elapsed.
			*/
			if (SetTimer (ghMainWindow, (UINT) -2, (UINT)gSleeptime, &EndSuspendTimerProc))
			{
				WaitMessage ();
			}
			else
			{
				rMessageBox (NULL,MB_APPLMODAL,"GetMessageQuickly","SetTimer failed to create timer");
			}
			/*	End of insertion.
			*/

			GetCursorPos (&p);

			pmsg->hwnd    = ghMainWindow;
			pmsg->message = WM_ENTERIDLE;
			pmsg->wParam  = MSGF_USER;
			pmsg->lParam  = (LPARAM) ghMainWindow;
			pmsg->time    = GetTickCount ();
			pmsg->pt      = p;
			return TRUE;
		}
	}
}	/* GetMessageQuickly */



//	TranslateKeyboardMessage is used by HandleCleanRequest only.
static BOOL TranslateKeyboardMessage (MSG * pmsg)
{
	int c;
	UINT msg;

	c   = 0;
	msg = pmsg->message;
	if (msg==WM_SYSKEYDOWN || msg==WM_KEYDOWN)
	{
		c = CheckVirtualKeyCode ((int) pmsg->wParam);
	}
	if (c)
	{
		gStoredMess = *pmsg;
		gStoredMess.wParam = (WPARAM) c;

		if (pmsg->message == WM_SYSKEYDOWN)
			gStoredMess.message = WM_SYSCHAR;
		else
			gStoredMess.message = WM_CHAR;

		gMessStored = TRUE;
		return TRUE;
	}
	else
		return TranslateMessage (pmsg);
}	/* TranslateKeyboardMessage */

void HandleCleanRequest (CrossCallInfo * pcci)
{
	switch (pcci->mess)
	{
		case CcRqDOMESSAGE: 	// idleTimerOn, sleeptime; no result.
			{
				MSG ms;
				int msgresult;

				msgresult  = GetMessageQuickly ((BOOL)pcci->p1, (int)pcci->p2, &ms);

				if (msgresult == -1)
					ErrorExit ("Fatal error: CcRqDoMessage, GetMessage result is -1");
				else if (msgresult == FALSE)
					ErrorExit ("Fatal error: CcRqDoMessage, GetMessage result is FALSE (WM_QUIT)");
				else
				{
					LONG msgtime = GetMessageTime ();

					gPrevMsgTime = msgtime;

					if (!gActiveDialog || !IsDialogMessage (gActiveDialog, &ms))
					{
						if (ghActiveClientWindow==NULL || !TranslateMDISysAccel (ghActiveClientWindow, &ms))
						{
							if (!gAcceleratorTableIsUpToDate)
							{	// Verify the correctness of gAcceleratorTable
								gAcceleratorTable = UpdateAcceleratorTable (gAcceleratorTable,ghActiveFrameWindow);
							}

							if (gAcceleratorTable==NULL || !TranslateAccelerator (ghActiveFrameWindow, gAcceleratorTable, &ms))
							{
								{
									TranslateKeyboardMessage (&ms);
									DispatchMessage (&ms);
								}
							}
						}
					}
					MakeReturn0Cci (pcci);
				}
			}
			break;
		default:
			{
				CrossCallProcedure action;

				action = FindCrossCallEntry (gCrossCallProcedureTable, pcci->mess);

				if (action == NULL)
				{	// Cross call request code not installed.
					ErrorExit ("\'HandleCleanRequest\' got uninstalled CcRq request code from Clean: %d\n", pcci->mess);
				}
				else
				{	// Cross call request code found. Apply it to pcci.
					action (pcci);
				}
			}
	}
	KickCleanThread (pcci);
}	/* HandleCleanRequest */


BOOL CleanThreadRunning (void)
{
	return GetCurrentThreadId () == gCleanThreadId;
}

BOOL OsThreadRunning (void)
{
	return GetCurrentThreadId () == gOsThreadId;
}

/*	InitGlobals is used by WinStartOSThread only. */
static void InitGlobals (void)
{
	LOGFONT lf;

	//	Globally, we create a logical font that is used in all controls.
	SetLogFontData (&lf, "MS Sans Serif", 0, 8);
	gControlFont = CreateFontIndirect (&lf);

	//	The cross call procedure table is set to the empty table.
	gCrossCallProcedureTable = EmptyCrossCallProcedureTable ();
}	/* InitGlobals */

void WinStartOsThread ()
{
	rprintf ("WSOT: Started\n");

	InitGlobals ();
	rprintf ("WSOT: Globals Initialised\n");
	gCLEAN_DONE = CreateEvent (NULL,	/* Default security attributes	*/
							   FALSE,	/* Not a manual-reset event 	*/
							   FALSE,	/* Initial state nonsignalled	*/
							   NULL);	/* No name						*/
	Check ((BOOL) gCLEAN_DONE, "\'InitOs\' could not create first event object");
	rprintf ("WSOT: CLEANDONE event created\n");

	gOS_DONE = CreateEvent (NULL, FALSE, FALSE, NULL);
	Check ((BOOL) gOS_DONE, "\'InitOs\' could not create second event object");
	rprintf ("WSOT: OS_DONE event created\n");

	gCleanThreadId = GetCurrentThreadId ();
	rprintf ("WSOT: gor current thread id\n");

	ghOSThreadHandle = CreateThread (NULL,				/* Default security attributes		*/
									 0,					/* Default stacksize				*/
								  (LPTHREAD_START_ROUTINE) OsThreadFunction,
									 0,					/* parameter to thread function		*/
									 0,					/* Not initially suspended			*/
									 &(gOsThreadId));	/* store threadId here				*/
	Check ((BOOL) ghOSThreadHandle, "\'InitOs\' could not create second thread");	// PA!!! test fails
	rprintf ("WSOT: new thread created\n");
	WaitForSingleObject (gOS_DONE, INFINITE);
	rprintf ("WSOT: wait done\n");
}	/* WinStartOSThread */

void WinKillOsThread ()
{
	rprintf ("void WinKillOsThread ()\n");
	if (ghOSThreadHandle != NULL)
	{
		rprintf ("ghOSThreadHandle != NULL\n");
		TerminateThread (ghOSThreadHandle, 0);
		rprintf ("		TerminateThread(ghOSThreadHandle, 0);\n");
		ghOSThreadHandle = NULL;
		rprintf ("		ghOSThreadHandle = NULL;\n");

		/* CleanUp */
		if (gAcceleratorTable)
			DestroyAcceleratorTable (gAcceleratorTable);
		rprintf ("		if (gAcceleratorTable) \n");
		rprintf ("				DestroyAcceleratorTable( gAcceleratorTable );\n");
		DeleteCursors ();
		rprintf ("		DeleteCursors();\n");
		CloseHandle (gOS_DONE);
		gOS_DONE = NULL;
		rprintf ("		CloseHandle(gOS_DONE);	  gOS_DONE	  = NULL;\n");
		CloseHandle (gCLEAN_DONE);
		gCLEAN_DONE = NULL;
		rprintf ("		CloseHandle(gCLEAN_DONE); gCLEAN_DONE = NULL;\n");
		if (gDlogFont != NULL)
			DeleteObject (gDlogFont);
		rprintf ("		if(gDlogFont != NULL)\n");
		rprintf ("		   DeleteObject(gDlogFont);\n");

		DeleteObject (gControlFont);	// The global logical font must be deleted.
		if (gCrossCallProcedureTable)
			FreeCrossCallProcedureTable (gCrossCallProcedureTable);
	};
// MW...
	ghMainWindow = NULL;
// ... MW
}	/* WinKillOsThread*/

void WinKickOsThread (int imess,
					  int ip1, int ip2, int ip3,
					  int ip4, int ip5, int ip6,
					  int *omess,
					  int *op1, int *op2, int *op3,
					  int *op4, int *op5, int *op6
					 )
{
/*	rprintf("KOT: filling in Cci\n"); */
	rprintf ("WinKickOsThread (");
	printCCI (&gCci);
	rprintf (")\n");

	gCci.mess = imess;
	gCci.p1 = ip1;
	gCci.p2 = ip2;
	gCci.p3 = ip3;
	gCci.p4 = ip4;
	gCci.p5 = ip5;
	gCci.p6 = ip6;

	if (ghOSThreadHandle != NULL)
	{
		rprintf ("KOT: Cci filled in, setting event\n");
		SetEvent (gCLEAN_DONE);
		rprintf ("KOT: Event set, start wait.\n");
		WaitForSingleObject (gOS_DONE, INFINITE);
		rprintf ("KOT: wait done, reading out Cci.\n");
		*omess = gCci.mess;
		*op1 = gCci.p1;
		*op2 = gCci.p2;
		*op3 = gCci.p3;
		*op4 = gCci.p4;
		*op5 = gCci.p5;
		*op6 = gCci.p6;
		rprintf ("KOT: Cci read: {");
		printCCI (&gCci);
		rprintf ("}\n");
	}
	else
	{
		rprintf ("KOT: no thread existed, returning CcWASQUIT for <");
		printCCI (&gCci);
		rprintf (">\n");
		*omess = CcWASQUIT;
		*op1 = 0;
		*op2 = 0;
		*op3 = 0;
		*op4 = 0;
		*op5 = 0;
		*op6 = 0;
	}

	rprintf ("KOT: done.\n");
}	/* WinKickOsThread */

#define PRINTCROSSCALLS

#ifdef PRINTCROSSCALLS
static CrossCallInfo osstack[10];
static CrossCallInfo clstack[10];
static int ossp = -1;
static int clsp = -1;
#endif

void KickCleanThread (CrossCallInfo * pcci)
{
#ifdef PRINTCROSSCALLS
	if (ossp == -1)
	{
		for (ossp = 0; ossp < 10; ossp++)
		{
			osstack[ossp].mess = -1;
		}
		ossp = 1;
		osstack[ossp].mess = -2;
	}

	if (clsp == -1)
	{
		for (clsp = 0; clsp < 10; clsp++)
		{
			clstack[clsp].mess = -1;
		}
		clsp = 1;
		clstack[clsp].mess = -2;
	}
#endif

	if (pcci != &gCci)
		gCci = *pcci;
	rprintf ("KCT: started\n");

#ifdef PRINTCROSSCALLS
	if (gCci.mess < 20)
	{
		rprintf ("	-- %d --> OS returning <", clsp + ossp - 2);
		printCCI (&gCci);
		rprintf ("> from <");
		printCCI (&(clstack[clsp]));
		rprintf (">\n");
		clsp--;
	}
	else
	{
		ossp++;
		osstack[ossp] = gCci;
		rprintf ("	-- %d --> OS calling with <", clsp + ossp - 2);
		printCCI (&gCci);
		rprintf (">\n");
	}
#endif

	rprintf ("KCT: setting event\n");
	SetEvent (gOS_DONE);
	rprintf ("KCT: starting wait\n");
	WaitForSingleObject (gCLEAN_DONE, INFINITE);
	rprintf ("KCT: wait done.\n");

	if (pcci != &gCci)
		*pcci = gCci;

#ifdef PRINTCROSSCALLS
	if (gCci.mess < 20)
	{
		rprintf (" <-- %d --  Clean returning <", clsp + ossp - 2);
		printCCI (&gCci);
		rprintf ("> from <");
		printCCI (&(osstack[ossp]));
		rprintf (">\n");
		ossp--;
	}
	else
	{
		clsp++;
		clstack[clsp] = gCci;
		rprintf (" <-- %d --  Clean calling with <", clsp + ossp - 2);
		printCCI (&gCci);
		rprintf (">\n");
	}
#endif
}	/* KickCleanThread */

void SendMessageToClean (int mess, int p1, int p2, int p3, int p4, int p5, int p6)
{
	gCci.mess = mess;
	gCci.p1 = p1;
	gCci.p2 = p2;
	gCci.p3 = p3;
	gCci.p4 = p4;
	gCci.p5 = p5;
	gCci.p6 = p6;

	KickCleanThread (&gCci);
	while (!IsReturnCci (&gCci))
	{
		HandleCleanRequest (&gCci);
	}
}

CrossCallInfo *MakeReturn0Cci (CrossCallInfo * pcci)
{
	pcci->mess = CcRETURN0;
	return pcci;
}

CrossCallInfo *MakeReturn1Cci (CrossCallInfo * pcci, int v1)
{
	pcci->mess = CcRETURN1;
	pcci->p1 = v1;
	return pcci;
}

CrossCallInfo *MakeReturn2Cci (CrossCallInfo * pcci, int v1, int v2)
{
	pcci->mess = CcRETURN2;
	pcci->p1 = v1;
	pcci->p2 = v2;
	return pcci;
}

CrossCallInfo *MakeReturn3Cci (CrossCallInfo * pcci, int v1, int v2, int v3)
{
	pcci->mess = CcRETURN3;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	return pcci;
}

CrossCallInfo *MakeReturn4Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4)
{
	pcci->mess = CcRETURN4;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	return pcci;
}

CrossCallInfo *MakeReturn5Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5)
{
	pcci->mess = CcRETURN5;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	pcci->p5 = v5;
	return pcci;
}

CrossCallInfo *MakeReturn6Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5, int v6)
{
	pcci->mess = CcRETURN6;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	pcci->p5 = v5;
	pcci->p6 = v6;
	return pcci;
}

BOOL IsReturnCci (CrossCallInfo * pcci)
{
	if (pcci->mess >= CcRETURNmin && pcci->mess <= CcRETURNmax)
		return TRUE;

	return FALSE;
}


extern double c_div_real (double n, double d);
extern int c_ftoi (double d);

//	GetAppFileName used by OsThreadFunction only.
static void GetAppFileName (void)
{
	char path[MAX_PATH + 1];
	int length, index;
	int start, end;
	BOOL newword;

	length = GetModuleFileName (NULL, path, MAX_PATH);

	for (index = length - 1; path[index] != '.'; index--)
		;
	end = index - 1;

	for (index = end;
		 path[index] != '/' &&
		 path[index] != '\\' &&
		 path[index] != ':';
		 index--)
		;

	start = index + 1;

	if (end - start > 31)
		end = start + 31;

	if (end - start >= 0)
	{
		gAppName = rmalloc (end - start + 2);
		for (index = 0; index <= end - start; index++)
			gAppName[index] = path[start + index];
		gAppName[index] = '\0';
	}
	else
	{
		gAppName = "Clean Application";
	}

	newword = TRUE;
	for (index = 0; gAppName[index] != '\0'; index++)
	{
		if (gAppName[index] >= 'A' && gAppName[index] <= 'Z' && !newword)
			gAppName[index] = gAppName[index] - ('A' - 'a');

		if (gAppName[index] == ' ')
			newword = TRUE;
		else
			newword = FALSE;
	}
}

/*	Registered Windows main window class name.
*/
static char MainWindowClassName[] = "__CleanMainWindow";	/* Class for main window (ghMainWindow). */


/*	The callback routine for the main window.
	PA: The WM_CREATE  message registers the main window as a clipboard viewer.
		The WM_DESTROY message unregisters the main window.
*/
static LRESULT CALLBACK MainWindowProcedure (HWND hWin, UINT uMess, WPARAM wPara, LPARAM lPara)
{
	printMessage ("Main Window", hWin, uMess, wPara, lPara);
	switch (uMess)
	{
		case WM_NCPAINT:
			break;
		case WM_ACTIVATEAPP:
			{
				if (wPara)
				{
					gAppRunning = TRUE;
				}
				else
				{
					gAppRunning = FALSE;
				}
			}
			break;
		/*	WM_ENTERIDLE message is used to let Clean evaluate the initialisation action
			of a modal dialog by sending the CcWmIDLEDIALOG message.
		*/
		case WM_ENTERIDLE:
			{
				HWND hwndModalDialog;

				hwndModalDialog = (HWND)lPara;

				if (wPara == MSGF_DIALOGBOX && hwndModalDialog != ghwndLastModalDialog)
				{
					SendMessage1ToClean (CcWmIDLEDIALOG,(int)hwndModalDialog);
					ghwndLastModalDialog = hwndModalDialog;
				}
				else
				{
					SendMessage0ToClean (CcWmIDLETIMER);
				}
				return 0;
			}
			break;
		case WM_TIMER:
			{
				SendMessage2ToClean (CcWmTIMER, wPara, GetMessageTime ());
			}
			break;
		case WM_ENABLE:
			{
				HWND hwin;
				char title[64];

				hwin = GetWindow (ghMainWindow, GW_HWNDFIRST);
				while (hwin != NULL)
				{
					GetWindowText (hwin, title, 63);

					if (GetWindow (hwin, GW_OWNER) == ghMainWindow)
					{
						RECT r;
						GetWindowRect (hwin, &r);
						if (r.top != -1 || r.left != -1 || r.right != 0 || r.bottom != 0)
						{
							EnableWindow (hwin, (BOOL) wPara);
						}
					}
					hwin = GetWindow (hwin, GW_HWNDNEXT);
				}
			}
			break;
		/* The WM_CREATE message registers the ghMainWindow (hWin) as a clipboard viewer.
		*/
		case WM_CREATE:
			{
				gNextClipboardViewer = SetClipboardViewer (hWin);
			}
			break;
		/* The WM_DESTROY message unregisters the ghMainWindow (hWin) as a clipboard viewer.
		*/
		case WM_DESTROY:
			{
				ChangeClipboardChain (hWin, gNextClipboardViewer);
			}
			break;
		/* Other clipboard chain management messages:
		*/
		case WM_DRAWCLIPBOARD:
			{
				gClipboardCount += 1;
				if (gNextClipboardViewer != NULL)
					SendMessage (gNextClipboardViewer, uMess, wPara, lPara);

				return 0;
			}
			break;
		case WM_CHANGECBCHAIN:
			{
				if ((HWND)wPara == gNextClipboardViewer)	/* gNextClipboardViewer is no longer a clipboard viewer. */
					gNextClipboardViewer = (HWND)lPara;		/*	then store the new viewer. */
				else if (gNextClipboardViewer != NULL)
					SendMessage (gNextClipboardViewer, uMess, wPara, lPara);

				return 0;
			}
			break;
		/*	PM_SOCKET_EVENT and PM_DNS_EVENT are intercepted by MainWindowProcedure.
			If ghTCPWindow != NULL, then these messages are passed on to ghTCPWindow.
		*/
		case PM_SOCKET_EVENT:
		case PM_DNS_EVENT:
			{
				if (ghTCPWindow != NULL)
					SendMessage (ghTCPWindow, uMess, wPara, lPara);

				return 0;
			}
			break;
		case WM_DDE_INITIATE:
			{
				static char apptext[256], topictext[256];
				ATOM aApp, aTopic;
/* RWS ... */
				BOOL handleTopic;
/* ... RWS */
				GlobalGetAtomName (HIWORD (lPara), topictext, 256);

				if (lstrcmp (topictext, "CLEANOPEN") == 0)
/* RWS: compare application name */
				{
					GlobalGetAtomName (LOWORD (lPara), apptext, 256);
					handleTopic	= CompareStringA (LOCALE_USER_DEFAULT, NORM_IGNORECASE,
									apptext, lstrlen (apptext), gAppName, lstrlen (gAppName)) == 2;	/* 2 means they are equal */
				}
				else
					handleTopic	= FALSE;

				if (handleTopic)
				{
/* ... RWS */
					aApp = GlobalAddAtom (apptext);
					aTopic = GlobalAddAtom (topictext);
					SendMessage ((HWND) wPara, WM_DDE_ACK, (WPARAM) hWin, MAKELONG (aApp, aTopic));
					GlobalDeleteAtom (aApp);
					GlobalDeleteAtom (aTopic);
				}
				else
				{
					return DefWindowProc (hWin, uMess, wPara, lPara);
				}
			}
			break;
		case WM_DDE_EXECUTE:
			{
				char *commandstring;
				char *pcommand;
				int len;
				union
				{
					DDEACK ddeack;
					WORD w;
				}	da;

				pcommand = GlobalLock ((HANDLE) lPara);
				len = lstrlen (pcommand) + 1;
				commandstring = rmalloc (len);	/* this pointer is passed to and freed in the Clean code. */
				lstrcpyn (commandstring, pcommand, len);
				GlobalUnlock ((HANDLE) lPara);

				SendMessage1ToClean (CcWmDDEEXECUTE, commandstring);

				da.ddeack.bAppReturnCode = 0;
				da.ddeack.fBusy = 0;
				da.ddeack.fAck = 1;
				PostMessage ((HWND) wPara, WM_DDE_ACK, (WPARAM) hWin, PackDDElParam (WM_DDE_ACK, (UINT) da.w, lPara));
				return 0;
			}
			break;
		case WM_DDE_TERMINATE:
			{
				PostMessage ((HWND) wPara, WM_DDE_TERMINATE, (WPARAM) hWin, 0);
			} return 0;
		default:
			return DefWindowProc (hWin, uMess, wPara, lPara);
			break;
	}
	return 0;
}	/*	MainWindowProcedure */

DWORD OsThreadFunction (DWORD param)
{
	WNDCLASSEX wclass;
	int width, height;
	HMENU mainSystemMenu;

	/* register main window class */
	wclass.cbSize        = sizeof (WNDCLASSEX);
	wclass.style         = CS_NOCLOSE;
	wclass.lpfnWndProc   = (WNDPROC) MainWindowProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = 0;
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = NULL;
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = MainWindowClassName;
	wclass.hIconSm       = NULL;
	RegisterClassEx (&wclass);

	GetAppFileName ();

	width  =     GetSystemMetrics (SM_CXMAXIMIZED) - 2 * GetSystemMetrics (SM_CXSIZEFRAME);
	height = 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION) + GetSystemMetrics (SM_CYMENU);

	ghMainWindow
		= CreateWindow (MainWindowClassName,	/* Class name					 */
						(LPCTSTR) gAppName, 	/* Window title 				 */
						WS_OVERLAPPEDWINDOW,	/* style flags					 */
						0, -5 - height,			/* x, y 						 */
						width, height,			/* width, height 				 */
						NULL,					/* Parent window				 */
						NULL,					/* menu handle					 */
						(HANDLE) ghInst,		/* Instance that owns the window */
						0);
	/*	Don't show the main window. This will result in one button less in the taskbar.
	ShowWindow (ghMainWindow, SW_SHOWNORMAL);
	*/
	/*	Before creating Clean controls, the tooltip control is created as the topmost child of this window. */
	ghwndTT = CreateWindowEx (	WS_EX_TOPMOST,					// Apply the topmost style for this window
								TOOLTIPS_CLASS,					// Class name
								NULL,							// Title (NULL)
								WS_POPUP | TTS_ALWAYSTIP,		// Style *must* be WS_POPUP
								CW_USEDEFAULT,					// Default position (x,y)
								CW_USEDEFAULT,
								CW_USEDEFAULT,					// Default size (w,h)
								CW_USEDEFAULT,
								ghMainWindow,					// Parent is the ghMainWindow
								(HMENU) NULL,					// No menu
								(HANDLE) ghInst,				// The instance
								NULL							// No window creation data
							 );

	mainSystemMenu = GetSystemMenu (ghMainWindow,FALSE);
	RemoveMenu (mainSystemMenu, SC_RESTORE,  MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_MOVE,     MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_SIZE,     MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_MINIMIZE, MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_MAXIMIZE, MF_BYCOMMAND);
	DrawMenuBar (ghMainWindow);

	KickCleanThread (MakeReturn0Cci (&gCci));

	while (1)
	{
		HandleCleanRequest (&gCci);
	}

	MakeReturn0Cci (&gCci);
	SetEvent (gOS_DONE);

	return 0;
}	/* OsThreadFunction */
