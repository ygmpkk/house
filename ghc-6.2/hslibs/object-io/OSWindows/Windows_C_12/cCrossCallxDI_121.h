#include <windows.h>
#include "util_121.h"


//	Global data with external references:
extern HWND ghTopDocWindow;
extern int gComboSelection;
extern BOOL gInMouseDown;
extern BOOL gInKey;
extern int gCurChar;

/*	Registered Windows class names:
*/
extern char SDIFrameClassName[];		/* Class for SDI frames.  */
extern char MDIFrameClassName[];		/* Class for MDI frames.  */
extern char SDIWindowClassName[];		/* Class for SDI windows (must have same length as MDIWindowClassName). */
extern char MDIWindowClassName[];		/* Class for MDI windows (must have same length as SDIWindowClassName). */

/*	Managing the double down distance.
*/
extern void WinSetDoubleDownDist (int dd);

/*	Sending keyboard events to Clean thread:
*/
extern void SendKeyDownToClean      (HWND hwndParent, HWND hwndChild, int c);
extern void SendKeyStillDownToClean (HWND hwndParent, HWND hwndChild, int c);
extern void SendKeyUpToClean        (HWND hwndParent, HWND hwndChild, int c);

/*	Sending mouse events to Clean thread:
*/
extern void SendMouseUpToClean        (HWND hwndParent, HWND hwndChild, int x, int y);
extern void SendMouseStillDownToClean (HWND hwndParent, HWND hwndChild, int x, int y);
extern void SendMouseStillUpToClean   (HWND hwndParent, HWND hwndChild, int x, int y);
extern void SendMouseDownToClean      (HWND hwndParent, HWND hwndChild, int x, int y);

//	InstallCrossCallxDI adds the proper cross call procedures to the
//	cross call procedures managed by cCrossCall_121.c.
extern void InstallCrossCallxDI ();
