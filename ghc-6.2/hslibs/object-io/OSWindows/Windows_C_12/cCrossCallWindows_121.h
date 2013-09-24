#include <windows.h>
#include "util_121.h"

extern HWND ghCaretWnd;

//	InstallCrossCallFileSelectors adds the proper cross call procedures to the
//	cross call procedures managed by cCrossCall_121.c.
extern void InstallCrossCallWindows ();
