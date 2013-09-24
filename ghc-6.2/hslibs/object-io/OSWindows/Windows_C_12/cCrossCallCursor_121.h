#include <windows.h>
#include "cCrossCall_121.h"


/*	Constants:
*/
#define CURSHIDDEN		6
#define CURSARROW		5
#define CURSFATCROSS	4
#define CURSCROSS		3
#define CURSIBEAM		2
#define CURSBUSY		1

/*	Functions:
*/
extern HCURSOR GetHiddenCursor (void);
extern void DeleteCursors (void);
extern HCURSOR SetCursorFromCode (int code);

extern int GetGlobalCursorCode (void);
extern BOOL GlobalCursorSet (void);

//	InstallCrossCallCursor adds the proper cross call procedures to the
//	cross call procedures managed by cCrossCall_121.c.
extern void InstallCrossCallCursor ();
