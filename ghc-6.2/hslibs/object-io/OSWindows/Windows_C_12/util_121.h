#ifndef _UTILH
#define _UTILH

#include <windows.h>
#include <mmsystem.h>
#include <shlobj.h>


#define SIGNEDLOWORD(i)  ((short) i)
#define SIGNEDHIWORD(i)  ((short) ((i)>>16))


/*  OS type, threading all calls from Clean.
*/

typedef int HITEM;

typedef struct
{   int  mess;
    int  p1;
    int  p2;
    int  p3;
    int  p4;
    int  p5;
    int  p6;
} CrossCallInfo;

#include "intrface_121.h"

extern void SetLogFontData (LOGFONT*, char*, int, int);

/*  since we don't use the C runtime library, here are some simple
    routines that would normally come from the C runtime lib.
*/
// PA: extern added
extern void rfree(void *ptr);
extern void *rmalloc(DWORD bytes);

/*  clean_strings don't have to end with 0, so we have to make
    copy the clean string and end it with a 0.
    global variables used for conversion from c strings to clean strings
*/

//	PA: extern added to the end
extern int nCopyAnsiToWideChar (LPWORD, LPSTR);

/*  The following routines are used to write to the console, or convey runtime errors
    with message boxes.
*/

#ifndef _RPRINTBUFSIZE
#define _RPRINTBUFSIZE 512
#endif

extern void rMessageBox(HWND owner, UINT style, char *title, char *format, ... );
extern void CheckF(BOOL theCheck, char *checkText, char *checkMess, char *filename, int linenum);
extern void ErrorExit(char *format, ...);

#define Check(check,mess) CheckF((check),(#check),(mess),__FILE__,__LINE__)

//#define LOGFILE "debuglog.txt"
# undef LOGFILE

#ifdef LOGFILE
extern void rprintf(char *format, ... );
extern void printCCI( CrossCallInfo *pcci );
extern void printMessage( char* fname, HWND hWin, UINT uMess, WPARAM wPara, LPARAM lPara);
#else
# define rprintf /* RWS() */
# define printCCI(a1)
# define printMessage(a1,a2,a3,a4,a5)
#endif

#endif
