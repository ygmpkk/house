#include <windows.h>
#include "errors.h"
#include "win32debug.h"
#include "HsFFI.h"
extern UINT prim_cOLOR_SCROLLBAR();
extern UINT prim_cOLOR_BACKGROUND();
extern UINT prim_cOLOR_ACTIVECAPTION();
extern UINT prim_cOLOR_INACTIVECAPTION();
extern UINT prim_cOLOR_MENU();
extern UINT prim_cOLOR_WINDOW();
extern UINT prim_cOLOR_WINDOWFRAME();
extern UINT prim_cOLOR_MENUTEXT();
extern UINT prim_cOLOR_WINDOWTEXT();
extern UINT prim_cOLOR_CAPTIONTEXT();
extern UINT prim_cOLOR_ACTIVEBORDER();
extern UINT prim_cOLOR_INACTIVEBORDER();
extern UINT prim_cOLOR_APPWORKSPACE();
extern UINT prim_cOLOR_HIGHLIGHT();
extern UINT prim_cOLOR_HIGHLIGHTTEXT();
extern UINT prim_cOLOR_BTNFACE();
extern UINT prim_cOLOR_BTNSHADOW();
extern UINT prim_cOLOR_GRAYTEXT();
extern UINT prim_cOLOR_BTNTEXT();
extern UINT prim_cOLOR_INACTIVECAPTIONTEXT();
extern UINT prim_cOLOR_BTNHIGHLIGHT();
extern UINT prim_sM_ARRANGE();
extern UINT prim_sM_CLEANBOOT();
extern UINT prim_sM_CMETRICS();
extern UINT prim_sM_CMOUSEBUTTONS();
extern UINT prim_sM_CXBORDER();
extern UINT prim_sM_CYBORDER();
extern UINT prim_sM_CXCURSOR();
extern UINT prim_sM_CYCURSOR();
extern UINT prim_sM_CXDLGFRAME();
extern UINT prim_sM_CYDLGFRAME();
extern UINT prim_sM_CXDOUBLECLK();
extern UINT prim_sM_CYDOUBLECLK();
extern UINT prim_sM_CXDRAG();
extern UINT prim_sM_CYDRAG();
extern UINT prim_sM_CXEDGE();
extern UINT prim_sM_CYEDGE();
extern UINT prim_sM_CXFRAME();
extern UINT prim_sM_CYFRAME();
extern UINT prim_sM_CXFULLSCREEN();
extern UINT prim_sM_CYFULLSCREEN();
extern UINT prim_sM_CXHSCROLL();
extern UINT prim_sM_CYVSCROLL();
extern UINT prim_sM_CXICON();
extern UINT prim_sM_CYICON();
extern UINT prim_sM_CXICONSPACING();
extern UINT prim_sM_CYICONSPACING();
extern UINT prim_sM_CXMAXIMIZED();
extern UINT prim_sM_CYMAXIMIZED();
extern UINT prim_sM_CXMENUCHECK();
extern UINT prim_sM_CYMENUCHECK();
extern UINT prim_sM_CXMENUSIZE();
extern UINT prim_sM_CYMENUSIZE();
extern UINT prim_sM_CXMIN();
extern UINT prim_sM_CYMIN();
extern UINT prim_sM_CXMINIMIZED();
extern UINT prim_sM_CYMINIMIZED();
extern UINT prim_sM_CXMINTRACK();
extern UINT prim_sM_CYMINTRACK();
extern UINT prim_sM_CXSCREEN();
extern UINT prim_sM_CYSCREEN();
extern UINT prim_sM_CXSIZE();
extern UINT prim_sM_CYSIZE();
extern UINT prim_sM_CXSIZEFRAME();
extern UINT prim_sM_CYSIZEFRAME();
extern UINT prim_sM_CXSMICON();
extern UINT prim_sM_CYSMICON();
extern UINT prim_sM_CXSMSIZE();
extern UINT prim_sM_CYSMSIZE();
extern UINT prim_sM_CXVSCROLL();
extern UINT prim_sM_CYHSCROLL();
extern UINT prim_sM_CYVTHUMB();
extern UINT prim_sM_CYCAPTION();
extern UINT prim_sM_CYKANJIWINDOW();
extern UINT prim_sM_CYMENU();
extern UINT prim_sM_CYSMCAPTION();
extern UINT prim_sM_DBCSENABLED();
extern UINT prim_sM_DEBUG();
extern UINT prim_sM_MENUDROPALIGNMENT();
extern UINT prim_sM_MIDEASTENABLED();
extern UINT prim_sM_MOUSEPRESENT();
extern UINT prim_sM_NETWORK();
extern UINT prim_sM_PENWINDOWS();
extern UINT prim_sM_SECURE();
extern UINT prim_sM_SHOWSOUNDS();
extern UINT prim_sM_SLOWMACHINE();
extern UINT prim_sM_SWAPBUTTON();
