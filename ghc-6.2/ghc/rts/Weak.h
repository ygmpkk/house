/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The GHC Team, 1998-1999
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

extern StgWeak *weak_ptr_list;

void finalizeWeakPointersNow(void);
void scheduleFinalizers(StgWeak *w);
void markWeakList(void);


