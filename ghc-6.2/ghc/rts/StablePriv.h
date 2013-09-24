/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The GHC Team, 1998-1999
 *
 * Internal RTS API for stable names and stable ptrs.
 *
 * ---------------------------------------------------------------------------*/

extern void    initStablePtrTable    ( void );
extern void    enlargeStablePtrTable ( void );
extern StgWord lookupStableName      ( StgPtr p );

extern void    markStablePtrTable    ( evac_fn evac );
extern void    threadStablePtrTable  ( evac_fn evac );
extern void    gcStablePtrTable      ( void );
extern void    updateStablePtrTable  ( rtsBool full );
