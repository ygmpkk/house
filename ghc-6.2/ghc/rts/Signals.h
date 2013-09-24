/* -----------------------------------------------------------------------------
 * $Id: Signals.h,v 1.1 2004/11/29 21:09:26 hallgren Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

/* #if !defined(PAR) && !defined(mingw32_TARGET_OS) && !defined(STANDALONE) */
#if !defined(PAR) && !defined(mingw32_TARGET_OS)
#define RTS_USER_SIGNALS 1

extern StgPtr pending_handler_buf[];
extern StgPtr *next_pending_handler;

#define signals_pending() (next_pending_handler != pending_handler_buf)

extern void    initUserSignals(void);
extern void    blockUserSignals(void);
extern void    unblockUserSignals(void);

extern rtsBool anyUserHandlers(void);
extern void    awaitUserSignals(void);

/* sig_install declared in PrimOps.h */

extern void startSignalHandlers(void);
extern void markSignalHandlers (evac_fn evac);
extern void initDefaultHandlers(void);

extern void handleSignalsInThisThread(void);

#else

#define signals_pending() (rtsFalse)
#define handleSignalsInThisThread() /* nothing */

#endif /* !PAR && !mingw32_TARGET_OS */
