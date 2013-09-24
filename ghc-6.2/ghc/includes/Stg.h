/* -----------------------------------------------------------------------------
 * $Id: Stg.h,v 1.55.2.1 2003/09/22 09:14:12 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Top-level include file for everything STG-ish.  
 *
 * This file is included *automatically* by all .hc files.
 *
 * NOTE: always include Stg.h *before* any other headers, because we
 * define some register variables which must be done before any inline
 * functions are defined (some system headers have been known to
 * define the odd inline function).
 *
 * ---------------------------------------------------------------------------*/

#ifndef STG_H
#define STG_H


/* If we include "Stg.h" directly, we're in STG code, and we therefore
 * get all the global register variables, macros etc. that go along
 * with that.  If "Stg.h" is included via "Rts.h", we're assumed to
 * be in vanilla C.
 */
#ifndef IN_STG_CODE
# define IN_STG_CODE 1
#endif

#if IN_STG_CODE == 0
# ifndef NO_REGS
#  define NO_REGS			/* don't define fixed registers */
# endif
#endif

/* Configuration */
#include "config.h"

/* This needs to be up near the top as the register line on alpha needs
 * to be before all procedures */
#include "TailCalls.h"

#if __GNUC__ >= 3
/* Assume that a flexible array member at the end of a struct
 * can be defined thus: T arr[]; */
#define FLEXIBLE_ARRAY
#else
/* Assume that it must be defined thus: T arr[0]; */
#define FLEXIBLE_ARRAY 0
#endif

#if defined(SMP) || defined(THREADED_RTS)
#define RTS_SUPPORTS_THREADS 1
#endif

/* Some macros to handle DLLing (Win32 only at the moment). */
#include "StgDLL.h"

/* Fix for mingw stat problem (done here so it's early enough) */
#ifdef mingw32_TARGET_OS
#define __MSVCRT__ 1
#endif

/* Turn lazy blackholing and eager blackholing on/off.
 *
 * Using eager blackholing makes things easier to debug because
 * the blackholes are more predictable - but it's slower and less sexy.
 *
 * For now, do lazy and not eager.
 */

/* TICKY_TICKY needs EAGER_BLACKHOLING to verify no double-entries of
 * single-entry thunks.
 *
 * SMP needs EAGER_BLACKHOLING because it has to lock thunks
 * synchronously, in case another thread is trying to evaluate the
 * same thunk simultaneously.
 */
#if defined(SMP) || defined(TICKY_TICKY)
#  define EAGER_BLACKHOLING
#else
#  define LAZY_BLACKHOLING
#endif

/* TABLES_NEXT_TO_CODE says whether to assume that info tables are
 * assumed to reside just before the code for a function.
 *
 * UNDEFINING THIS WON'T WORK ON ITS OWN.  You have been warned.
 */
#if !defined(USE_MINIINTERPRETER) && !defined(ia64_TARGET_ARCH)
#define TABLES_NEXT_TO_CODE
#endif

/* bit macros
 */
#define BITS_PER_BYTE 8
#define BITS_IN(x) (BITS_PER_BYTE * sizeof(x))

/* -----------------------------------------------------------------------------
   Assertions and Debuggery
   -------------------------------------------------------------------------- */

#ifndef DEBUG
#define ASSERT(predicate) /* nothing */
#else

void _stgAssert (char *, unsigned int);

#define ASSERT(predicate)			\
	if (predicate)				\
	    /*null*/;				\
	else					\
	    _stgAssert(__FILE__, __LINE__)
#endif /* DEBUG */

/* 
 * Use this on the RHS of macros which expand to nothing
 * to make sure that the macro can be used in a context which
 * demands a non-empty statement.
 */

#define doNothing() do { } while (0)

/* -----------------------------------------------------------------------------
   Global type definitions
   -------------------------------------------------------------------------- */

#include "StgTypes.h"
#include "RtsTypes.h"

/* -----------------------------------------------------------------------------
   Shorthand forms
   -------------------------------------------------------------------------- */

typedef StgChar		C_;
typedef StgWord		W_;
typedef StgWord*	P_;
typedef P_*		PP_;
typedef StgInt		I_;
typedef StgAddr	        A_;
typedef const StgWord*  D_;
typedef StgFunPtr       F_;
typedef StgByteArray    B_;
typedef StgClosurePtr   L_;

typedef StgInt64        LI_;
typedef StgWord64       LW_;

/*
 * We often want to know the size of something in units of an
 * StgWord... (rounded up, of course!)
 */

#define sizeofW(t) ((sizeof(t)+sizeof(W_)-1)/sizeof(W_))

/* 
 * It's nice to be able to grep for casts
 */

#define stgCast(ty,e) ((ty)(e))

/* -----------------------------------------------------------------------------
   Include everything STG-ish
   -------------------------------------------------------------------------- */

/* Global constaints */
#include "Constants.h"

/* Profiling information */
#include "StgProf.h"
#include "StgLdvProf.h"

/* Storage format definitions */
#include "StgFun.h"
#include "Closures.h"
#include "ClosureTypes.h"
#include "InfoTables.h"
#include "TSO.h"

/* Simulated-parallel information */
#include "GranSim.h"

/* Parallel information */
#include "Parallel.h"

/* STG/Optimised-C related stuff */
#include "SMP.h"
#include "MachRegs.h"
#include "Regs.h"
#include "Block.h"

/* RTS public interface */
#include "RtsAPI.h"

/* System headers: stdlib.h is eeded so that we can use NULL.  It must
 * come after MachRegs.h, because stdlib.h might define some inline
 * functions which may only be defined after register variables have
 * been declared.
 */
#include <stdlib.h>

#ifdef SMP
#include <pthread.h>
#endif

/* GNU mp library */
#include "gmp.h"

/* Storage Manager */
#include "StgStorage.h"

/* Macros for STG/C code */
#include "ClosureMacros.h"
#include "InfoMacros.h"
#include "StgMacros.h"
#include "PrimOps.h"
#include "Updates.h"
#include "StgTicky.h"
#include "CCall.h"
#include "Stable.h"

/* Built-in entry points */
#include "StgMiscClosures.h"

/* Runtime-system hooks */
#include "Hooks.h"

#include "Signals.h"

#include "HsFFI.h"

/* Misc stuff without a home */
DLL_IMPORT_RTS extern char **prog_argv;	/* so we can get at these from Haskell */
DLL_IMPORT_RTS extern int    prog_argc;
DLL_IMPORT_RTS extern char  *prog_name;

extern void stackOverflow(void);

#if defined(WANT_DOTNET_SUPPORT)
#include "DNInvoke.h"
#endif

/* Creating and destroying an adjustor thunk.
   I cannot make myself create a separate .h file
   for these two (sof.) 
   
*/
extern void* createAdjustor(int cconv, StgStablePtr hptr, StgFunPtr wptr);
extern void  freeHaskellFunctionPtr(void* ptr);

#endif /* STG_H */
