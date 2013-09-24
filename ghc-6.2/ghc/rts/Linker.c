/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The GHC Team, 2000-2003
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/
#ifndef STANDALONE

#if 0
#include "PosixSource.h"
#endif
#include "Rts.h"
#include "RtsFlags.h"
#include "HsFFI.h"
#include "Hash.h"
#include "Linker.h"
#include "LinkerInternals.h"
#include "RtsUtils.h"
#include "StoragePriv.h"
#include "Schedule.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <stdlib.h>
#include <string.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if defined(HAVE_FRAMEWORK_HASKELLSUPPORT)
#include <HaskellSupport/dlfcn.h>
#elif defined(HAVE_DLFCN_H)
#include <dlfcn.h>
#endif

#if defined(cygwin32_TARGET_OS)
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <regex.h>
#include <sys/fcntl.h>
#include <sys/termios.h>
#include <sys/utime.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#endif

#if defined(ia64_TARGET_ARCH)
#define USE_MMAP
#include <fcntl.h>
#include <sys/mman.h>
#endif

#if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS) || defined(freebsd_TARGET_OS) || defined(netbsd_TARGET_OS) || defined(openbsd_TARGET_OS)
#  define OBJFORMAT_ELF
#elif defined(cygwin32_TARGET_OS) || defined (mingw32_TARGET_OS)
#  define OBJFORMAT_PEi386
#  include <windows.h>
#  include <math.h>
#elif defined(darwin_TARGET_OS)
#  include <mach-o/ppc/reloc.h>
#  define OBJFORMAT_MACHO
#  include <mach-o/loader.h>
#  include <mach-o/nlist.h>
#  include <mach-o/reloc.h>
#  include <mach-o/dyld.h>
#endif

/* Hash table mapping symbol names to Symbol */
static /*Str*/HashTable *symhash;

/* List of currently loaded objects */
ObjectCode *objects = NULL;	/* initially empty */

#if defined(OBJFORMAT_ELF)
static int ocVerifyImage_ELF    ( ObjectCode* oc );
static int ocGetNames_ELF       ( ObjectCode* oc );
static int ocResolve_ELF        ( ObjectCode* oc );
#elif defined(OBJFORMAT_PEi386)
static int ocVerifyImage_PEi386 ( ObjectCode* oc );
static int ocGetNames_PEi386    ( ObjectCode* oc );
static int ocResolve_PEi386     ( ObjectCode* oc );
#elif defined(OBJFORMAT_MACHO)
static int ocAllocateJumpIslands_MachO ( ObjectCode* oc );
static int ocVerifyImage_MachO    ( ObjectCode* oc );
static int ocGetNames_MachO       ( ObjectCode* oc );
static int ocResolve_MachO        ( ObjectCode* oc );

static void machoInitSymbolsWithoutUnderscore( void );
#endif

/* -----------------------------------------------------------------------------
 * Built-in symbols from the RTS
 */

typedef struct _RtsSymbolVal {
    char   *lbl;
    void   *addr;
} RtsSymbolVal;


#if !defined(PAR)
#define Maybe_ForeignObj        SymX(mkForeignObjzh_fast)

#define Maybe_Stable_Names      SymX(mkWeakzh_fast)			\
      				SymX(makeStableNamezh_fast)		\
      				SymX(finalizzeWeakzh_fast)
#else
/* These are not available in GUM!!! -- HWL */
#define Maybe_ForeignObj
#define Maybe_Stable_Names
#endif

#if !defined (mingw32_TARGET_OS)
#define RTS_POSIX_ONLY_SYMBOLS                  \
      SymX(stg_sig_install)			\
      Sym(nocldstop)
#endif

#if defined (cygwin32_TARGET_OS)
#define RTS_MINGW_ONLY_SYMBOLS /**/
/* Don't have the ability to read import libs / archives, so
 * we have to stupidly list a lot of what libcygwin.a
 * exports; sigh. 
 */
#define RTS_CYGWIN_ONLY_SYMBOLS                 \
      SymX(regfree)                             \
      SymX(regexec)                             \
      SymX(regerror)                            \
      SymX(regcomp)                             \
      SymX(__errno)                             \
      SymX(access)                              \
      SymX(chmod)                               \
      SymX(chdir)                               \
      SymX(close)                               \
      SymX(creat)                               \
      SymX(dup)                                 \
      SymX(dup2)                                \
      SymX(fstat)                               \
      SymX(fcntl)                               \
      SymX(getcwd)                              \
      SymX(getenv)                              \
      SymX(lseek)                               \
      SymX(open)                                \
      SymX(fpathconf)                           \
      SymX(pathconf)                            \
      SymX(stat)                                \
      SymX(pow)                                 \
      SymX(tanh)                                \
      SymX(cosh)                                \
      SymX(sinh)                                \
      SymX(atan)                                \
      SymX(acos)                                \
      SymX(asin)                                \
      SymX(tan)                                 \
      SymX(cos)                                 \
      SymX(sin)                                 \
      SymX(exp)                                 \
      SymX(log)                                 \
      SymX(sqrt)                                \
      SymX(localtime_r)                         \
      SymX(gmtime_r)                            \
      SymX(mktime)                              \
      Sym(_imp___tzname)                        \
      SymX(gettimeofday)                        \
      SymX(timezone)                            \
      SymX(tcgetattr)                           \
      SymX(tcsetattr)                           \
      SymX(memcpy)                              \
      SymX(memmove)                             \
      SymX(realloc)                             \
      SymX(malloc)                              \
      SymX(free)                                \
      SymX(fork)                                \
      SymX(lstat)                               \
      SymX(isatty)                              \
      SymX(mkdir)                               \
      SymX(opendir)                             \
      SymX(readdir)                             \
      SymX(rewinddir)                           \
      SymX(closedir)                            \
      SymX(link)                                \
      SymX(mkfifo)                              \
      SymX(pipe)                                \
      SymX(read)                                \
      SymX(rename)                              \
      SymX(rmdir)                               \
      SymX(select)                              \
      SymX(system)                              \
      SymX(write)                               \
      SymX(strcmp)                              \
      SymX(strcpy)                              \
      SymX(strncpy)                             \
      SymX(strerror)                            \
      SymX(sigaddset)                           \
      SymX(sigemptyset)                         \
      SymX(sigprocmask)                         \
      SymX(umask)                               \
      SymX(uname)                               \
      SymX(unlink)                              \
      SymX(utime)                               \
      SymX(waitpid)

#elif !defined(mingw32_TARGET_OS)
#define RTS_MINGW_ONLY_SYMBOLS /**/
#define RTS_CYGWIN_ONLY_SYMBOLS /**/
#else /* defined(mingw32_TARGET_OS) */
#define RTS_POSIX_ONLY_SYMBOLS  /**/
#define RTS_CYGWIN_ONLY_SYMBOLS /**/

/* Extra syms gen'ed by mingw-2's gcc-3.2: */
#if __GNUC__>=3
#define RTS_MINGW_EXTRA_SYMS                    \
      Sym(_imp____mb_cur_max)                   \
      Sym(_imp___pctype)            
#else
#define RTS_MINGW_EXTRA_SYMS
#endif

/* These are statically linked from the mingw libraries into the ghc
   executable, so we have to employ this hack. */
#define RTS_MINGW_ONLY_SYMBOLS                  \
      SymX(asyncReadzh_fast)			\
      SymX(asyncWritezh_fast)			\
      SymX(memset)                              \
      SymX(inet_ntoa)                           \
      SymX(inet_addr)                           \
      SymX(htonl)                               \
      SymX(recvfrom)                            \
      SymX(listen)                              \
      SymX(bind)                                \
      SymX(shutdown)                            \
      SymX(connect)                             \
      SymX(htons)                               \
      SymX(ntohs)                               \
      SymX(getservbyname)                       \
      SymX(getservbyport)                       \
      SymX(getprotobynumber)                    \
      SymX(getprotobyname)                      \
      SymX(gethostbyname)                       \
      SymX(gethostbyaddr)                       \
      SymX(gethostname)                         \
      SymX(strcpy)                              \
      SymX(strncpy)                             \
      SymX(abort)                               \
      Sym(_alloca)                              \
      Sym(isxdigit)                             \
      Sym(isupper)                              \
      Sym(ispunct)                              \
      Sym(islower)                              \
      Sym(isspace)                              \
      Sym(isprint)                              \
      Sym(isdigit)                              \
      Sym(iscntrl)                              \
      Sym(isalpha)                              \
      Sym(isalnum)                              \
      SymX(strcmp)                              \
      SymX(memmove)                             \
      SymX(realloc)                             \
      SymX(malloc)                              \
      SymX(pow)                                 \
      SymX(tanh)                                \
      SymX(cosh)                                \
      SymX(sinh)                                \
      SymX(atan)                                \
      SymX(acos)                                \
      SymX(asin)                                \
      SymX(tan)                                 \
      SymX(cos)                                 \
      SymX(sin)                                 \
      SymX(exp)                                 \
      SymX(log)                                 \
      SymX(sqrt)                                \
      SymX(memcpy)                              \
      Sym(mktime)                               \
      Sym(_imp___timezone)                      \
      Sym(_imp___tzname)                        \
      Sym(_imp___iob)                           \
      Sym(localtime)                            \
      Sym(gmtime)                               \
      Sym(opendir)                              \
      Sym(readdir)                              \
      Sym(rewinddir)                            \
      RTS_MINGW_EXTRA_SYMS                      \
      Sym(closedir)
#endif

#ifndef SMP
# define MAIN_CAP_SYM SymX(MainCapability)
#else
# define MAIN_CAP_SYM
#endif

#define RTS_SYMBOLS				\
      Maybe_ForeignObj				\
      Maybe_Stable_Names			\
      Sym(StgReturn)				\
      SymX(stg_enter_info)			\
      SymX(stg_enter_ret)			\
      SymX(stg_gc_void_info)			\
      SymX(__stg_gc_enter_1)			\
      SymX(stg_gc_noregs)			\
      SymX(stg_gc_unpt_r1_info)			\
      SymX(stg_gc_unpt_r1)			\
      SymX(stg_gc_unbx_r1_info)			\
      SymX(stg_gc_unbx_r1)			\
      SymX(stg_gc_f1_info)			\
      SymX(stg_gc_f1)				\
      SymX(stg_gc_d1_info)			\
      SymX(stg_gc_d1)				\
      SymX(stg_gc_l1_info)			\
      SymX(stg_gc_l1)				\
      SymX(__stg_gc_fun)			\
      SymX(stg_gc_fun_info)			\
      SymX(stg_gc_fun_ret)			\
      SymX(stg_gc_gen)				\
      SymX(stg_gc_gen_info)			\
      SymX(stg_gc_gen_hp)			\
      SymX(stg_gc_ut)				\
      SymX(stg_gen_yield)			\
      SymX(stg_yield_noregs)			\
      SymX(stg_yield_to_interpreter)		\
      SymX(stg_gen_block)			\
      SymX(stg_block_noregs)			\
      SymX(stg_block_1)				\
      SymX(stg_block_takemvar)			\
      SymX(stg_block_putmvar)			\
      SymX(stg_seq_frame_info)			\
      SymX(ErrorHdrHook)			\
      MAIN_CAP_SYM                              \
      SymX(MallocFailHook)			\
      SymX(OnExitHook)				\
      SymX(OutOfHeapHook)			\
      SymX(PatErrorHdrHook)			\
      SymX(PostTraceHook)			\
      SymX(PreTraceHook)			\
      SymX(StackOverflowHook)			\
      SymX(__encodeDouble)			\
      SymX(__encodeFloat)			\
      SymX(__gmpn_gcd_1)			\
      SymX(__gmpz_cmp)				\
      SymX(__gmpz_cmp_si)			\
      SymX(__gmpz_cmp_ui)			\
      SymX(__gmpz_get_si)			\
      SymX(__gmpz_get_ui)			\
      SymX(__int_encodeDouble)			\
      SymX(__int_encodeFloat)			\
      SymX(andIntegerzh_fast)			\
      SymX(blockAsyncExceptionszh_fast)		\
      SymX(catchzh_fast)			\
      SymX(cmp_thread)				\
      SymX(complementIntegerzh_fast)		\
      SymX(cmpIntegerzh_fast)	        	\
      SymX(cmpIntegerIntzh_fast)	      	\
      SymX(createAdjustor)			\
      SymX(decodeDoublezh_fast)			\
      SymX(decodeFloatzh_fast)			\
      SymX(defaultsHook)			\
      SymX(delayzh_fast)			\
      SymX(deRefWeakzh_fast)			\
      SymX(deRefStablePtrzh_fast)		\
      SymX(divExactIntegerzh_fast)		\
      SymX(divModIntegerzh_fast)		\
      SymX(forkzh_fast)				\
      SymX(forkProcess)				\
      SymX(forkOS_createThread)			\
      SymX(freeHaskellFunctionPtr)		\
      SymX(freeStablePtr)		        \
      SymX(gcdIntegerzh_fast)			\
      SymX(gcdIntegerIntzh_fast)		\
      SymX(gcdIntzh_fast)			\
      SymX(genSymZh)				\
      SymX(getProgArgv)				\
      SymX(getStablePtr)			\
      SymX(int2Integerzh_fast)			\
      SymX(integer2Intzh_fast)			\
      SymX(integer2Wordzh_fast)			\
      SymX(isCurrentThreadBoundzh_fast)		\
      SymX(isDoubleDenormalized)		\
      SymX(isDoubleInfinite)			\
      SymX(isDoubleNaN)				\
      SymX(isDoubleNegativeZero)		\
      SymX(isEmptyMVarzh_fast)			\
      SymX(isFloatDenormalized)			\
      SymX(isFloatInfinite)			\
      SymX(isFloatNaN)				\
      SymX(isFloatNegativeZero)			\
      SymX(killThreadzh_fast)			\
      SymX(makeStablePtrzh_fast)		\
      SymX(minusIntegerzh_fast)			\
      SymX(mkApUpd0zh_fast)			\
      SymX(myThreadIdzh_fast)			\
      SymX(labelThreadzh_fast)                  \
      SymX(newArrayzh_fast)			\
      SymX(newBCOzh_fast)			\
      SymX(newByteArrayzh_fast)			\
      SymX_redirect(newCAF, newDynCAF)		\
      SymX(newMVarzh_fast)			\
      SymX(newMutVarzh_fast)			\
      SymX(atomicModifyMutVarzh_fast)		\
      SymX(newPinnedByteArrayzh_fast)		\
      SymX(orIntegerzh_fast)			\
      SymX(performGC)				\
      SymX(plusIntegerzh_fast)			\
      SymX(prog_argc)				\
      SymX(prog_argv)				\
      SymX(putMVarzh_fast)			\
      SymX(quotIntegerzh_fast)			\
      SymX(quotRemIntegerzh_fast)		\
      SymX(raisezh_fast)			\
      SymX(raiseIOzh_fast)			\
      SymX(remIntegerzh_fast)			\
      SymX(resetNonBlockingFd)			\
      SymX(resumeThread)			\
      SymX(rts_apply)				\
      SymX(rts_checkSchedStatus)		\
      SymX(rts_eval)				\
      SymX(rts_evalIO)				\
      SymX(rts_evalLazyIO)			\
      SymX(rts_evalStableIO)			\
      SymX(rts_eval_)				\
      SymX(rts_getBool)				\
      SymX(rts_getChar)				\
      SymX(rts_getDouble)			\
      SymX(rts_getFloat)			\
      SymX(rts_getInt)				\
      SymX(rts_getInt32)			\
      SymX(rts_getPtr)				\
      SymX(rts_getFunPtr)			\
      SymX(rts_getStablePtr)			\
      SymX(rts_getThreadId)			\
      SymX(rts_getWord)				\
      SymX(rts_getWord32)			\
      SymX(rts_lock)				\
      SymX(rts_mkBool)				\
      SymX(rts_mkChar)				\
      SymX(rts_mkDouble)			\
      SymX(rts_mkFloat)				\
      SymX(rts_mkInt)				\
      SymX(rts_mkInt16)				\
      SymX(rts_mkInt32)				\
      SymX(rts_mkInt64)				\
      SymX(rts_mkInt8)				\
      SymX(rts_mkPtr)				\
      SymX(rts_mkFunPtr)			\
      SymX(rts_mkStablePtr)			\
      SymX(rts_mkString)			\
      SymX(rts_mkWord)				\
      SymX(rts_mkWord16)			\
      SymX(rts_mkWord32)			\
      SymX(rts_mkWord64)			\
      SymX(rts_mkWord8)				\
      SymX(rts_unlock)				\
      SymX(rtsSupportsBoundThreads)		\
      SymX(run_queue_hd)			\
      Sym(saved_termios)			\
      SymX(setProgArgv)				\
      SymX(startupHaskell)			\
      SymX(shutdownHaskell)			\
      SymX(shutdownHaskellAndExit)		\
      SymX(stable_ptr_table)			\
      SymX(stackOverflow)			\
      SymX(stg_CAF_BLACKHOLE_info)		\
      SymX(stg_BLACKHOLE_BQ_info)		\
      SymX(awakenBlockedQueue)			\
      SymX(stg_CHARLIKE_closure)		\
      SymX(stg_EMPTY_MVAR_info)			\
      SymX(stg_IND_STATIC_info)			\
      SymX(stg_INTLIKE_closure)			\
      SymX(stg_MUT_ARR_PTRS_FROZEN_info)	\
      SymX(stg_WEAK_info)                       \
      SymX(stg_ap_v_info)			\
      SymX(stg_ap_f_info)			\
      SymX(stg_ap_d_info)			\
      SymX(stg_ap_l_info)			\
      SymX(stg_ap_n_info)			\
      SymX(stg_ap_p_info)			\
      SymX(stg_ap_pv_info)			\
      SymX(stg_ap_pp_info)			\
      SymX(stg_ap_ppv_info)			\
      SymX(stg_ap_ppp_info)			\
      SymX(stg_ap_pppp_info)			\
      SymX(stg_ap_ppppp_info)			\
      SymX(stg_ap_pppppp_info)			\
      SymX(stg_ap_ppppppp_info)			\
      SymX(stg_ap_0_ret)			\
      SymX(stg_ap_v_ret)			\
      SymX(stg_ap_f_ret)			\
      SymX(stg_ap_d_ret)			\
      SymX(stg_ap_l_ret)			\
      SymX(stg_ap_n_ret)			\
      SymX(stg_ap_p_ret)			\
      SymX(stg_ap_pv_ret)			\
      SymX(stg_ap_pp_ret)			\
      SymX(stg_ap_ppv_ret)			\
      SymX(stg_ap_ppp_ret)			\
      SymX(stg_ap_pppp_ret)			\
      SymX(stg_ap_ppppp_ret)			\
      SymX(stg_ap_pppppp_ret)			\
      SymX(stg_ap_ppppppp_ret)			\
      SymX(stg_ap_1_upd_info)			\
      SymX(stg_ap_2_upd_info)			\
      SymX(stg_ap_3_upd_info)			\
      SymX(stg_ap_4_upd_info)			\
      SymX(stg_ap_5_upd_info)			\
      SymX(stg_ap_6_upd_info)			\
      SymX(stg_ap_7_upd_info)			\
      SymX(stg_ap_8_upd_info)			\
      SymX(stg_exit)				\
      SymX(stg_sel_0_upd_info)			\
      SymX(stg_sel_10_upd_info)			\
      SymX(stg_sel_11_upd_info)			\
      SymX(stg_sel_12_upd_info)			\
      SymX(stg_sel_13_upd_info)			\
      SymX(stg_sel_14_upd_info)			\
      SymX(stg_sel_15_upd_info)			\
      SymX(stg_sel_1_upd_info)			\
      SymX(stg_sel_2_upd_info)			\
      SymX(stg_sel_3_upd_info)			\
      SymX(stg_sel_4_upd_info)			\
      SymX(stg_sel_5_upd_info)			\
      SymX(stg_sel_6_upd_info)			\
      SymX(stg_sel_7_upd_info)			\
      SymX(stg_sel_8_upd_info)			\
      SymX(stg_sel_9_upd_info)			\
      SymX(stg_upd_frame_info)			\
      SymX(suspendThread)			\
      SymX(takeMVarzh_fast)			\
      SymX(timesIntegerzh_fast)			\
      SymX(tryPutMVarzh_fast)			\
      SymX(tryTakeMVarzh_fast)			\
      SymX(unblockAsyncExceptionszh_fast)	\
      SymX(unsafeThawArrayzh_fast)		\
      SymX(waitReadzh_fast)			\
      SymX(waitWritezh_fast)			\
      SymX(word2Integerzh_fast)			\
      SymX(xorIntegerzh_fast)			\
      SymX(yieldzh_fast)

#ifdef SUPPORT_LONG_LONGS
#define RTS_LONG_LONG_SYMS			\
      SymX(int64ToIntegerzh_fast)		\
      SymX(word64ToIntegerzh_fast)
#else
#define RTS_LONG_LONG_SYMS /* nothing */
#endif

// 64-bit support functions in libgcc.a
#if defined(__GNUC__) && SIZEOF_VOID_P <= 4
#define RTS_LIBGCC_SYMBOLS			\
      Sym(__divdi3)                             \
      Sym(__udivdi3)                            \
      Sym(__moddi3)                             \
      Sym(__umoddi3)				\
      Sym(__ashldi3)				\
      Sym(__ashrdi3)				\
      Sym(__lshrdi3)				\
      Sym(__eprintf)
#elif defined(ia64_TARGET_ARCH)
#define RTS_LIBGCC_SYMBOLS			\
      Sym(__divdi3)				\
      Sym(__udivdi3)                            \
      Sym(__moddi3)				\
      Sym(__umoddi3)				\
      Sym(__divsf3)				\
      Sym(__divdf3)
#else
#define RTS_LIBGCC_SYMBOLS
#endif

#ifdef darwin_TARGET_OS
      // Symbols that don't have a leading underscore
      // on Mac OS X. They have to receive special treatment,
      // see machoInitSymbolsWithoutUnderscore()
#define RTS_MACHO_NOUNDERLINE_SYMBOLS		\
      Sym(saveFP)				\
      Sym(restFP)
#endif

/* entirely bogus claims about types of these symbols */
#define Sym(vvv)  extern void vvv(void);
#define SymX(vvv) /**/
#define SymX_redirect(vvv,xxx) /**/
RTS_SYMBOLS
RTS_LONG_LONG_SYMS
RTS_POSIX_ONLY_SYMBOLS
RTS_MINGW_ONLY_SYMBOLS
RTS_CYGWIN_ONLY_SYMBOLS
RTS_LIBGCC_SYMBOLS
#undef Sym
#undef SymX
#undef SymX_redirect

#ifdef LEADING_UNDERSCORE
#define MAYBE_LEADING_UNDERSCORE_STR(s) ("_" s)
#else
#define MAYBE_LEADING_UNDERSCORE_STR(s) (s)
#endif

#define Sym(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
                    (void*)(&(vvv)) },
#define SymX(vvv) Sym(vvv)

// SymX_redirect allows us to redirect references to one symbol to
// another symbol.  See newCAF/newDynCAF for an example.
#define SymX_redirect(vvv,xxx) \
    { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
      (void*)(&(xxx)) },

static RtsSymbolVal rtsSyms[] = {
      RTS_SYMBOLS
      RTS_LONG_LONG_SYMS
      RTS_POSIX_ONLY_SYMBOLS
      RTS_MINGW_ONLY_SYMBOLS
      RTS_CYGWIN_ONLY_SYMBOLS
      RTS_LIBGCC_SYMBOLS
      { 0, 0 } /* sentinel */
};

/* -----------------------------------------------------------------------------
 * Insert symbols into hash tables, checking for duplicates.
 */
static void ghciInsertStrHashTable ( char* obj_name,
                                     HashTable *table,
                                     char* key,
                                     void *data
				   )
{
   if (lookupHashTable(table, (StgWord)key) == NULL)
   {
      insertStrHashTable(table, (StgWord)key, data);
      return;
   }
   fprintf(stderr,
      "\n\n"
      "GHCi runtime linker: fatal error: I found a duplicate definition for symbol\n"
      "   %s\n"
      "whilst processing object file\n"
      "   %s\n"
      "This could be caused by:\n"
      "   * Loading two different object files which export the same symbol\n"
      "   * Specifying the same object file twice on the GHCi command line\n"
      "   * An incorrect `package.conf' entry, causing some object to be\n"
      "     loaded twice.\n"
      "GHCi cannot safely continue in this situation.  Exiting now.  Sorry.\n"
      "\n",
      (char*)key,
      obj_name
   );
   exit(1);
}


/* -----------------------------------------------------------------------------
 * initialize the object linker
 */


static int linker_init_done = 0 ;

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
static void *dl_prog_handle;
#endif

void
initLinker( void )
{
    RtsSymbolVal *sym;

    /* Make initLinker idempotent, so we can call it
       before evey relevant operation; that means we
       don't need to initialise the linker separately */
    if (linker_init_done == 1) { return; } else {
      linker_init_done = 1;
    }

    symhash = allocStrHashTable();

    /* populate the symbol table with stuff from the RTS */
    for (sym = rtsSyms; sym->lbl != NULL; sym++) {
	ghciInsertStrHashTable("(GHCi built-in symbols)",
                               symhash, sym->lbl, sym->addr);
    }
#   if defined(OBJFORMAT_MACHO)
    machoInitSymbolsWithoutUnderscore();
#   endif

#   if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
    dl_prog_handle = dlopen(NULL, RTLD_LAZY);
#   endif
}

/* -----------------------------------------------------------------------------
 *                  Loading DLL or .so dynamic libraries
 * -----------------------------------------------------------------------------
 *
 * Add a DLL from which symbols may be found.  In the ELF case, just
 * do RTLD_GLOBAL-style add, so no further messing around needs to
 * happen in order that symbols in the loaded .so are findable --
 * lookupSymbol() will subsequently see them by dlsym on the program's
 * dl-handle.  Returns NULL if success, otherwise ptr to an err msg.
 *
 * In the PEi386 case, open the DLLs and put handles to them in a
 * linked list.  When looking for a symbol, try all handles in the
 * list.  This means that we need to load even DLLs that are guaranteed
 * to be in the ghc.exe image already, just so we can get a handle
 * to give to loadSymbol, so that we can find the symbols.  For such
 * libraries, the LoadLibrary call should be a no-op except for returning
 * the handle.
 * 
 */

#if defined(OBJFORMAT_PEi386)
/* A record for storing handles into DLLs. */

typedef
   struct _OpenedDLL {
      char*              name;
      struct _OpenedDLL* next;
      HINSTANCE instance;
   }
   OpenedDLL;

/* A list thereof. */
static OpenedDLL* opened_dlls = NULL;
#endif

char *
addDLL( char *dll_name )
{
#  if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
   /* ------------------- ELF DLL loader ------------------- */
   void *hdl;
   char *errmsg;

   initLinker();

   hdl= dlopen(dll_name, RTLD_NOW | RTLD_GLOBAL);

   if (hdl == NULL) {
      /* dlopen failed; return a ptr to the error msg. */
      errmsg = dlerror();
      if (errmsg == NULL) errmsg = "addDLL: unknown error";
      return errmsg;
   } else {
      return NULL;
   }
   /*NOTREACHED*/

#  elif defined(OBJFORMAT_PEi386)
   /* ------------------- Win32 DLL loader ------------------- */

   char*      buf;
   OpenedDLL* o_dll;
   HINSTANCE  instance;

   initLinker();

   /* fprintf(stderr, "\naddDLL; dll_name = `%s'\n", dll_name); */

   /* See if we've already got it, and ignore if so. */
   for (o_dll = opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
      if (0 == strcmp(o_dll->name, dll_name))
         return NULL;
   }

   /* The file name has no suffix (yet) so that we can try
      both foo.dll and foo.drv

      The documentation for LoadLibrary says:
      	If no file name extension is specified in the lpFileName
      	parameter, the default library extension .dll is
      	appended. However, the file name string can include a trailing
      	point character (.) to indicate that the module name has no
      	extension. */

   buf = stgMallocBytes(strlen(dll_name) + 10, "addDLL");
   sprintf(buf, "%s.DLL", dll_name);
   instance = LoadLibrary(buf);
   if (instance == NULL) {
	 sprintf(buf, "%s.DRV", dll_name);	// KAA: allow loading of drivers (like winspool.drv)
	 instance = LoadLibrary(buf);
	 if (instance == NULL) {
		stgFree(buf);

	    /* LoadLibrary failed; return a ptr to the error msg. */
	    return "addDLL: unknown error";
   	 }
   }
   stgFree(buf);

   /* Add this DLL to the list of DLLs in which to search for symbols. */
   o_dll = stgMallocBytes( sizeof(OpenedDLL), "addDLL" );
   o_dll->name     = stgMallocBytes(1+strlen(dll_name), "addDLL");
   strcpy(o_dll->name, dll_name);
   o_dll->instance = instance;
   o_dll->next     = opened_dlls;
   opened_dlls     = o_dll;

   return NULL;
#  else
   barf("addDLL: not implemented on this platform");
#  endif
}

/* -----------------------------------------------------------------------------
 * lookup a symbol in the hash table
 */
void *
lookupSymbol( char *lbl )
{
    void *val;
    initLinker() ;
    ASSERT(symhash != NULL);
    val = lookupStrHashTable(symhash, lbl);

    if (val == NULL) {
#       if defined(OBJFORMAT_ELF)
	return dlsym(dl_prog_handle, lbl);
#       elif defined(OBJFORMAT_MACHO)
	if(NSIsSymbolNameDefined(lbl)) {
	    NSSymbol symbol = NSLookupAndBindSymbol(lbl);
	    return NSAddressOfSymbol(symbol);
	} else {
	    return NULL;
	}
#       elif defined(OBJFORMAT_PEi386)
        OpenedDLL* o_dll;
        void* sym;
        for (o_dll = opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
	  /* fprintf(stderr, "look in %s for %s\n", o_dll->name, lbl); */
           if (lbl[0] == '_') {
              /* HACK: if the name has an initial underscore, try stripping
                 it off & look that up first. I've yet to verify whether there's
                 a Rule that governs whether an initial '_' *should always* be
                 stripped off when mapping from import lib name to the DLL name.
              */
              sym = GetProcAddress(o_dll->instance, (lbl+1));
              if (sym != NULL) {
		/*fprintf(stderr, "found %s in %s\n", lbl+1,o_dll->name); fflush(stderr);*/
		return sym;
	      }
           }
           sym = GetProcAddress(o_dll->instance, lbl);
           if (sym != NULL) {
	     /*fprintf(stderr, "found %s in %s\n", lbl,o_dll->name); fflush(stderr);*/
	     return sym;
	   }
        }
        return NULL;
#       else
        ASSERT(2+2 == 5);
        return NULL;
#       endif
    } else {
	return val;
    }
}

static
__attribute((unused))
void *
lookupLocalSymbol( ObjectCode* oc, char *lbl )
{
    void *val;
    initLinker() ;
    val = lookupStrHashTable(oc->lochash, lbl);

    if (val == NULL) {
        return NULL;
    } else {
	return val;
    }
}


/* -----------------------------------------------------------------------------
 * Debugging aid: look in GHCi's object symbol tables for symbols
 * within DELTA bytes of the specified address, and show their names.
 */
#ifdef DEBUG
void ghci_enquire ( char* addr );

void ghci_enquire ( char* addr )
{
   int   i;
   char* sym;
   char* a;
   const int DELTA = 64;
   ObjectCode* oc;

   initLinker();

   for (oc = objects; oc; oc = oc->next) {
      for (i = 0; i < oc->n_symbols; i++) {
         sym = oc->symbols[i];
         if (sym == NULL) continue;
         // fprintf(stderr, "enquire %p %p\n", sym, oc->lochash);
         a = NULL;
         if (oc->lochash != NULL) {
            a = lookupStrHashTable(oc->lochash, sym);
	 }
         if (a == NULL) {
            a = lookupStrHashTable(symhash, sym);
	 }
         if (a == NULL) {
	     // fprintf(stderr, "ghci_enquire: can't find %s\n", sym);
         }
         else if (addr-DELTA <= a && a <= addr+DELTA) {
            fprintf(stderr, "%p + %3d  ==  `%s'\n", addr, a - addr, sym);
         }
      }
   }
}
#endif

#ifdef ia64_TARGET_ARCH
static unsigned int PLTSize(void);
#endif

/* -----------------------------------------------------------------------------
 * Load an obj (populate the global symbol table, but don't resolve yet)
 *
 * Returns: 1 if ok, 0 on error.
 */
HsInt
loadObj( char *path )
{
   ObjectCode* oc;
   struct stat st;
   int r, n;
#ifdef USE_MMAP
   int fd, pagesize;
   void *map_addr;
#else
   FILE *f;
#endif

   initLinker();

   /* fprintf(stderr, "loadObj %s\n", path ); */

   /* Check that we haven't already loaded this object.  Don't give up
      at this stage; ocGetNames_* will barf later. */
   {
       ObjectCode *o;
       int is_dup = 0;
       for (o = objects; o; o = o->next) {
          if (0 == strcmp(o->fileName, path))
             is_dup = 1;
       }
       if (is_dup) {
	 fprintf(stderr,
            "\n\n"
            "GHCi runtime linker: warning: looks like you're trying to load the\n"
            "same object file twice:\n"
            "   %s\n"
            "GHCi will continue, but a duplicate-symbol error may shortly follow.\n"
            "\n"
            , path);
       }
   }

   oc = stgMallocBytes(sizeof(ObjectCode), "loadObj(oc)");

#  if defined(OBJFORMAT_ELF)
   oc->formatName = "ELF";
#  elif defined(OBJFORMAT_PEi386)
   oc->formatName = "PEi386";
#  elif defined(OBJFORMAT_MACHO)
   oc->formatName = "Mach-O";
#  else
   stgFree(oc);
   barf("loadObj: not implemented on this platform");
#  endif

   r = stat(path, &st);
   if (r == -1) { return 0; }

   /* sigh, strdup() isn't a POSIX function, so do it the long way */
   oc->fileName = stgMallocBytes( strlen(path)+1, "loadObj" );
   strcpy(oc->fileName, path);

   oc->fileSize          = st.st_size;
   oc->symbols           = NULL;
   oc->sections          = NULL;
   oc->lochash           = allocStrHashTable();
   oc->proddables        = NULL;

   /* chain it onto the list of objects */
   oc->next              = objects;
   objects               = oc;

#ifdef USE_MMAP
#define ROUND_UP(x,size) ((x + size - 1) & ~(size - 1))

   /* On many architectures malloc'd memory isn't executable, so we need to use mmap. */

   fd = open(path, O_RDONLY);
   if (fd == -1)
      barf("loadObj: can't open `%s'", path);

   pagesize = getpagesize();

#ifdef ia64_TARGET_ARCH
   /* The PLT needs to be right before the object */
   n = ROUND_UP(PLTSize(), pagesize);
   oc->plt = mmap(NULL, n, PROT_EXEC|PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
   if (oc->plt == MAP_FAILED)
      barf("loadObj: can't allocate PLT");

   oc->pltIndex = 0;
   map_addr = oc->plt + n;
#endif

   n = ROUND_UP(oc->fileSize, pagesize);
   oc->image = mmap(map_addr, n, PROT_EXEC|PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);
   if (oc->image == MAP_FAILED)
      barf("loadObj: can't map `%s'", path);

   close(fd);

#else /* !USE_MMAP */

   oc->image = stgMallocBytes(oc->fileSize, "loadObj(image)");

   /* load the image into memory */
   f = fopen(path, "rb");
   if (!f)
       barf("loadObj: can't read `%s'", path);

   n = fread ( oc->image, 1, oc->fileSize, f );
   if (n != oc->fileSize)
      barf("loadObj: error whilst reading `%s'", path);

   fclose(f);

#endif /* USE_MMAP */

#  if defined(OBJFORMAT_MACHO)
   r = ocAllocateJumpIslands_MachO ( oc );
   if (!r) { return r; }
#endif

   /* verify the in-memory image */
#  if defined(OBJFORMAT_ELF)
   r = ocVerifyImage_ELF ( oc );
#  elif defined(OBJFORMAT_PEi386)
   r = ocVerifyImage_PEi386 ( oc );
#  elif defined(OBJFORMAT_MACHO)
   r = ocVerifyImage_MachO ( oc );
#  else
   barf("loadObj: no verify method");
#  endif
   if (!r) { return r; }

   /* build the symbol list for this image */
#  if defined(OBJFORMAT_ELF)
   r = ocGetNames_ELF ( oc );
#  elif defined(OBJFORMAT_PEi386)
   r = ocGetNames_PEi386 ( oc );
#  elif defined(OBJFORMAT_MACHO)
   r = ocGetNames_MachO ( oc );
#  else
   barf("loadObj: no getNames method");
#  endif
   if (!r) { return r; }

   /* loaded, but not resolved yet */
   oc->status = OBJECT_LOADED;

   return 1;
}

/* -----------------------------------------------------------------------------
 * resolve all the currently unlinked objects in memory
 *
 * Returns: 1 if ok, 0 on error.
 */
HsInt
resolveObjs( void )
{
    ObjectCode *oc;
    int r;

    initLinker();

    for (oc = objects; oc; oc = oc->next) {
	if (oc->status != OBJECT_RESOLVED) {
#           if defined(OBJFORMAT_ELF)
	    r = ocResolve_ELF ( oc );
#           elif defined(OBJFORMAT_PEi386)
	    r = ocResolve_PEi386 ( oc );
#           elif defined(OBJFORMAT_MACHO)
	    r = ocResolve_MachO ( oc );
#           else
	    barf("resolveObjs: not implemented on this platform");
#           endif
	    if (!r) { return r; }
	    oc->status = OBJECT_RESOLVED;
	}
    }
    return 1;
}

/* -----------------------------------------------------------------------------
 * delete an object from the pool
 */
HsInt
unloadObj( char *path )
{
    ObjectCode *oc, *prev;

    ASSERT(symhash != NULL);
    ASSERT(objects != NULL);

    initLinker(); 

    prev = NULL;
    for (oc = objects; oc; prev = oc, oc = oc->next) {
	if (!strcmp(oc->fileName,path)) {

	    /* Remove all the mappings for the symbols within this
	     * object..
	     */
	    {
                int i;
                for (i = 0; i < oc->n_symbols; i++) {
                   if (oc->symbols[i] != NULL) {
                       removeStrHashTable(symhash, oc->symbols[i], NULL);
                   }
                }
            }

	    if (prev == NULL) {
		objects = oc->next;
	    } else {
		prev->next = oc->next;
	    }

	    /* We're going to leave this in place, in case there are
	       any pointers from the heap into it: */
	    /* stgFree(oc->image); */
	    stgFree(oc->fileName);
	    stgFree(oc->symbols);
	    stgFree(oc->sections);
	    /* The local hash table should have been freed at the end
               of the ocResolve_ call on it. */
            ASSERT(oc->lochash == NULL);
	    stgFree(oc);
	    return 1;
	}
    }

    belch("unloadObj: can't find `%s' to unload", path);
    return 0;
}

/* -----------------------------------------------------------------------------
 * Sanity checking.  For each ObjectCode, maintain a list of address ranges
 * which may be prodded during relocation, and abort if we try and write
 * outside any of these.
 */
static void addProddableBlock ( ObjectCode* oc, void* start, int size )
{
   ProddableBlock* pb
      = stgMallocBytes(sizeof(ProddableBlock), "addProddableBlock");
   /* fprintf(stderr, "aPB %p %p %d\n", oc, start, size); */
   ASSERT(size > 0);
   pb->start      = start;
   pb->size       = size;
   pb->next       = oc->proddables;
   oc->proddables = pb;
}

static void checkProddableBlock ( ObjectCode* oc, void* addr )
{
   ProddableBlock* pb;
   for (pb = oc->proddables; pb != NULL; pb = pb->next) {
      char* s = (char*)(pb->start);
      char* e = s + pb->size - 1;
      char* a = (char*)addr;
      /* Assumes that the biggest fixup involves a 4-byte write.  This
         probably needs to be changed to 8 (ie, +7) on 64-bit
         plats. */
      if (a >= s && (a+3) <= e) return;
   }
   barf("checkProddableBlock: invalid fixup in runtime linker");
}

/* -----------------------------------------------------------------------------
 * Section management.
 */
static void addSection ( ObjectCode* oc, SectionKind kind,
                         void* start, void* end )
{
   Section* s   = stgMallocBytes(sizeof(Section), "addSection");
   s->start     = start;
   s->end       = end;
   s->kind      = kind;
   s->next      = oc->sections;
   oc->sections = s;
   /*
   fprintf(stderr, "addSection: %p-%p (size %d), kind %d\n",
                   start, ((char*)end)-1, end - start + 1, kind );
   */
}



/* --------------------------------------------------------------------------
 * PEi386 specifics (Win32 targets)
 * ------------------------------------------------------------------------*/

/* The information for this linker comes from
      Microsoft Portable Executable
      and Common Object File Format Specification
      revision 5.1 January 1998
   which SimonM says comes from the MS Developer Network CDs.

   It can be found there (on older CDs), but can also be found
   online at:

      http://www.microsoft.com/hwdev/hardware/PECOFF.asp

   (this is Rev 6.0 from February 1999).

   Things move, so if that fails, try searching for it via

      http://www.google.com/search?q=PE+COFF+specification

   The ultimate reference for the PE format is the Winnt.h
   header file that comes with the Platform SDKs; as always,
   implementations will drift wrt their documentation.

   A good background article on the PE format is Matt Pietrek's
   March 1994 article in Microsoft System Journal (MSJ)
   (Vol.9, No. 3): "Peering Inside the PE: A Tour of the
   Win32 Portable Executable File Format." The info in there
   has recently been updated in a two part article in
   MSDN magazine, issues Feb and March 2002,
   "Inside Windows: An In-Depth Look into the Win32 Portable
   Executable File Format"

   John Levine's book "Linkers and Loaders" contains useful
   info on PE too.
*/


#if defined(OBJFORMAT_PEi386)



typedef unsigned char  UChar;
typedef unsigned short UInt16;
typedef unsigned int   UInt32;
typedef          int   Int32;


typedef
   struct {
      UInt16 Machine;
      UInt16 NumberOfSections;
      UInt32 TimeDateStamp;
      UInt32 PointerToSymbolTable;
      UInt32 NumberOfSymbols;
      UInt16 SizeOfOptionalHeader;
      UInt16 Characteristics;
   }
   COFF_header;

#define sizeof_COFF_header 20


typedef
   struct {
      UChar  Name[8];
      UInt32 VirtualSize;
      UInt32 VirtualAddress;
      UInt32 SizeOfRawData;
      UInt32 PointerToRawData;
      UInt32 PointerToRelocations;
      UInt32 PointerToLinenumbers;
      UInt16 NumberOfRelocations;
      UInt16 NumberOfLineNumbers;
      UInt32 Characteristics;
   }
   COFF_section;

#define sizeof_COFF_section 40


typedef
   struct {
      UChar  Name[8];
      UInt32 Value;
      UInt16 SectionNumber;
      UInt16 Type;
      UChar  StorageClass;
      UChar  NumberOfAuxSymbols;
   }
   COFF_symbol;

#define sizeof_COFF_symbol 18


typedef
   struct {
      UInt32 VirtualAddress;
      UInt32 SymbolTableIndex;
      UInt16 Type;
   }
   COFF_reloc;

#define sizeof_COFF_reloc 10


/* From PE spec doc, section 3.3.2 */
/* Note use of MYIMAGE_* since IMAGE_* are already defined in
   windows.h -- for the same purpose, but I want to know what I'm
   getting, here. */
#define MYIMAGE_FILE_RELOCS_STRIPPED     0x0001
#define MYIMAGE_FILE_EXECUTABLE_IMAGE    0x0002
#define MYIMAGE_FILE_DLL                 0x2000
#define MYIMAGE_FILE_SYSTEM              0x1000
#define MYIMAGE_FILE_BYTES_REVERSED_HI   0x8000
#define MYIMAGE_FILE_BYTES_REVERSED_LO   0x0080
#define MYIMAGE_FILE_32BIT_MACHINE       0x0100

/* From PE spec doc, section 5.4.2 and 5.4.4 */
#define MYIMAGE_SYM_CLASS_EXTERNAL       2
#define MYIMAGE_SYM_CLASS_STATIC         3
#define MYIMAGE_SYM_UNDEFINED            0

/* From PE spec doc, section 4.1 */
#define MYIMAGE_SCN_CNT_CODE             0x00000020
#define MYIMAGE_SCN_CNT_INITIALIZED_DATA 0x00000040
#define MYIMAGE_SCN_LNK_NRELOC_OVFL      0x01000000

/* From PE spec doc, section 5.2.1 */
#define MYIMAGE_REL_I386_DIR32           0x0006
#define MYIMAGE_REL_I386_REL32           0x0014


/* We use myindex to calculate array addresses, rather than
   simply doing the normal subscript thing.  That's because
   some of the above structs have sizes which are not
   a whole number of words.  GCC rounds their sizes up to a
   whole number of words, which means that the address calcs
   arising from using normal C indexing or pointer arithmetic
   are just plain wrong.  Sigh.
*/
static UChar *
myindex ( int scale, void* base, int index )
{
   return
      ((UChar*)base) + scale * index;
}


static void
printName ( UChar* name, UChar* strtab )
{
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      fprintf ( stderr, "%s", strtab + strtab_offset );
   } else {
      int i;
      for (i = 0; i < 8; i++) {
         if (name[i] == 0) break;
         fprintf ( stderr, "%c", name[i] );
      }
   }
}


static void
copyName ( UChar* name, UChar* strtab, UChar* dst, int dstSize )
{
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      strncpy ( dst, strtab+strtab_offset, dstSize );
      dst[dstSize-1] = 0;
   } else {
      int i = 0;
      while (1) {
         if (i >= 8) break;
         if (name[i] == 0) break;
         dst[i] = name[i];
         i++;
      }
      dst[i] = 0;
   }
}


static UChar *
cstring_from_COFF_symbol_name ( UChar* name, UChar* strtab )
{
   UChar* newstr;
   /* If the string is longer than 8 bytes, look in the
      string table for it -- this will be correctly zero terminated.
   */
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      return ((UChar*)strtab) + strtab_offset;
   }
   /* Otherwise, if shorter than 8 bytes, return the original,
      which by defn is correctly terminated.
   */
   if (name[7]==0) return name;
   /* The annoying case: 8 bytes.  Copy into a temporary
      (which is never freed ...)
   */
   newstr = stgMallocBytes(9, "cstring_from_COFF_symbol_name");
   ASSERT(newstr);
   strncpy(newstr,name,8);
   newstr[8] = 0;
   return newstr;
}


/* Just compares the short names (first 8 chars) */
static COFF_section *
findPEi386SectionCalled ( ObjectCode* oc,  char* name )
{
   int i;
   COFF_header* hdr
      = (COFF_header*)(oc->image);
   COFF_section* sectab
      = (COFF_section*) (
           ((UChar*)(oc->image))
           + sizeof_COFF_header + hdr->SizeOfOptionalHeader
        );
   for (i = 0; i < hdr->NumberOfSections; i++) {
      UChar* n1;
      UChar* n2;
      COFF_section* section_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      n1 = (UChar*) &(section_i->Name);
      n2 = name;
      if (n1[0]==n2[0] && n1[1]==n2[1] && n1[2]==n2[2] &&
          n1[3]==n2[3] && n1[4]==n2[4] && n1[5]==n2[5] &&
          n1[6]==n2[6] && n1[7]==n2[7])
         return section_i;
   }

   return NULL;
}


static void
zapTrailingAtSign ( UChar* sym )
{
#  define my_isdigit(c) ((c) >= '0' && (c) <= '9')
   int i, j;
   if (sym[0] == 0) return;
   i = 0;
   while (sym[i] != 0) i++;
   i--;
   j = i;
   while (j > 0 && my_isdigit(sym[j])) j--;
   if (j > 0 && sym[j] == '@' && j != i) sym[j] = 0;
#  undef my_isdigit
}


static int
ocVerifyImage_PEi386 ( ObjectCode* oc )
{
   int i;
   UInt32 j, noRelocs;
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;
   /* fprintf(stderr, "\nLOADING %s\n", oc->fileName); */
   hdr = (COFF_header*)(oc->image);
   sectab = (COFF_section*) (
               ((UChar*)(oc->image))
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->image))
               + hdr->PointerToSymbolTable
            );
   strtab = ((UChar*)symtab)
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   if (hdr->Machine != 0x14c) {
      belch("Not x86 PEi386");
      return 0;
   }
   if (hdr->SizeOfOptionalHeader != 0) {
      belch("PEi386 with nonempty optional header");
      return 0;
   }
   if ( /* (hdr->Characteristics & MYIMAGE_FILE_RELOCS_STRIPPED) || */
        (hdr->Characteristics & MYIMAGE_FILE_EXECUTABLE_IMAGE) ||
        (hdr->Characteristics & MYIMAGE_FILE_DLL) ||
        (hdr->Characteristics & MYIMAGE_FILE_SYSTEM) ) {
      belch("Not a PEi386 object file");
      return 0;
   }
   if ( (hdr->Characteristics & MYIMAGE_FILE_BYTES_REVERSED_HI)
        /* || !(hdr->Characteristics & MYIMAGE_FILE_32BIT_MACHINE) */ ) {
      belch("Invalid PEi386 word size or endiannness: %d",
            (int)(hdr->Characteristics));
      return 0;
   }
   /* If the string table size is way crazy, this might indicate that
      there are more than 64k relocations, despite claims to the
      contrary.  Hence this test. */
   /* fprintf(stderr, "strtab size %d\n", * (UInt32*)strtab); */
#if 0
   if ( (*(UInt32*)strtab) > 600000 ) {
      /* Note that 600k has no special significance other than being
         big enough to handle the almost-2MB-sized lumps that
         constitute HSwin32*.o. */
      belch("PEi386 object has suspiciously large string table; > 64k relocs?");
      return 0;
   }
#endif

   /* No further verification after this point; only debug printing. */
   i = 0;
   IF_DEBUG(linker, i=1);
   if (i == 0) return 1;

   fprintf ( stderr,
             "sectab offset = %d\n", ((UChar*)sectab) - ((UChar*)hdr) );
   fprintf ( stderr,
             "symtab offset = %d\n", ((UChar*)symtab) - ((UChar*)hdr) );
   fprintf ( stderr,
             "strtab offset = %d\n", ((UChar*)strtab) - ((UChar*)hdr) );

   fprintf ( stderr, "\n" );
   fprintf ( stderr,
             "Machine:           0x%x\n", (UInt32)(hdr->Machine) );
   fprintf ( stderr,
             "# sections:        %d\n",   (UInt32)(hdr->NumberOfSections) );
   fprintf ( stderr,
             "time/date:         0x%x\n", (UInt32)(hdr->TimeDateStamp) );
   fprintf ( stderr,
             "symtab offset:     %d\n",   (UInt32)(hdr->PointerToSymbolTable) );
   fprintf ( stderr,
             "# symbols:         %d\n",   (UInt32)(hdr->NumberOfSymbols) );
   fprintf ( stderr,
             "sz of opt hdr:     %d\n",   (UInt32)(hdr->SizeOfOptionalHeader) );
   fprintf ( stderr,
             "characteristics:   0x%x\n", (UInt32)(hdr->Characteristics) );

   /* Print the section table. */
   fprintf ( stderr, "\n" );
   for (i = 0; i < hdr->NumberOfSections; i++) {
      COFF_reloc* reltab;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      fprintf ( stderr,
                "\n"
                "section %d\n"
                "     name `",
                i
              );
      printName ( sectab_i->Name, strtab );
      fprintf ( stderr,
                "'\n"
                "    vsize %d\n"
                "    vaddr %d\n"
                "  data sz %d\n"
                " data off %d\n"
                "  num rel %d\n"
                "  off rel %d\n"
                "  ptr raw 0x%x\n",
                sectab_i->VirtualSize,
                sectab_i->VirtualAddress,
                sectab_i->SizeOfRawData,
                sectab_i->PointerToRawData,
                sectab_i->NumberOfRelocations,
                sectab_i->PointerToRelocations,
                sectab_i->PointerToRawData
              );
      reltab = (COFF_reloc*) (
                  ((UChar*)(oc->image)) + sectab_i->PointerToRelocations
               );

      if ( sectab_i->Characteristics & MYIMAGE_SCN_LNK_NRELOC_OVFL ) {
	/* If the relocation field (a short) has overflowed, the
	 * real count can be found in the first reloc entry.
	 *
	 * See Section 4.1 (last para) of the PE spec (rev6.0).
	 */
        COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, reltab, 0 );
	noRelocs = rel->VirtualAddress;
	j = 1;
      } else {
	noRelocs = sectab_i->NumberOfRelocations;
        j = 0;
      }

      for (; j < noRelocs; j++) {
         COFF_symbol* sym;
         COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, reltab, j );
         fprintf ( stderr,
                   "        type 0x%-4x   vaddr 0x%-8x   name `",
                   (UInt32)rel->Type,
                   rel->VirtualAddress );
         sym = (COFF_symbol*)
               myindex ( sizeof_COFF_symbol, symtab, rel->SymbolTableIndex );
	 /* Hmm..mysterious looking offset - what's it for? SOF */
         printName ( sym->Name, strtab -10 );
         fprintf ( stderr, "'\n" );
      }

      fprintf ( stderr, "\n" );
   }
   fprintf ( stderr, "\n" );
   fprintf ( stderr, "string table has size 0x%x\n", * (UInt32*)strtab );
   fprintf ( stderr, "---START of string table---\n");
   for (i = 4; i < *(Int32*)strtab; i++) {
      if (strtab[i] == 0)
         fprintf ( stderr, "\n"); else
         fprintf( stderr, "%c", strtab[i] );
   }
   fprintf ( stderr, "--- END  of string table---\n");

   fprintf ( stderr, "\n" );
   i = 0;
   while (1) {
      COFF_symbol* symtab_i;
      if (i >= (Int32)(hdr->NumberOfSymbols)) break;
      symtab_i = (COFF_symbol*)
                 myindex ( sizeof_COFF_symbol, symtab, i );
      fprintf ( stderr,
                "symbol %d\n"
                "     name `",
                i
              );
      printName ( symtab_i->Name, strtab );
      fprintf ( stderr,
                "'\n"
                "    value 0x%x\n"
                "   1+sec# %d\n"
                "     type 0x%x\n"
                "   sclass 0x%x\n"
                "     nAux %d\n",
                symtab_i->Value,
                (Int32)(symtab_i->SectionNumber),
                (UInt32)symtab_i->Type,
                (UInt32)symtab_i->StorageClass,
                (UInt32)symtab_i->NumberOfAuxSymbols
              );
      i += symtab_i->NumberOfAuxSymbols;
      i++;
   }

   fprintf ( stderr, "\n" );
   return 1;
}


static int
ocGetNames_PEi386 ( ObjectCode* oc )
{
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;

   UChar* sname;
   void*  addr;
   int    i;

   hdr = (COFF_header*)(oc->image);
   sectab = (COFF_section*) (
               ((UChar*)(oc->image))
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->image))
               + hdr->PointerToSymbolTable
            );
   strtab = ((UChar*)(oc->image))
            + hdr->PointerToSymbolTable
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   /* Allocate space for any (local, anonymous) .bss sections. */

   for (i = 0; i < hdr->NumberOfSections; i++) {
      UChar* zspace;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      if (0 != strcmp(sectab_i->Name, ".bss")) continue;
      if (sectab_i->VirtualSize == 0) continue;
      /* This is a non-empty .bss section.  Allocate zeroed space for
         it, and set its PointerToRawData field such that oc->image +
         PointerToRawData == addr_of_zeroed_space.  */
      zspace = stgCallocBytes(1, sectab_i->VirtualSize,
                              "ocGetNames_PEi386(anonymous bss)");
      sectab_i->PointerToRawData = ((UChar*)zspace) - ((UChar*)(oc->image));
      addProddableBlock(oc, zspace, sectab_i->VirtualSize);
      /* fprintf(stderr, "BSS anon section at 0x%x\n", zspace); */
   }

   /* Copy section information into the ObjectCode. */

   for (i = 0; i < hdr->NumberOfSections; i++) {
      UChar* start;
      UChar* end;
      UInt32 sz;

      SectionKind kind
         = SECTIONKIND_OTHER;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      IF_DEBUG(linker, belch("section name = %s\n", sectab_i->Name ));

#     if 0
      /* I'm sure this is the Right Way to do it.  However, the
         alternative of testing the sectab_i->Name field seems to
         work ok with Cygwin.
      */
      if (sectab_i->Characteristics & MYIMAGE_SCN_CNT_CODE ||
          sectab_i->Characteristics & MYIMAGE_SCN_CNT_INITIALIZED_DATA)
         kind = SECTIONKIND_CODE_OR_RODATA;
#     endif

      if (0==strcmp(".text",sectab_i->Name) ||
          0==strcmp(".rodata",sectab_i->Name))
         kind = SECTIONKIND_CODE_OR_RODATA;
      if (0==strcmp(".data",sectab_i->Name) ||
          0==strcmp(".bss",sectab_i->Name))
         kind = SECTIONKIND_RWDATA;

      ASSERT(sectab_i->SizeOfRawData == 0 || sectab_i->VirtualSize == 0);
      sz = sectab_i->SizeOfRawData;
      if (sz < sectab_i->VirtualSize) sz = sectab_i->VirtualSize;

      start = ((UChar*)(oc->image)) + sectab_i->PointerToRawData;
      end   = start + sz - 1;

      if (kind == SECTIONKIND_OTHER
          /* Ignore sections called which contain stabs debugging
             information. */
          && 0 != strcmp(".stab", sectab_i->Name)
          && 0 != strcmp(".stabstr", sectab_i->Name)
         ) {
         belch("Unknown PEi386 section name `%s'", sectab_i->Name);
         return 0;
      }

      if (kind != SECTIONKIND_OTHER && end >= start) {
         addSection(oc, kind, start, end);
         addProddableBlock(oc, start, end - start + 1);
      }
   }

   /* Copy exported symbols into the ObjectCode. */

   oc->n_symbols = hdr->NumberOfSymbols;
   oc->symbols   = stgMallocBytes(oc->n_symbols * sizeof(char*),
                                  "ocGetNames_PEi386(oc->symbols)");
   /* Call me paranoid; I don't care. */
   for (i = 0; i < oc->n_symbols; i++)
      oc->symbols[i] = NULL;

   i = 0;
   while (1) {
      COFF_symbol* symtab_i;
      if (i >= (Int32)(hdr->NumberOfSymbols)) break;
      symtab_i = (COFF_symbol*)
                 myindex ( sizeof_COFF_symbol, symtab, i );

      addr  = NULL;

      if (symtab_i->StorageClass == MYIMAGE_SYM_CLASS_EXTERNAL
          && symtab_i->SectionNumber != MYIMAGE_SYM_UNDEFINED) {
         /* This symbol is global and defined, viz, exported */
         /* for MYIMAGE_SYMCLASS_EXTERNAL
                && !MYIMAGE_SYM_UNDEFINED,
            the address of the symbol is:
                address of relevant section + offset in section
         */
         COFF_section* sectabent
            = (COFF_section*) myindex ( sizeof_COFF_section,
                                        sectab,
                                        symtab_i->SectionNumber-1 );
         addr = ((UChar*)(oc->image))
                + (sectabent->PointerToRawData
                   + symtab_i->Value);
      }
      else
      if (symtab_i->SectionNumber == MYIMAGE_SYM_UNDEFINED
	  && symtab_i->Value > 0) {
         /* This symbol isn't in any section at all, ie, global bss.
            Allocate zeroed space for it. */
         addr = stgCallocBytes(1, symtab_i->Value,
                               "ocGetNames_PEi386(non-anonymous bss)");
         addSection(oc, SECTIONKIND_RWDATA, addr,
                        ((UChar*)addr) + symtab_i->Value - 1);
         addProddableBlock(oc, addr, symtab_i->Value);
         /* fprintf(stderr, "BSS      section at 0x%x\n", addr); */
      }

      if (addr != NULL ) {
         sname = cstring_from_COFF_symbol_name ( symtab_i->Name, strtab );
         /* fprintf(stderr,"addSymbol %p `%s \n", addr,sname);  */
         IF_DEBUG(linker, belch("addSymbol %p `%s'\n", addr,sname);)
         ASSERT(i >= 0 && i < oc->n_symbols);
         /* cstring_from_COFF_symbol_name always succeeds. */
         oc->symbols[i] = sname;
         ghciInsertStrHashTable(oc->fileName, symhash, sname, addr);
      } else {
#        if 0
         fprintf ( stderr,
                   "IGNORING symbol %d\n"
                   "     name `",
                   i
                 );
         printName ( symtab_i->Name, strtab );
         fprintf ( stderr,
                   "'\n"
                   "    value 0x%x\n"
                   "   1+sec# %d\n"
                   "     type 0x%x\n"
                   "   sclass 0x%x\n"
                   "     nAux %d\n",
                   symtab_i->Value,
                   (Int32)(symtab_i->SectionNumber),
                   (UInt32)symtab_i->Type,
                   (UInt32)symtab_i->StorageClass,
                   (UInt32)symtab_i->NumberOfAuxSymbols
                 );
#        endif
      }

      i += symtab_i->NumberOfAuxSymbols;
      i++;
   }

   return 1;
}


static int
ocResolve_PEi386 ( ObjectCode* oc )
{
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;

   UInt32        A;
   UInt32        S;
   UInt32*       pP;

   int i;
   UInt32 j, noRelocs;

   /* ToDo: should be variable-sized?  But is at least safe in the
      sense of buffer-overrun-proof. */
   char symbol[1000];
   /* fprintf(stderr, "resolving for %s\n", oc->fileName); */

   hdr = (COFF_header*)(oc->image);
   sectab = (COFF_section*) (
               ((UChar*)(oc->image))
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->image))
               + hdr->PointerToSymbolTable
            );
   strtab = ((UChar*)(oc->image))
            + hdr->PointerToSymbolTable
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   for (i = 0; i < hdr->NumberOfSections; i++) {
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      COFF_reloc* reltab
         = (COFF_reloc*) (
              ((UChar*)(oc->image)) + sectab_i->PointerToRelocations
           );

      /* Ignore sections called which contain stabs debugging
         information. */
      if (0 == strcmp(".stab", sectab_i->Name)
          || 0 == strcmp(".stabstr", sectab_i->Name))
         continue;

      if ( sectab_i->Characteristics & MYIMAGE_SCN_LNK_NRELOC_OVFL ) {
	/* If the relocation field (a short) has overflowed, the
	 * real count can be found in the first reloc entry.
         *
	 * See Section 4.1 (last para) of the PE spec (rev6.0).
	 *
	 * Nov2003 update: the GNU linker still doesn't correctly 
	 * handle the generation of relocatable object files with 
	 * overflown relocations. Hence the output to warn of potential 
	 * troubles.
	 */
        COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, reltab, 0 );
	noRelocs = rel->VirtualAddress;
	fprintf(stderr, "WARNING: Overflown relocation field (# relocs found: %u)\n", noRelocs); fflush(stderr);
	j = 1;
      } else {
	noRelocs = sectab_i->NumberOfRelocations;
        j = 0;
      }


      for (; j < noRelocs; j++) {
         COFF_symbol* sym;
         COFF_reloc* reltab_j
            = (COFF_reloc*)
              myindex ( sizeof_COFF_reloc, reltab, j );

         /* the location to patch */
         pP = (UInt32*)(
                 ((UChar*)(oc->image))
                 + (sectab_i->PointerToRawData
                    + reltab_j->VirtualAddress
                    - sectab_i->VirtualAddress )
              );
         /* the existing contents of pP */
         A = *pP;
         /* the symbol to connect to */
         sym = (COFF_symbol*)
               myindex ( sizeof_COFF_symbol,
                         symtab, reltab_j->SymbolTableIndex );
         IF_DEBUG(linker,
                  fprintf ( stderr,
                            "reloc sec %2d num %3d:  type 0x%-4x   "
                            "vaddr 0x%-8x   name `",
                            i, j,
                            (UInt32)reltab_j->Type,
                            reltab_j->VirtualAddress );
                            printName ( sym->Name, strtab );
                            fprintf ( stderr, "'\n" ));

         if (sym->StorageClass == MYIMAGE_SYM_CLASS_STATIC) {
            COFF_section* section_sym
               = findPEi386SectionCalled ( oc, sym->Name );
            if (!section_sym) {
               belch("%s: can't find section `%s'", oc->fileName, sym->Name);
               return 0;
            }
            S = ((UInt32)(oc->image))
                + (section_sym->PointerToRawData
                   + sym->Value);
         } else {
            copyName ( sym->Name, strtab, symbol, 1000-1 );
            (void*)S = lookupLocalSymbol( oc, symbol );
            if ((void*)S != NULL) goto foundit;
            (void*)S = lookupSymbol( symbol );
            if ((void*)S != NULL) goto foundit;
            zapTrailingAtSign ( symbol );
            (void*)S = lookupLocalSymbol( oc, symbol );
            if ((void*)S != NULL) goto foundit;
            (void*)S = lookupSymbol( symbol );
            if ((void*)S != NULL) goto foundit;
	    /* Newline first because the interactive linker has printed "linking..." */
            belch("\n%s: unknown symbol `%s'", oc->fileName, symbol);
            return 0;
           foundit:
         }
         checkProddableBlock(oc, pP);
         switch (reltab_j->Type) {
            case MYIMAGE_REL_I386_DIR32:
               *pP = A + S;
               break;
            case MYIMAGE_REL_I386_REL32:
               /* Tricky.  We have to insert a displacement at
                  pP which, when added to the PC for the _next_
                  insn, gives the address of the target (S).
                  Problem is to know the address of the next insn
                  when we only know pP.  We assume that this
                  literal field is always the last in the insn,
                  so that the address of the next insn is pP+4
                  -- hence the constant 4.
                  Also I don't know if A should be added, but so
                  far it has always been zero.
	       */
               ASSERT(A==0);
               *pP = S - ((UInt32)pP) - 4;
               break;
            default:
               belch("%s: unhandled PEi386 relocation type %d",
		     oc->fileName, reltab_j->Type);
               return 0;
         }

      }
   }

   IF_DEBUG(linker, belch("completed %s", oc->fileName));
   return 1;
}

#endif /* defined(OBJFORMAT_PEi386) */


/* --------------------------------------------------------------------------
 * ELF specifics
 * ------------------------------------------------------------------------*/

#if defined(OBJFORMAT_ELF)

#define FALSE 0
#define TRUE  1

#if defined(sparc_TARGET_ARCH)
#  define ELF_TARGET_SPARC  /* Used inside <elf.h> */
#elif defined(i386_TARGET_ARCH)
#  define ELF_TARGET_386    /* Used inside <elf.h> */
#elif defined(x86_64_TARGET_ARCH)
#  define ELF_TARGET_X64_64
#  define ELF_64BIT
#elif defined (ia64_TARGET_ARCH)
#  define ELF_TARGET_IA64   /* Used inside <elf.h> */
#  define ELF_64BIT
#  define ELF_FUNCTION_DESC /* calling convention uses function descriptors */
#  define ELF_NEED_GOT      /* needs Global Offset Table */
#  define ELF_NEED_PLT      /* needs Procedure Linkage Tables */
#endif

#if !defined(openbsd_TARGET_OS)
#include <elf.h>
#else
/* openbsd elf has things in different places, with diff names */
#include <elf_abi.h>
#include <machine/reloc.h>
#define R_386_32    RELOC_32
#define R_386_PC32  RELOC_PC32
#endif

/*
 * Define a set of types which can be used for both ELF32 and ELF64
 */

#ifdef ELF_64BIT
#define ELFCLASS    ELFCLASS64
#define Elf_Addr    Elf64_Addr
#define Elf_Word    Elf64_Word
#define Elf_Sword   Elf64_Sword
#define Elf_Ehdr    Elf64_Ehdr
#define Elf_Phdr    Elf64_Phdr
#define Elf_Shdr    Elf64_Shdr
#define Elf_Sym     Elf64_Sym
#define Elf_Rel     Elf64_Rel
#define Elf_Rela    Elf64_Rela
#define ELF_ST_TYPE ELF64_ST_TYPE
#define ELF_ST_BIND ELF64_ST_BIND
#define ELF_R_TYPE  ELF64_R_TYPE
#define ELF_R_SYM   ELF64_R_SYM
#else
#define ELFCLASS    ELFCLASS32
#define Elf_Addr    Elf32_Addr
#define Elf_Word    Elf32_Word
#define Elf_Sword   Elf32_Sword
#define Elf_Ehdr    Elf32_Ehdr
#define Elf_Phdr    Elf32_Phdr
#define Elf_Shdr    Elf32_Shdr
#define Elf_Sym     Elf32_Sym
#define Elf_Rel     Elf32_Rel
#define Elf_Rela    Elf32_Rela
#ifndef ELF_ST_TYPE
#define ELF_ST_TYPE ELF32_ST_TYPE
#endif
#ifndef ELF_ST_BIND
#define ELF_ST_BIND ELF32_ST_BIND
#endif
#ifndef ELF_R_TYPE
#define ELF_R_TYPE  ELF32_R_TYPE
#endif
#ifndef ELF_R_SYM
#define ELF_R_SYM   ELF32_R_SYM
#endif
#endif


/*
 * Functions to allocate entries in dynamic sections.  Currently we simply
 * preallocate a large number, and we don't check if a entry for the given
 * target already exists (a linear search is too slow).  Ideally these
 * entries would be associated with symbols.
 */

/* These sizes sufficient to load HSbase + HShaskell98 + a few modules */
#define GOT_SIZE            0x20000
#define FUNCTION_TABLE_SIZE 0x10000
#define PLT_SIZE            0x08000

#ifdef ELF_NEED_GOT
static Elf_Addr got[GOT_SIZE];
static unsigned int gotIndex;
static Elf_Addr gp_val = (Elf_Addr)got;

static Elf_Addr
allocateGOTEntry(Elf_Addr target)
{
   Elf_Addr *entry;

   if (gotIndex >= GOT_SIZE)
      barf("Global offset table overflow");

   entry = &got[gotIndex++];
   *entry = target;
   return (Elf_Addr)entry;
}
#endif

#ifdef ELF_FUNCTION_DESC
typedef struct {
   Elf_Addr ip;
   Elf_Addr gp;
} FunctionDesc;

static FunctionDesc functionTable[FUNCTION_TABLE_SIZE];
static unsigned int functionTableIndex;

static Elf_Addr
allocateFunctionDesc(Elf_Addr target)
{
   FunctionDesc *entry;

   if (functionTableIndex >= FUNCTION_TABLE_SIZE)
      barf("Function table overflow");

   entry = &functionTable[functionTableIndex++];
   entry->ip = target;
   entry->gp = (Elf_Addr)gp_val;
   return (Elf_Addr)entry;
}

static Elf_Addr
copyFunctionDesc(Elf_Addr target)
{
   FunctionDesc *olddesc = (FunctionDesc *)target;
   FunctionDesc *newdesc;

   newdesc = (FunctionDesc *)allocateFunctionDesc(olddesc->ip);
   newdesc->gp = olddesc->gp;
   return (Elf_Addr)newdesc;
}
#endif

#ifdef ELF_NEED_PLT
#ifdef ia64_TARGET_ARCH
static void ia64_reloc_gprel22(Elf_Addr target, Elf_Addr value);
static void ia64_reloc_pcrel21(Elf_Addr target, Elf_Addr value, ObjectCode *oc);

static unsigned char plt_code[] =
{
   /* taken from binutils bfd/elfxx-ia64.c */
   0x0b, 0x78, 0x00, 0x02, 0x00, 0x24,  /*   [MMI]       addl r15=0,r1;;    */
   0x00, 0x41, 0x3c, 0x30, 0x28, 0xc0,  /*               ld8 r16=[r15],8    */
   0x01, 0x08, 0x00, 0x84,              /*               mov r14=r1;;       */
   0x11, 0x08, 0x00, 0x1e, 0x18, 0x10,  /*   [MIB]       ld8 r1=[r15]       */
   0x60, 0x80, 0x04, 0x80, 0x03, 0x00,  /*               mov b6=r16         */
   0x60, 0x00, 0x80, 0x00               /*               br.few b6;;        */
};

/* If we can't get to the function descriptor via gp, take a local copy of it */
#define PLT_RELOC(code, target) { \
   Elf64_Sxword rel_value = target - gp_val; \
   if ((rel_value > 0x1fffff) || (rel_value < -0x1fffff)) \
      ia64_reloc_gprel22((Elf_Addr)code, copyFunctionDesc(target)); \
   else \
      ia64_reloc_gprel22((Elf_Addr)code, target); \
   }
#endif

typedef struct {
   unsigned char code[sizeof(plt_code)];
} PLTEntry;

static Elf_Addr
allocatePLTEntry(Elf_Addr target, ObjectCode *oc)
{
   PLTEntry *plt = (PLTEntry *)oc->plt;
   PLTEntry *entry;

   if (oc->pltIndex >= PLT_SIZE)
      barf("Procedure table overflow");

   entry = &plt[oc->pltIndex++];
   memcpy(entry->code, plt_code, sizeof(entry->code));
   PLT_RELOC(entry->code, target);
   return (Elf_Addr)entry;
}

static unsigned int
PLTSize(void)
{
   return (PLT_SIZE * sizeof(PLTEntry));
}
#endif


/*
 * Generic ELF functions
 */

static char *
findElfSection ( void* objImage, Elf_Word sh_type )
{
   char* ehdrC = (char*)objImage;
   Elf_Ehdr* ehdr = (Elf_Ehdr*)ehdrC;
   Elf_Shdr* shdr = (Elf_Shdr*)(ehdrC + ehdr->e_shoff);
   char* sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   char* ptr = NULL;
   int i;

   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == sh_type
          /* Ignore the section header's string table. */
          && i != ehdr->e_shstrndx
	  /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != memcmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         ptr = ehdrC + shdr[i].sh_offset;
         break;
      }
   }
   return ptr;
}

#if defined(ia64_TARGET_ARCH)
static Elf_Addr
findElfSegment ( void* objImage, Elf_Addr vaddr )
{
   char* ehdrC = (char*)objImage;
   Elf_Ehdr* ehdr = (Elf_Ehdr*)ehdrC;
   Elf_Phdr* phdr = (Elf_Phdr*)(ehdrC + ehdr->e_phoff);
   Elf_Addr segaddr = 0;
   int i;

   for (i = 0; i < ehdr->e_phnum; i++) {
      segaddr = phdr[i].p_vaddr;
      if ((vaddr >= segaddr) && (vaddr < segaddr + phdr[i].p_memsz))
	      break;
   }
   return segaddr;
}
#endif

static int
ocVerifyImage_ELF ( ObjectCode* oc )
{
   Elf_Shdr* shdr;
   Elf_Sym*  stab;
   int i, j, nent, nstrtab, nsymtabs;
   char* sh_strtab;
   char* strtab;

   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*)ehdrC;

   if (ehdr->e_ident[EI_MAG0] != ELFMAG0 ||
       ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
       ehdr->e_ident[EI_MAG2] != ELFMAG2 ||
       ehdr->e_ident[EI_MAG3] != ELFMAG3) {
      belch("%s: not an ELF object", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_CLASS] != ELFCLASS) {
      belch("%s: unsupported ELF format", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_DATA] == ELFDATA2LSB) {
       IF_DEBUG(linker,belch( "Is little-endian" ));
   } else
   if (ehdr->e_ident[EI_DATA] == ELFDATA2MSB) {
       IF_DEBUG(linker,belch( "Is big-endian" ));
   } else {
       belch("%s: unknown endiannness", oc->fileName);
       return 0;
   }

   if (ehdr->e_type != ET_REL) {
      belch("%s: not a relocatable object (.o) file", oc->fileName);
      return 0;
   }
   IF_DEBUG(linker, belch( "Is a relocatable object (.o) file" ));

   IF_DEBUG(linker,belch( "Architecture is " ));
   switch (ehdr->e_machine) {
      case EM_386:   IF_DEBUG(linker,belch( "x86" )); break;
      case EM_SPARC: IF_DEBUG(linker,belch( "sparc" )); break;
#ifdef EM_IA_64
      case EM_IA_64: IF_DEBUG(linker,belch( "ia64" )); break;
#endif
      default:       IF_DEBUG(linker,belch( "unknown" ));
                     belch("%s: unknown architecture", oc->fileName);
                     return 0;
   }

   IF_DEBUG(linker,belch(
             "\nSection header table: start %d, n_entries %d, ent_size %d",
             ehdr->e_shoff, ehdr->e_shnum, ehdr->e_shentsize  ));

   ASSERT (ehdr->e_shentsize == sizeof(Elf_Shdr));

   shdr = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   if (ehdr->e_shstrndx == SHN_UNDEF) {
      belch("%s: no section header string table", oc->fileName);
      return 0;
   } else {
      IF_DEBUG(linker,belch( "Section header string table is section %d",
                          ehdr->e_shstrndx));
      sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   }

   for (i = 0; i < ehdr->e_shnum; i++) {
      IF_DEBUG(linker,fprintf(stderr, "%2d:  ", i ));
      IF_DEBUG(linker,fprintf(stderr, "type=%2d  ", (int)shdr[i].sh_type ));
      IF_DEBUG(linker,fprintf(stderr, "size=%4d  ", (int)shdr[i].sh_size ));
      IF_DEBUG(linker,fprintf(stderr, "offs=%4d  ", (int)shdr[i].sh_offset ));
      IF_DEBUG(linker,fprintf(stderr, "  (%p .. %p)  ",
               ehdrC + shdr[i].sh_offset,
		      ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1));

      if (shdr[i].sh_type == SHT_REL) {
	  IF_DEBUG(linker,fprintf(stderr, "Rel  " ));
      } else if (shdr[i].sh_type == SHT_RELA) {
	  IF_DEBUG(linker,fprintf(stderr, "RelA " ));
      } else {
	  IF_DEBUG(linker,fprintf(stderr,"     "));
      }
      if (sh_strtab) {
	  IF_DEBUG(linker,fprintf(stderr, "sname=%s\n", sh_strtab + shdr[i].sh_name ));
      }
   }

   IF_DEBUG(linker,belch( "\nString tables" ));
   strtab = NULL;
   nstrtab = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == SHT_STRTAB
          /* Ignore the section header's string table. */
          && i != ehdr->e_shstrndx
	  /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != memcmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         IF_DEBUG(linker,belch("   section %d is a normal string table", i ));
         strtab = ehdrC + shdr[i].sh_offset;
         nstrtab++;
      }
   }
   if (nstrtab != 1) {
      belch("%s: no string tables, or too many", oc->fileName);
      return 0;
   }

   nsymtabs = 0;
   IF_DEBUG(linker,belch( "\nSymbol tables" ));
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type != SHT_SYMTAB) continue;
      IF_DEBUG(linker,belch( "section %d is a symbol table", i ));
      nsymtabs++;
      stab = (Elf_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf_Sym);
      IF_DEBUG(linker,belch( "   number of entries is apparently %d (%d rem)",
               nent,
               shdr[i].sh_size % sizeof(Elf_Sym)
             ));
      if (0 != shdr[i].sh_size % sizeof(Elf_Sym)) {
         belch("%s: non-integral number of symbol table entries", oc->fileName);
         return 0;
      }
      for (j = 0; j < nent; j++) {
         IF_DEBUG(linker,fprintf(stderr, "   %2d  ", j ));
         IF_DEBUG(linker,fprintf(stderr, "  sec=%-5d  size=%-3d  val=%5p  ",
                             (int)stab[j].st_shndx,
                             (int)stab[j].st_size,
                             (char*)stab[j].st_value ));

         IF_DEBUG(linker,fprintf(stderr, "type=" ));
         switch (ELF_ST_TYPE(stab[j].st_info)) {
            case STT_NOTYPE:  IF_DEBUG(linker,fprintf(stderr, "notype " )); break;
            case STT_OBJECT:  IF_DEBUG(linker,fprintf(stderr, "object " )); break;
            case STT_FUNC  :  IF_DEBUG(linker,fprintf(stderr, "func   " )); break;
            case STT_SECTION: IF_DEBUG(linker,fprintf(stderr, "section" )); break;
            case STT_FILE:    IF_DEBUG(linker,fprintf(stderr, "file   " )); break;
            default:          IF_DEBUG(linker,fprintf(stderr, "?      " )); break;
         }
         IF_DEBUG(linker,fprintf(stderr, "  " ));

         IF_DEBUG(linker,fprintf(stderr, "bind=" ));
         switch (ELF_ST_BIND(stab[j].st_info)) {
            case STB_LOCAL :  IF_DEBUG(linker,fprintf(stderr, "local " )); break;
            case STB_GLOBAL:  IF_DEBUG(linker,fprintf(stderr, "global" )); break;
            case STB_WEAK  :  IF_DEBUG(linker,fprintf(stderr, "weak  " )); break;
            default:          IF_DEBUG(linker,fprintf(stderr, "?     " )); break;
         }
         IF_DEBUG(linker,fprintf(stderr, "  " ));

         IF_DEBUG(linker,fprintf(stderr, "name=%s\n", strtab + stab[j].st_name ));
      }
   }

   if (nsymtabs == 0) {
      belch("%s: didn't find any symbol tables", oc->fileName);
      return 0;
   }

   return 1;
}


static int
ocGetNames_ELF ( ObjectCode* oc )
{
   int i, j, k, nent;
   Elf_Sym* stab;

   char*     ehdrC    = (char*)(oc->image);
   Elf_Ehdr* ehdr     = (Elf_Ehdr*)ehdrC;
   char*     strtab   = findElfSection ( ehdrC, SHT_STRTAB );
   Elf_Shdr* shdr     = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   ASSERT(symhash != NULL);

   if (!strtab) {
      belch("%s: no strtab", oc->fileName);
      return 0;
   }

   k = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {
      /* Figure out what kind of section it is.  Logic derived from
         Figure 1.14 ("Special Sections") of the ELF document
         ("Portable Formats Specification, Version 1.1"). */
      Elf_Shdr    hdr    = shdr[i];
      SectionKind kind   = SECTIONKIND_OTHER;
      int         is_bss = FALSE;

      if (hdr.sh_type == SHT_PROGBITS
          && (hdr.sh_flags & SHF_ALLOC) && (hdr.sh_flags & SHF_EXECINSTR)) {
         /* .text-style section */
         kind = SECTIONKIND_CODE_OR_RODATA;
      }
      else
      if (hdr.sh_type == SHT_PROGBITS
          && (hdr.sh_flags & SHF_ALLOC) && (hdr.sh_flags & SHF_WRITE)) {
         /* .data-style section */
         kind = SECTIONKIND_RWDATA;
      }
      else
      if (hdr.sh_type == SHT_PROGBITS
          && (hdr.sh_flags & SHF_ALLOC) && !(hdr.sh_flags & SHF_WRITE)) {
         /* .rodata-style section */
         kind = SECTIONKIND_CODE_OR_RODATA;
      }
      else
      if (hdr.sh_type == SHT_NOBITS
          && (hdr.sh_flags & SHF_ALLOC) && (hdr.sh_flags & SHF_WRITE)) {
         /* .bss-style section */
         kind = SECTIONKIND_RWDATA;
         is_bss = TRUE;
      }

      if (is_bss && shdr[i].sh_size > 0) {
         /* This is a non-empty .bss section.  Allocate zeroed space for
            it, and set its .sh_offset field such that
            ehdrC + .sh_offset == addr_of_zeroed_space.  */
         char* zspace = stgCallocBytes(1, shdr[i].sh_size,
                                       "ocGetNames_ELF(BSS)");
         shdr[i].sh_offset = ((char*)zspace) - ((char*)ehdrC);
	 /*
         fprintf(stderr, "BSS section at 0x%x, size %d\n",
                         zspace, shdr[i].sh_size);
	 */
      }

      /* fill in the section info */
      if (kind != SECTIONKIND_OTHER && shdr[i].sh_size > 0) {
         addProddableBlock(oc, ehdrC + shdr[i].sh_offset, shdr[i].sh_size);
         addSection(oc, kind, ehdrC + shdr[i].sh_offset,
                        ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1);
      }

      if (shdr[i].sh_type != SHT_SYMTAB) continue;

      /* copy stuff into this module's object symbol table */
      stab = (Elf_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf_Sym);

      oc->n_symbols = nent;
      oc->symbols = stgMallocBytes(oc->n_symbols * sizeof(char*),
                                   "ocGetNames_ELF(oc->symbols)");

      for (j = 0; j < nent; j++) {

         char  isLocal = FALSE; /* avoids uninit-var warning */
         char* ad      = NULL;
         char* nm      = strtab + stab[j].st_name;
         int   secno   = stab[j].st_shndx;

	 /* Figure out if we want to add it; if so, set ad to its
            address.  Otherwise leave ad == NULL. */

         if (secno == SHN_COMMON) {
            isLocal = FALSE;
            ad = stgCallocBytes(1, stab[j].st_size, "ocGetNames_ELF(COMMON)");
	    /*
            fprintf(stderr, "COMMON symbol, size %d name %s\n",
                            stab[j].st_size, nm);
	    */
	    /* Pointless to do addProddableBlock() for this area,
               since the linker should never poke around in it. */
	 }
         else
         if ( ( ELF_ST_BIND(stab[j].st_info)==STB_GLOBAL
                || ELF_ST_BIND(stab[j].st_info)==STB_LOCAL
              )
              /* and not an undefined symbol */
              && stab[j].st_shndx != SHN_UNDEF
	      /* and not in a "special section" */
              && stab[j].st_shndx < SHN_LORESERVE
              &&
	      /* and it's a not a section or string table or anything silly */
              ( ELF_ST_TYPE(stab[j].st_info)==STT_FUNC ||
                ELF_ST_TYPE(stab[j].st_info)==STT_OBJECT ||
                ELF_ST_TYPE(stab[j].st_info)==STT_NOTYPE
              )
            ) {
	    /* Section 0 is the undefined section, hence > and not >=. */
            ASSERT(secno > 0 && secno < ehdr->e_shnum);
	    /*
            if (shdr[secno].sh_type == SHT_NOBITS) {
               fprintf(stderr, "   BSS symbol, size %d off %d name %s\n",
                               stab[j].st_size, stab[j].st_value, nm);
            }
            */
            ad = ehdrC + shdr[ secno ].sh_offset + stab[j].st_value;
            if (ELF_ST_BIND(stab[j].st_info)==STB_LOCAL) {
               isLocal = TRUE;
            } else {
#ifdef ELF_FUNCTION_DESC
               /* dlsym() and the initialisation table both give us function
		* descriptors, so to be consistent we store function descriptors
		* in the symbol table */
               if (ELF_ST_TYPE(stab[j].st_info) == STT_FUNC)
                   ad = (char *)allocateFunctionDesc((Elf_Addr)ad);
#endif
               IF_DEBUG(linker,belch( "addOTabName(GLOB): %10p  %s %s",
                                      ad, oc->fileName, nm ));
               isLocal = FALSE;
            }
         }

         /* And the decision is ... */

         if (ad != NULL) {
            ASSERT(nm != NULL);
	    oc->symbols[j] = nm;
            /* Acquire! */
            if (isLocal) {
               /* Ignore entirely. */
            } else {
               ghciInsertStrHashTable(oc->fileName, symhash, nm, ad);
            }
         } else {
            /* Skip. */
            IF_DEBUG(linker,belch( "skipping `%s'",
                                   strtab + stab[j].st_name ));
            /*
            fprintf(stderr,
                    "skipping   bind = %d,  type = %d,  shndx = %d   `%s'\n",
                    (int)ELF_ST_BIND(stab[j].st_info),
                    (int)ELF_ST_TYPE(stab[j].st_info),
                    (int)stab[j].st_shndx,
                    strtab + stab[j].st_name
                   );
            */
            oc->symbols[j] = NULL;
         }

      }
   }

   return 1;
}

/* Do ELF relocations which lack an explicit addend.  All x86-linux
   relocations appear to be of this form. */
static int
do_Elf_Rel_relocations ( ObjectCode* oc, char* ehdrC,
                         Elf_Shdr* shdr, int shnum,
                         Elf_Sym*  stab, char* strtab )
{
   int j;
   char *symbol;
   Elf_Word* targ;
   Elf_Rel*  rtab = (Elf_Rel*) (ehdrC + shdr[shnum].sh_offset);
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rel);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   targ  = (Elf_Word*)(ehdrC + shdr[ target_shndx ].sh_offset);
   IF_DEBUG(linker,belch( "relocations for section %d using symtab %d",
                          target_shndx, symtab_shndx ));

   for (j = 0; j < nent; j++) {
      Elf_Addr offset = rtab[j].r_offset;
      Elf_Addr info   = rtab[j].r_info;

      Elf_Addr  P  = ((Elf_Addr)targ) + offset;
      Elf_Word* pP = (Elf_Word*)P;
      Elf_Addr  A  = *pP;
      Elf_Addr  S;
      Elf_Addr  value;

      IF_DEBUG(linker,belch( "Rel entry %3d is raw(%6p %6p)",
                             j, (void*)offset, (void*)info ));
      if (!info) {
         IF_DEBUG(linker,belch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
	 /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            S = (Elf_Addr)
                (ehdrC + shdr[ sym.st_shndx ].sh_offset
                       + stab[ELF_R_SYM(info)].st_value);

	 } else {
            /* No, so look up the name in our global table. */
            symbol = strtab + sym.st_name;
            (void*)S = lookupSymbol( symbol );
	 }
         if (!S) {
            belch("%s: unknown symbol `%s'", oc->fileName, symbol);
	    return 0;
         }
         IF_DEBUG(linker,belch( "`%s' resolves to %p", symbol, (void*)S ));
      }

      IF_DEBUG(linker,belch( "Reloc: P = %p   S = %p   A = %p",
			     (void*)P, (void*)S, (void*)A ));
      checkProddableBlock ( oc, pP );

      value = S + A;

      switch (ELF_R_TYPE(info)) {
#        ifdef i386_TARGET_ARCH
         case R_386_32:   *pP = value;     break;
         case R_386_PC32: *pP = value - P; break;
#        endif
         default:
            belch("%s: unhandled ELF relocation(Rel) type %d\n",
		  oc->fileName, ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

/* Do ELF relocations for which explicit addends are supplied.
   sparc-solaris relocations appear to be of this form. */
static int
do_Elf_Rela_relocations ( ObjectCode* oc, char* ehdrC,
                          Elf_Shdr* shdr, int shnum,
                          Elf_Sym*  stab, char* strtab )
{
   int j;
   char *symbol;
   Elf_Addr targ;
   Elf_Rela* rtab = (Elf_Rela*) (ehdrC + shdr[shnum].sh_offset);
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rela);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   targ  = (Elf_Addr) (ehdrC + shdr[ target_shndx ].sh_offset);
   IF_DEBUG(linker,belch( "relocations for section %d using symtab %d",
                          target_shndx, symtab_shndx ));

   for (j = 0; j < nent; j++) {
#if defined(DEBUG) || defined(sparc_TARGET_ARCH) || defined(ia64_TARGET_ARCH)
      /* This #ifdef only serves to avoid unused-var warnings. */
      Elf_Addr  offset = rtab[j].r_offset;
      Elf_Addr  P      = targ + offset;
#endif
      Elf_Addr  info   = rtab[j].r_info;
      Elf_Addr  A      = rtab[j].r_addend;
      Elf_Addr  S;
      Elf_Addr  value;
#     if defined(sparc_TARGET_ARCH)
      Elf_Word* pP = (Elf_Word*)P;
      Elf_Word  w1, w2;
#     elif defined(ia64_TARGET_ARCH)
      Elf64_Xword *pP = (Elf64_Xword *)P;
      Elf_Addr addr;
#     endif

      IF_DEBUG(linker,belch( "Rel entry %3d is raw(%6p %6p %6p)   ",
                             j, (void*)offset, (void*)info,
                                (void*)A ));
      if (!info) {
         IF_DEBUG(linker,belch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
	 /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            S = (Elf_Addr)
                (ehdrC + shdr[ sym.st_shndx ].sh_offset
                       + stab[ELF_R_SYM(info)].st_value);
#ifdef ELF_FUNCTION_DESC
	    /* Make a function descriptor for this function */
            if (S && ELF_ST_TYPE(sym.st_info) == STT_FUNC) {
               S = allocateFunctionDesc(S + A);
       	       A = 0;
            }
#endif
	 } else {
            /* No, so look up the name in our global table. */
            symbol = strtab + sym.st_name;
            (void*)S = lookupSymbol( symbol );

#ifdef ELF_FUNCTION_DESC
	    /* If a function, already a function descriptor - we would
	       have to copy it to add an offset. */
            if (S && (ELF_ST_TYPE(sym.st_info) == STT_FUNC) && (A != 0))
               belch("%s: function %s with addend %p", oc->fileName, symbol, (void *)A);
#endif
	 }
         if (!S) {
	   belch("%s: unknown symbol `%s'", oc->fileName, symbol);
	   return 0;
         }
         IF_DEBUG(linker,belch( "`%s' resolves to %p", symbol, (void*)S ));
      }

      IF_DEBUG(linker,fprintf ( stderr, "Reloc: P = %p   S = %p   A = %p\n",
                                        (void*)P, (void*)S, (void*)A ));
      /* checkProddableBlock ( oc, (void*)P ); */

      value = S + A;

      switch (ELF_R_TYPE(info)) {
#        if defined(sparc_TARGET_ARCH)
         case R_SPARC_WDISP30:
            w1 = *pP & 0xC0000000;
            w2 = (Elf_Word)((value - P) >> 2);
            ASSERT((w2 & 0xC0000000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_HI22:
            w1 = *pP & 0xFFC00000;
            w2 = (Elf_Word)(value >> 10);
            ASSERT((w2 & 0xFFC00000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_LO10:
            w1 = *pP & ~0x3FF;
            w2 = (Elf_Word)(value & 0x3FF);
            ASSERT((w2 & ~0x3FF) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         /* According to the Sun documentation:
            R_SPARC_UA32
            This relocation type resembles R_SPARC_32, except it refers to an
            unaligned word. That is, the word to be relocated must be treated
            as four separate bytes with arbitrary alignment, not as a word
            aligned according to the architecture requirements.

            (JRS: which means that freeloading on the R_SPARC_32 case
            is probably wrong, but hey ...)
         */
         case R_SPARC_UA32:
         case R_SPARC_32:
            w2 = (Elf_Word)value;
            *pP = w2;
            break;
#        elif defined(ia64_TARGET_ARCH)
	 case R_IA64_DIR64LSB:
	 case R_IA64_FPTR64LSB:
	    *pP = value;
	    break;
	 case R_IA64_PCREL64LSB:
	    *pP = value - P;
	    break;
	 case R_IA64_SEGREL64LSB:
	    addr = findElfSegment(ehdrC, value);
	    *pP = value - addr;
	    break;
	 case R_IA64_GPREL22:
	    ia64_reloc_gprel22(P, value);
	    break;
	 case R_IA64_LTOFF22:
	 case R_IA64_LTOFF22X:
	 case R_IA64_LTOFF_FPTR22:
	    addr = allocateGOTEntry(value);
	    ia64_reloc_gprel22(P, addr);
	    break;
	 case R_IA64_PCREL21B:
	    ia64_reloc_pcrel21(P, S, oc);
	    break;
	 case R_IA64_LDXMOV:
	    /* This goes with R_IA64_LTOFF22X and points to the load to
	     * convert into a move.  We don't implement relaxation. */
	    break;
#        endif
         default:
            belch("%s: unhandled ELF relocation(RelA) type %d\n",
		  oc->fileName, ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

static int
ocResolve_ELF ( ObjectCode* oc )
{
   char *strtab;
   int   shnum, ok;
   Elf_Sym*  stab  = NULL;
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   char* sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;

   /* first find "the" symbol table */
   stab = (Elf_Sym*) findElfSection ( ehdrC, SHT_SYMTAB );

   /* also go find the string table */
   strtab = findElfSection ( ehdrC, SHT_STRTAB );

   if (stab == NULL || strtab == NULL) {
      belch("%s: can't find string or symbol table", oc->fileName);
      return 0;
   }

   /* Process the relocation sections. */
   for (shnum = 0; shnum < ehdr->e_shnum; shnum++) {

      /* Skip sections called ".rel.stab".  These appear to contain
         relocation entries that, when done, make the stabs debugging
         info point at the right places.  We ain't interested in all
         dat jazz, mun. */
      if (0 == memcmp(".rel.stab", sh_strtab + shdr[shnum].sh_name, 9))
         continue;

      if (shdr[shnum].sh_type == SHT_REL ) {
         ok = do_Elf_Rel_relocations ( oc, ehdrC, shdr,
                                       shnum, stab, strtab );
         if (!ok) return ok;
      }
      else
      if (shdr[shnum].sh_type == SHT_RELA) {
         ok = do_Elf_Rela_relocations ( oc, ehdrC, shdr,
                                        shnum, stab, strtab );
         if (!ok) return ok;
      }
   }

   /* Free the local symbol table; we won't need it again. */
   freeHashTable(oc->lochash, NULL);
   oc->lochash = NULL;

   return 1;
}

/*
 * IA64 specifics
 * Instructions are 41 bits long, packed into 128 bit bundles with a 5-bit template
 * at the front.  The following utility functions pack and unpack instructions, and
 * take care of the most common relocations.
 */

#ifdef ia64_TARGET_ARCH

static Elf64_Xword
ia64_extract_instruction(Elf64_Xword *target)
{
   Elf64_Xword w1, w2;
   int slot = (Elf_Addr)target & 3;
   (Elf_Addr)target &= ~3;

   w1 = *target;
   w2 = *(target+1);

   switch (slot)
   {
      case 0:
         return ((w1 >> 5) & 0x1ffffffffff);
      case 1:
         return (w1 >> 46) | ((w2 & 0x7fffff) << 18);
      case 2:
         return (w2 >> 23);
      default:
         barf("ia64_extract_instruction: invalid slot %p", target);
   }
}

static void
ia64_deposit_instruction(Elf64_Xword *target, Elf64_Xword value)
{
   int slot = (Elf_Addr)target & 3;
   (Elf_Addr)target &= ~3;

   switch (slot)
   {
      case 0:
         *target |= value << 5;
         break;
      case 1:
         *target |= value << 46;
         *(target+1) |= value >> 18;
         break;
      case 2:
         *(target+1) |= value << 23;
         break;
   }
}

static void
ia64_reloc_gprel22(Elf_Addr target, Elf_Addr value)
{
   Elf64_Xword instruction;
   Elf64_Sxword rel_value;

   rel_value = value - gp_val;
   if ((rel_value > 0x1fffff) || (rel_value < -0x1fffff))
      barf("GP-relative data out of range (address = 0x%lx, gp = 0x%lx)", value, gp_val);

   instruction = ia64_extract_instruction((Elf64_Xword *)target);
   instruction |= (((rel_value >> 0) & 0x07f) << 13)		/* imm7b */
		    | (((rel_value >> 7) & 0x1ff) << 27)	/* imm9d */
		    | (((rel_value >> 16) & 0x01f) << 22)	/* imm5c */
		    | ((Elf64_Xword)(rel_value < 0) << 36);	/* s */
   ia64_deposit_instruction((Elf64_Xword *)target, instruction);
}

static void
ia64_reloc_pcrel21(Elf_Addr target, Elf_Addr value, ObjectCode *oc)
{
   Elf64_Xword instruction;
   Elf64_Sxword rel_value;
   Elf_Addr entry;

   entry = allocatePLTEntry(value, oc);

   rel_value = (entry >> 4) - (target >> 4);
   if ((rel_value > 0xfffff) || (rel_value < -0xfffff))
      barf("PLT entry too far away (entry = 0x%lx, target = 0x%lx)", entry, target);

   instruction = ia64_extract_instruction((Elf64_Xword *)target);
   instruction |= ((rel_value & 0xfffff) << 13) 		/* imm20b */
	    	    | ((Elf64_Xword)(rel_value < 0) << 36);	/* s */
   ia64_deposit_instruction((Elf64_Xword *)target, instruction);
}

#endif /* ia64 */

#endif /* ELF */

/* --------------------------------------------------------------------------
 * Mach-O specifics
 * ------------------------------------------------------------------------*/

#if defined(OBJFORMAT_MACHO)

/*
  Support for MachO linking on Darwin/MacOS X on PowerPC chips
  by Wolfgang Thaller (wolfgang.thaller@gmx.net)
  
  I hereby formally apologize for the hackish nature of this code.
  Things that need to be done:
  *) implement ocVerifyImage_MachO
  *) add still more sanity checks.
*/


/*
  ocAllocateJumpIslands_MachO
  
  Allocate additional space at the end of the object file image to make room
  for jump islands.
  
  PowerPC relative branch instructions have a 24 bit displacement field.
  As PPC code is always 4-byte-aligned, this yields a +-32MB range.
  If a particular imported symbol is outside this range, we have to redirect
  the jump to a short piece of new code that just loads the 32bit absolute
  address and jumps there.
  This function just allocates space for one 16 byte jump island for every
  undefined symbol in the object file. The code for the islands is filled in by
  makeJumpIsland below.
*/

static const int islandSize = 16;

static int ocAllocateJumpIslands_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    unsigned i;

    for(i=0;i<header->ncmds;i++)
    {
	if(lc->cmd == LC_DYSYMTAB)
        {
	    struct dysymtab_command *dsymLC = (struct dysymtab_command*) lc;
            unsigned long nundefsym = dsymLC->nundefsym;
            oc->island_start_symbol = dsymLC->iundefsym;
            oc->n_islands = nundefsym;
            
            if(nundefsym > 0)
            {
#ifdef USE_MMAP
                #error ocAllocateJumpIslands_MachO doesnt want USE_MMAP to be defined
#else
                oc->image = stgReallocBytes(
                    image, oc->fileSize + islandSize * nundefsym,
                    "ocAllocateJumpIslands_MachO");
#endif                    
                oc->jump_islands = oc->image + oc->fileSize;
                memset(oc->jump_islands, 0, islandSize * nundefsym);
            }
            
            break;  // there can be only one LC_DSYMTAB
        }
	lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }
    return 1;
}

static int ocVerifyImage_MachO(ObjectCode* oc)
{
    // FIXME: do some verifying here
    return 1;
}

static int resolveImports(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC,
    struct section *sect,    // ptr to lazy or non-lazy symbol pointer section
    unsigned long *indirectSyms,
    struct nlist *nlist)
{
    unsigned i;
    
    for(i=0;i*4<sect->size;i++)
    {
	// according to otool, reserved1 contains the first index into the indirect symbol table
	struct nlist *symbol = &nlist[indirectSyms[sect->reserved1+i]];
	char *nm = image + symLC->stroff + symbol->n_un.n_strx;
	void *addr = NULL;
	
	if((symbol->n_type & N_TYPE) == N_UNDF
	    && (symbol->n_type & N_EXT) && (symbol->n_value != 0))
	    addr = (void*) (symbol->n_value);
	else if((addr = lookupLocalSymbol(oc,nm)) != NULL)
	    ;
	else
	    addr = lookupSymbol(nm);
	if(!addr)
	{
	    belch("\n%s: unknown symbol `%s'", oc->fileName, nm);
	    return 0;
	}
	ASSERT(addr);
	checkProddableBlock(oc,((void**)(image + sect->offset)) + i);
	((void**)(image + sect->offset))[i] = addr;
    }
    
    return 1;
}

static void* makeJumpIsland(
    ObjectCode* oc,
    unsigned long symbolNumber,
    void* target)
{
    if(symbolNumber < oc->island_start_symbol ||
        symbolNumber - oc->island_start_symbol > oc->n_islands)
        return NULL;
    symbolNumber -= oc->island_start_symbol;
    
    void *island = (void*) ((char*)oc->jump_islands + islandSize * symbolNumber);
    unsigned long *p = (unsigned long*) island;
    
        // lis r12, hi16(target)
    *p++ = 0x3d800000 | ( ((unsigned long) target) >> 16 );
        // ori r12, r12, lo16(target)
    *p++ = 0x618c0000 | ( ((unsigned long) target) & 0xFFFF ); 
        // mtctr r12
    *p++ = 0x7d8903a6;
        // bctr
    *p++ = 0x4e800420;
    
    return (void*) island;
}

static char* relocateAddress(
    ObjectCode* oc,
    int nSections,
    struct section* sections,
    unsigned long address)
{  
    int i;
    for(i = 0; i < nSections; i++)
    {
        if(sections[i].addr <= address
            && address < sections[i].addr + sections[i].size)
        {
            return oc->image + sections[i].offset + address - sections[i].addr;
        }
    }
    barf("Invalid Mach-O file:"
         "Address out of bounds while relocating object file");
    return NULL;
}

static int relocateSection(
    ObjectCode* oc,
    char *image, 
    struct symtab_command *symLC, struct nlist *nlist,
    int nSections, struct section* sections, struct section *sect)
{
    struct relocation_info *relocs;
    int i,n;
    
    if(!strcmp(sect->sectname,"__la_symbol_ptr"))
	return 1;
    else if(!strcmp(sect->sectname,"__nl_symbol_ptr"))
	return 1;

    n = sect->nreloc;
    relocs = (struct relocation_info*) (image + sect->reloff);
    
    for(i=0;i<n;i++)
    {
	if(relocs[i].r_address & R_SCATTERED)
	{
	    struct scattered_relocation_info *scat =
		(struct scattered_relocation_info*) &relocs[i];
	    
	    if(!scat->r_pcrel)
	    {
		if(scat->r_length == 2)
		{
		    unsigned long word = 0;
		    unsigned long* wordPtr = (unsigned long*) (image + sect->offset + scat->r_address);
		    checkProddableBlock(oc,wordPtr);
		    
		    // Step 1: Figure out what the relocated value should be
		    if(scat->r_type == GENERIC_RELOC_VANILLA)
		    {
		        word = scat->r_value + sect->offset + ((long) image);
		    }
		    else if(scat->r_type == PPC_RELOC_SECTDIFF
		        || scat->r_type == PPC_RELOC_LO16_SECTDIFF
		        || scat->r_type == PPC_RELOC_HI16_SECTDIFF
		        || scat->r_type == PPC_RELOC_HA16_SECTDIFF)
		    {
		        struct scattered_relocation_info *pair =
		                (struct scattered_relocation_info*) &relocs[i+1];
		                
		        if(!pair->r_scattered || pair->r_type != PPC_RELOC_PAIR)
		            barf("Invalid Mach-O file: "
		                 "PPC_RELOC_*_SECTDIFF not followed by PPC_RELOC_PAIR");
		        
		        word = (unsigned long)
		               (relocateAddress(oc, nSections, sections, scat->r_value)
		              - relocateAddress(oc, nSections, sections, pair->r_value));
		        i++;
		    }
		    else
		        continue;  // ignore the others

                    if(scat->r_type == GENERIC_RELOC_VANILLA
                        || scat->r_type == PPC_RELOC_SECTDIFF)
                    {
                        *wordPtr = word;
                    }
                    else if(scat->r_type == PPC_RELOC_LO16_SECTDIFF)
                    {
                        ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
                    }
                    else if(scat->r_type == PPC_RELOC_HI16_SECTDIFF)
                    {
                        ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
                    }
                    else if(scat->r_type == PPC_RELOC_HA16_SECTDIFF)
                    {
                        ((unsigned short*) wordPtr)[1] = ((word >> 16) & 0xFFFF)
                            + ((word & (1<<15)) ? 1 : 0);
                    }
		}
	    }
	    
	    continue; // FIXME: I hope it's OK to ignore all the others.
	}
	else
	{
	    struct relocation_info *reloc = &relocs[i];
	    if(reloc->r_pcrel && !reloc->r_extern)
		continue;
		
	    if(reloc->r_length == 2)
	    {
		unsigned long word = 0;
                unsigned long jumpIsland = 0;
                long offsetToJumpIsland;
                
		unsigned long* wordPtr = (unsigned long*) (image + sect->offset + reloc->r_address);
		checkProddableBlock(oc,wordPtr);
		
		if(reloc->r_type == GENERIC_RELOC_VANILLA)
		{
		    word = *wordPtr;
		}
		else if(reloc->r_type == PPC_RELOC_LO16)
		{
		    word = ((unsigned short*) wordPtr)[1];
		    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
		}
		else if(reloc->r_type == PPC_RELOC_HI16)
		{
		    word = ((unsigned short*) wordPtr)[1] << 16;
		    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF);
		}
		else if(reloc->r_type == PPC_RELOC_HA16)
		{
		    word = ((unsigned short*) wordPtr)[1] << 16;
		    word += ((short)relocs[i+1].r_address & (short)0xFFFF);
		}
		else if(reloc->r_type == PPC_RELOC_BR24)
		{
		    word = *wordPtr;
		    word = (word & 0x03FFFFFC) | (word & 0x02000000) ? 0xFC000000 : 0;
		}


		if(!reloc->r_extern)
		{
		    long delta = 
			sections[reloc->r_symbolnum-1].offset
			- sections[reloc->r_symbolnum-1].addr
			+ ((long) image);
		    
		    word += delta;
		}
		else
		{
		    struct nlist *symbol = &nlist[reloc->r_symbolnum];
		    char *nm = image + symLC->stroff + symbol->n_un.n_strx;
		    word = (unsigned long) (lookupSymbol(nm));
		    if(!word)
		    {
			belch("\nunknown symbol `%s'", nm);
			return 0;
		    }
		    
		    if(reloc->r_pcrel)
                    {
                        jumpIsland = (long) makeJumpIsland(oc,reloc->r_symbolnum,(void*)word);
			word -= ((long)image) + sect->offset + reloc->r_address;
                        if(jumpIsland != 0)
                        {
                            offsetToJumpIsland = jumpIsland
                                - (((long)image) + sect->offset + reloc->r_address);
                        }
                    }
		}
		
		if(reloc->r_type == GENERIC_RELOC_VANILLA)
		{
		    *wordPtr = word;
		    continue;
		}
		else if(reloc->r_type == PPC_RELOC_LO16)
		{
		    ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
		    i++; continue;
		}
		else if(reloc->r_type == PPC_RELOC_HI16)
		{
		    ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
		    i++; continue;
		}
		else if(reloc->r_type == PPC_RELOC_HA16)
		{
		    ((unsigned short*) wordPtr)[1] = ((word >> 16) & 0xFFFF)
			+ ((word & (1<<15)) ? 1 : 0);
		    i++; continue;
		}
		else if(reloc->r_type == PPC_RELOC_BR24)
		{
                    if((long)word > (long)0x01FFFFFF || (long)word < (long)0xFFE00000)
                    {
                        // The branch offset is too large.
                        // Therefore, we try to use a jump island.
                        if(jumpIsland == 0)
                            barf("unconditional relative branch out of range: "
                                 "no jump island available");
                            
                        word = offsetToJumpIsland;
                        if((long)word > (long)0x01FFFFFF || (long)word < (long)0xFFE00000)
                            barf("unconditional relative branch out of range: "
                                 "jump island out of range");
                    }
		    *wordPtr = (*wordPtr & 0xFC000003) | (word & 0x03FFFFFC);
		    continue;
		}
	    }
	    barf("\nunknown relocation %d",reloc->r_type);
	    return 0;
	}
    }
    return 1;
}

static int ocGetNames_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    unsigned i,curSymbol;
    struct segment_command *segLC = NULL;
    struct section *sections;
    struct symtab_command *symLC = NULL;
    struct dysymtab_command *dsymLC = NULL;
    struct nlist *nlist;
    unsigned long commonSize = 0;
    char    *commonStorage = NULL;
    unsigned long commonCounter;

    for(i=0;i<header->ncmds;i++)
    {
	if(lc->cmd == LC_SEGMENT)
	    segLC = (struct segment_command*) lc;
	else if(lc->cmd == LC_SYMTAB)
	    symLC = (struct symtab_command*) lc;
	else if(lc->cmd == LC_DYSYMTAB)
	    dsymLC = (struct dysymtab_command*) lc;
	lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1); 
    nlist = (struct nlist*) (image + symLC->symoff);

    for(i=0;i<segLC->nsects;i++)
    {
        if(sections[i].size == 0)
            continue;
        
        if((sections[i].flags & SECTION_TYPE) == S_ZEROFILL)
        {
            char * zeroFillArea = stgCallocBytes(1,sections[i].size,
                                      "ocGetNames_MachO(common symbols)");
            sections[i].offset = zeroFillArea - image;
        }
    
	if(!strcmp(sections[i].sectname,"__text"))
	    addSection(oc, SECTIONKIND_CODE_OR_RODATA, 
		(void*) (image + sections[i].offset),
		(void*) (image + sections[i].offset + sections[i].size));
	else if(!strcmp(sections[i].sectname,"__const"))
	    addSection(oc, SECTIONKIND_RWDATA, 
		(void*) (image + sections[i].offset),
		(void*) (image + sections[i].offset + sections[i].size));
	else if(!strcmp(sections[i].sectname,"__data"))
	    addSection(oc, SECTIONKIND_RWDATA, 
		(void*) (image + sections[i].offset),
		(void*) (image + sections[i].offset + sections[i].size));
	else if(!strcmp(sections[i].sectname,"__bss")
	        || !strcmp(sections[i].sectname,"__common"))
	    addSection(oc, SECTIONKIND_RWDATA, 
		(void*) (image + sections[i].offset),
		(void*) (image + sections[i].offset + sections[i].size));

        addProddableBlock(oc, (void*) (image + sections[i].offset),
                                        sections[i].size);
    }

	// count external symbols defined here
    oc->n_symbols = 0;
    for(i=dsymLC->iextdefsym;i<dsymLC->iextdefsym+dsymLC->nextdefsym;i++)
    {
	if((nlist[i].n_type & N_TYPE) == N_SECT)
	    oc->n_symbols++;
    }
    for(i=0;i<symLC->nsyms;i++)
    {
	if((nlist[i].n_type & N_TYPE) == N_UNDF
		&& (nlist[i].n_type & N_EXT) && (nlist[i].n_value != 0))
	{
	    commonSize += nlist[i].n_value;
	    oc->n_symbols++;
	}
    }
    oc->symbols = stgMallocBytes(oc->n_symbols * sizeof(char*),
				   "ocGetNames_MachO(oc->symbols)");
    
	// insert symbols into hash table
    for(i=dsymLC->iextdefsym,curSymbol=0;i<dsymLC->iextdefsym+dsymLC->nextdefsym;i++)
    {
	if((nlist[i].n_type & N_TYPE) == N_SECT)
	{
	    char *nm = image + symLC->stroff + nlist[i].n_un.n_strx;
	    ghciInsertStrHashTable(oc->fileName, symhash, nm, image + 
		sections[nlist[i].n_sect-1].offset
		- sections[nlist[i].n_sect-1].addr
		+ nlist[i].n_value);
	    oc->symbols[curSymbol++] = nm;
	}
    }
    
	// insert local symbols into lochash
    for(i=dsymLC->ilocalsym;i<dsymLC->ilocalsym+dsymLC->nlocalsym;i++)
    {
	if((nlist[i].n_type & N_TYPE) == N_SECT)
	{
	    char *nm = image + symLC->stroff + nlist[i].n_un.n_strx;
	    ghciInsertStrHashTable(oc->fileName, oc->lochash, nm, image + 
		sections[nlist[i].n_sect-1].offset
		- sections[nlist[i].n_sect-1].addr
		+ nlist[i].n_value);
	}
    }

    
    commonStorage = stgCallocBytes(1,commonSize,"ocGetNames_MachO(common symbols)");
    commonCounter = (unsigned long)commonStorage;
    for(i=0;i<symLC->nsyms;i++)
    {
	if((nlist[i].n_type & N_TYPE) == N_UNDF
		&& (nlist[i].n_type & N_EXT) && (nlist[i].n_value != 0))
	{
	    char *nm = image + symLC->stroff + nlist[i].n_un.n_strx;
	    unsigned long sz = nlist[i].n_value;
	    
	    nlist[i].n_value = commonCounter;
	    
	    ghciInsertStrHashTable(oc->fileName, symhash, nm, (void*)commonCounter);
	    oc->symbols[curSymbol++] = nm;
	    
	    commonCounter += sz;
	}
    }
    return 1;
}

static int ocResolve_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    unsigned i;
    struct segment_command *segLC = NULL;
    struct section *sections, *la_ptrs = NULL, *nl_ptrs = NULL;
    struct symtab_command *symLC = NULL;
    struct dysymtab_command *dsymLC = NULL;
    struct nlist *nlist;
    unsigned long *indirectSyms;

    for(i=0;i<header->ncmds;i++)
    {
	if(lc->cmd == LC_SEGMENT)
	    segLC = (struct segment_command*) lc;
	else if(lc->cmd == LC_SYMTAB)
	    symLC = (struct symtab_command*) lc;
	else if(lc->cmd == LC_DYSYMTAB)
	    dsymLC = (struct dysymtab_command*) lc;
	lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }
    
    sections = (struct section*) (segLC+1); 
    nlist = (struct nlist*) (image + symLC->symoff);

    for(i=0;i<segLC->nsects;i++)
    {
	if(!strcmp(sections[i].sectname,"__la_symbol_ptr"))
	    la_ptrs = &sections[i];
	else if(!strcmp(sections[i].sectname,"__nl_symbol_ptr"))
	    nl_ptrs = &sections[i];
    }
    
    indirectSyms = (unsigned long*) (image + dsymLC->indirectsymoff);

    if(la_ptrs)
	if(!resolveImports(oc,image,symLC,la_ptrs,indirectSyms,nlist))
	    return 0;
    if(nl_ptrs)
	if(!resolveImports(oc,image,symLC,nl_ptrs,indirectSyms,nlist))
	    return 0;
    
    for(i=0;i<segLC->nsects;i++)
    {
	if(!relocateSection(oc,image,symLC,nlist,segLC->nsects,sections,&sections[i]))
	    return 0;
    }

    /* Free the local symbol table; we won't need it again. */
    freeHashTable(oc->lochash, NULL);
    oc->lochash = NULL;
    
    /*
        Flush the data & instruction caches.
        Because the PPC has split data/instruction caches, we have to
        do that whenever we modify code at runtime.
    */
    {
        int n = (oc->fileSize + islandSize * oc->n_islands) / 4;
        unsigned long *p = (unsigned long*)oc->image;
        while(n--)
        {
            __asm__ volatile ("dcbf 0,%0\n\tsync\n\ticbi 0,%0"
                                : : "r" (p));
            p++;
        }
        __asm__ volatile ("sync\n\tisync");
    }
    return 1;
}

/*
 * The Mach-O object format uses leading underscores. But not everywhere.
 * There is a small number of runtime support functions defined in
 * libcc_dynamic.a whose name does not have a leading underscore.
 * As a consequence, we can't get their address from C code.
 * We have to use inline assembler just to take the address of a function.
 * Yuck.
 */

static void machoInitSymbolsWithoutUnderscore()
{
    void *p;

#undef Sym    
#define Sym(x)						\
    __asm__ ("lis %0,hi16(" #x ")\n\tori %0,%0,lo16(" #x ")" : "=r" (p));	\
    ghciInsertStrHashTable("(GHCi built-in symbols)", symhash, #x, p);
    
    RTS_MACHO_NOUNDERLINE_SYMBOLS

}
#endif

#endif /* !STANDALONE */
