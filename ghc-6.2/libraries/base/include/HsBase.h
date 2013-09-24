/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2001-2002
 *
 * Definitions for package `base' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef __HSBASE_H__
#define __HSBASE_H__

#include "config.h"
#include "HsFFI.h"

#include <math.h>

#if HAVE_LIMITS_H
#include <limits.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif

#ifndef STANDALONE

#include <stdio.h>
#include <stdlib.h>

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_FCNTL_H
# include <fcntl.h>
#endif
#if HAVE_TERMIOS_H
#include <termios.h>
#endif
#if HAVE_SIGNAL_H
#include <signal.h>
/* Ultra-ugly: OpenBSD uses broken macros for sigemptyset and sigfillset (missing casts) */
#if __OpenBSD__
#undef sigemptyset
#undef sigfillset
#endif
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_DIRENT_H
#include <dirent.h>
#endif
#if HAVE_UTIME_H
#include <utime.h>
#endif
#if HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#if HAVE_GETTIMEOFDAY
#  if HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif
#elif HAVE_GETCLOCK
# if HAVE_SYS_TIMERS_H
#  define POSIX_4D9 1
#  include <sys/timers.h>
# endif
#endif
#if HAVE_TIME_H
#include <time.h>
#endif
#if HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#if HAVE_WINSOCK_H && defined(__MINGW32__)
#include <winsock.h>
#endif

#if !defined(mingw32_TARGET_OS) && !defined(irix_TARGET_OS)
# if HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
# endif
#endif

#ifdef hpux_TARGET_OS
#include <sys/syscall.h>
#define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#define HAVE_GETRUSAGE
#endif

/* For System */
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#if HAVE_VFORK_H
#include <vfork.h>
#endif
#include "lockFile.h"
#include "dirUtils.h"

#if defined(__MINGW32__)
#include <io.h>
#include <fcntl.h>
#include "timeUtils.h"
#endif

/* in ghc_errno.c */
int *ghcErrno(void);

/* in system.c */
HsInt systemCmd(HsAddr cmd);

/* in rawSystem.c */
#if defined(mingw32_TARGET_OS)
HsInt rawSystem(HsAddr cmd);
#else
HsInt rawSystem(HsAddr cmd, HsAddr args);
#endif

/* in inputReady.c */
int inputReady(int fd, int msecs, int isSock);

/* in writeError.c */
void writeErrString__(HsAddr msg_hdr, HsAddr msg, HsInt len);

/* in Signals.c */
extern HsInt nocldstop;

#endif

/* -----------------------------------------------------------------------------
   64-bit operations, defined in longlong.c
   -------------------------------------------------------------------------- */

#ifdef SUPPORT_LONG_LONGS

StgInt stg_gtWord64 (StgWord64, StgWord64);
StgInt stg_geWord64 (StgWord64, StgWord64);
StgInt stg_eqWord64 (StgWord64, StgWord64);
StgInt stg_neWord64 (StgWord64, StgWord64);
StgInt stg_ltWord64 (StgWord64, StgWord64);
StgInt stg_leWord64 (StgWord64, StgWord64);

StgInt stg_gtInt64 (StgInt64, StgInt64);
StgInt stg_geInt64 (StgInt64, StgInt64);
StgInt stg_eqInt64 (StgInt64, StgInt64);
StgInt stg_neInt64 (StgInt64, StgInt64);
StgInt stg_ltInt64 (StgInt64, StgInt64);
StgInt stg_leInt64 (StgInt64, StgInt64);

StgWord64 stg_remWord64  (StgWord64, StgWord64);
StgWord64 stg_quotWord64 (StgWord64, StgWord64);

StgInt64 stg_remInt64    (StgInt64, StgInt64);
StgInt64 stg_quotInt64   (StgInt64, StgInt64);
StgInt64 stg_negateInt64 (StgInt64);
StgInt64 stg_plusInt64   (StgInt64, StgInt64);
StgInt64 stg_minusInt64  (StgInt64, StgInt64);
StgInt64 stg_timesInt64  (StgInt64, StgInt64);

StgWord64 stg_and64  (StgWord64, StgWord64);
StgWord64 stg_or64   (StgWord64, StgWord64);
StgWord64 stg_xor64  (StgWord64, StgWord64);
StgWord64 stg_not64  (StgWord64);

StgWord64 stg_uncheckedShiftL64   (StgWord64, StgInt);
StgWord64 stg_uncheckedShiftRL64  (StgWord64, StgInt);
StgInt64  stg_uncheckedIShiftL64  (StgInt64, StgInt);
StgInt64  stg_uncheckedIShiftRL64 (StgInt64, StgInt);
StgInt64  stg_uncheckedIShiftRA64 (StgInt64, StgInt);

StgInt64  stg_intToInt64    (StgInt);
StgInt    stg_int64ToInt    (StgInt64);
StgWord64 stg_int64ToWord64 (StgInt64);

StgWord64 stg_wordToWord64  (StgWord);
StgWord   stg_word64ToWord  (StgWord64);
StgInt64  stg_word64ToInt64 (StgWord64);

StgInt64  stg_integerToInt64 (StgInt sa, StgByteArray /* Really: mp_limb_t* */ da);
StgWord64 stg_integerToWord64 (StgInt sa, StgByteArray /* Really: mp_limb_t* */ da);

#endif /* SUPPORT_LONG_LONGS */

/* -----------------------------------------------------------------------------
   INLINE functions.

   These functions are given as inlines here for when compiling via C,
   but we also generate static versions into the cbits library for
   when compiling to native code.
   -------------------------------------------------------------------------- */

#ifndef INLINE
#define INLINE extern inline
#endif

#ifndef STANDALONE

INLINE int __hscore_s_isreg(m)  { return S_ISREG(m);  }
INLINE int __hscore_s_isdir(m)  { return S_ISDIR(m);  }
INLINE int __hscore_s_isfifo(m) { return S_ISFIFO(m); }
INLINE int __hscore_s_isblk(m)  { return S_ISBLK(m);  }
INLINE int __hscore_s_ischr(m)  { return S_ISCHR(m);  }
#ifdef S_ISSOCK
INLINE int __hscore_s_issock(m) { return S_ISSOCK(m); }
#endif

#ifndef mingw32_TARGET_OS
INLINE int
__hscore_sigemptyset( sigset_t *set )
{ return sigemptyset(set); }

INLINE int
__hscore_sigfillset( sigset_t *set )
{ return sigfillset(set); }

INLINE int
__hscore_sigaddset( sigset_t * set, int s )
{ return sigaddset(set,s); }

INLINE int
__hscore_sigdelset( sigset_t * set, int s )
{ return sigdelset(set,s); }

INLINE int
__hscore_sigismember( sigset_t * set, int s )
{ return sigismember(set,s); }
#endif

INLINE void *
__hscore_memcpy_dst_off( char *dst, int dst_off, char *src, size_t sz )
{ return memcpy(dst+dst_off, src, sz); }

INLINE void *
__hscore_memcpy_src_off( char *dst, char *src, int src_off, size_t sz )
{ return memcpy(dst, src+src_off, sz); }

INLINE HsBool
__hscore_supportsTextMode()
{
#if defined(mingw32_TARGET_OS)
  return HS_BOOL_FALSE;
#else
  return HS_BOOL_TRUE;
#endif
}

INLINE HsInt
__hscore_bufsiz()
{
  return BUFSIZ;
}

INLINE HsInt
__hscore_seek_cur()
{
  return SEEK_CUR;
}

INLINE HsInt
__hscore_o_binary()
{
  return CONST_O_BINARY;
}

INLINE int
__hscore_o_rdonly()
{
#ifdef O_RDONLY
  return O_RDONLY;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_wronly( void )
{
#ifdef O_WRONLY
  return O_WRONLY;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_rdwr( void )
{
#ifdef O_RDWR
  return O_RDWR;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_append( void )
{
#ifdef O_APPEND
  return O_APPEND;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_creat( void )
{
#ifdef O_CREAT
  return O_CREAT;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_excl( void )
{
#ifdef O_EXCL
  return O_EXCL;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_trunc( void )
{
#ifdef O_TRUNC
  return O_TRUNC;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_noctty( void )
{
#ifdef O_NOCTTY
  return O_NOCTTY;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_nonblock( void )
{
#ifdef O_NONBLOCK
  return O_NONBLOCK;
#else
  return 0;
#endif
}

INLINE HsInt
__hscore_seek_set( void )
{
  return SEEK_SET;
}

INLINE HsInt
__hscore_seek_end( void )
{
  return SEEK_END;
}

INLINE HsInt
__hscore_setmode( HsInt fd, HsBool toBin )
{
#if defined(__MINGW32__)
  return setmode(fd,(toBin == HS_BOOL_TRUE) ? _O_BINARY : _O_TEXT);
#else
  return 0;
#endif  
}

INLINE HsInt
__hscore_PrelHandle_write( HsInt fd, HsBool isSock, HsAddr ptr, 
			   HsInt off, int sz )
{
#if defined(__MINGW32__)
  if (isSock) {
    return send(fd,(char *)ptr + off, sz, 0);
  }
#endif
  return write(fd,(char *)ptr + off, sz);
}

INLINE HsInt
__hscore_PrelHandle_read( HsInt fd, HsBool isSock, HsAddr ptr, 
			  HsInt off, int sz )
{
#if defined(__MINGW32__)
  if (isSock) {
    return recv(fd,(char *)ptr + off, sz, 0);
  }
#endif
  return read(fd,(char *)ptr + off, sz);

}

#if defined(__MINGW32__)
INLINE long *
__hscore_Time_ghcTimezone( void ) { return &_timezone; }

INLINE char **
__hscore_Time_ghcTzname( void ) { return _tzname; }
#endif

INLINE HsInt
__hscore_mkdir( HsAddr pathName, HsInt mode )
{
#if defined(__MINGW32__)
  return mkdir(pathName);
#else
  return mkdir(pathName,mode);
#endif
}

INLINE HsInt
__hscore_lstat( HsAddr fname, HsAddr st )
{
#if HAVE_LSTAT
  return lstat((const char*)fname, (struct stat*)st);
#else
  return stat((const char*)fname, (struct stat*)st);
#endif
}

#ifdef PATH_MAX
/* A size that will contain many path names, but not necessarily all
 * (PATH_MAX is not defined on systems with unlimited path length,
 * e.g. the Hurd).
 */
INLINE HsInt __hscore_long_path_size() { return PATH_MAX; }
#else
INLINE HsInt __hscore_long_path_size() { return 4096; }
#endif

INLINE mode_t __hscore_R_OK() { return R_OK; }
INLINE mode_t __hscore_W_OK() { return W_OK; }
INLINE mode_t __hscore_X_OK() { return X_OK; }

INLINE mode_t __hscore_S_IRUSR() { return S_IRUSR; }
INLINE mode_t __hscore_S_IWUSR() { return S_IWUSR; }
INLINE mode_t __hscore_S_IXUSR() { return S_IXUSR; }

INLINE HsAddr
__hscore_d_name( struct dirent* d )
{ 
#ifndef mingw32_TARGET_OS
  return (HsAddr)(&d->d_name);
#else
  return (HsAddr)(d->d_name);
#endif
}

INLINE HsInt
__hscore_end_of_dir( void )
{
#ifndef mingw32_TARGET_OS
  return 0;
#else
  return ENOENT;
#endif  
}

INLINE void
__hscore_free_dirent(HsAddr dEnt)
{
#if HAVE_READDIR_R
  free(dEnt);
#endif
}

INLINE HsInt
__hscore_sizeof_stat( void )
{
  return sizeof(struct stat);
}

INLINE time_t __hscore_st_mtime ( struct stat* st ) { return st->st_mtime; }
INLINE off_t  __hscore_st_size  ( struct stat* st ) { return st->st_size; }
INLINE mode_t __hscore_st_mode  ( struct stat* st ) { return st->st_mode; }

#if HAVE_TERMIOS_H
INLINE tcflag_t __hscore_lflag( struct termios* ts ) { return ts->c_lflag; }

INLINE void
__hscore_poke_lflag( struct termios* ts, tcflag_t t ) { ts->c_lflag = t; }

INLINE unsigned char*
__hscore_ptr_c_cc( struct termios* ts ) 
{ return (unsigned char*) &ts->c_cc; }
#endif

INLINE HsInt
__hscore_sizeof_termios( void )
{
#ifndef mingw32_TARGET_OS
  return sizeof(struct termios);
#else
  return 0;
#endif
}

INLINE HsInt
__hscore_sizeof_sigset_t( void )
{
#ifndef mingw32_TARGET_OS
  return sizeof(sigset_t);
#else
  return 0;
#endif
}

INLINE int
__hscore_echo( void )
{
#ifdef ECHO
  return ECHO;
#else
  return 0;
#endif

}

INLINE int
__hscore_tcsanow( void )
{
#ifdef TCSANOW
  return TCSANOW;
#else
  return 0;
#endif

}

INLINE int
__hscore_icanon( void )
{
#ifdef ICANON
  return ICANON;
#else
  return 0;
#endif
}

INLINE int __hscore_vmin( void )
{
#ifdef VMIN
  return VMIN;
#else
  return 0;
#endif
}

INLINE int __hscore_vtime( void )
{
#ifdef VTIME
  return VTIME;
#else
  return 0;
#endif
}

INLINE int __hscore_sigttou( void )
{
#ifdef SIGTTOU
  return SIGTTOU;
#else
  return 0;
#endif
}

INLINE int __hscore_sig_block( void )
{
#ifdef SIG_BLOCK
  return SIG_BLOCK;
#else
  return 0;
#endif
}

INLINE int __hscore_sig_setmask( void )
{
#ifdef SIG_SETMASK
  return SIG_SETMASK;
#else
  return 0;
#endif
}

INLINE int
__hscore_f_getfl( void )
{
#ifdef F_GETFL
  return F_GETFL;
#else
  return 0;
#endif
}

INLINE int
__hscore_f_setfl( void )
{
#ifdef F_SETFL
  return F_SETFL;
#else
  return 0;
#endif
}

#if HAVE_TERMIOS_H
// defined in rts/RtsStartup.c.
extern struct termios saved_termios[];
#endif

INLINE int __hscore_hs_fileno (FILE *f) { return fileno (f); }

#ifndef mingw32_TARGET_OS
INLINE int __hsposix_SIGABRT()   { return SIGABRT; }
INLINE int __hsposix_SIGALRM()   { return SIGALRM; }
INLINE int __hsposix_SIGBUS()    { return SIGBUS; }
INLINE int __hsposix_SIGCHLD()   { return SIGCHLD; }
INLINE int __hsposix_SIGCONT()   { return SIGCONT; }
INLINE int __hsposix_SIGFPE()    { return SIGFPE; }
INLINE int __hsposix_SIGHUP()    { return SIGHUP; }
INLINE int __hsposix_SIGILL()    { return SIGILL; }
INLINE int __hsposix_SIGINT()    { return SIGINT; }
INLINE int __hsposix_SIGKILL()   { return SIGKILL; }
INLINE int __hsposix_SIGPIPE()   { return SIGPIPE; }
INLINE int __hsposix_SIGQUIT()   { return SIGQUIT; }
INLINE int __hsposix_SIGSEGV()   { return SIGSEGV; }
INLINE int __hsposix_SIGSTOP()   { return SIGSTOP; }
INLINE int __hsposix_SIGTERM()   { return SIGTERM; }
INLINE int __hsposix_SIGTSTP()   { return SIGTSTP; }
INLINE int __hsposix_SIGTTIN()   { return SIGTTIN; }
INLINE int __hsposix_SIGTTOU()   { return SIGTTOU; }
INLINE int __hsposix_SIGUSR1()   { return SIGUSR1; }
INLINE int __hsposix_SIGUSR2()   { return SIGUSR2; }
#if HAVE_SIGPOLL
INLINE int __hsposix_SIGPOLL()   { return SIGPOLL; }
#endif
INLINE int __hsposix_SIGPROF()   { return SIGPROF; }
INLINE int __hsposix_SIGSYS()    { return SIGSYS; }
INLINE int __hsposix_SIGTRAP()   { return SIGTRAP; }
INLINE int __hsposix_SIGURG()    { return SIGURG; }
INLINE int __hsposix_SIGVTALRM() { return SIGVTALRM; }
INLINE int __hsposix_SIGXCPU()   { return SIGXCPU; }
INLINE int __hsposix_SIGXFSZ()   { return SIGXFSZ; }

INLINE int __hsposix_SIG_BLOCK()   { return SIG_BLOCK; }
INLINE int __hsposix_SIG_UNBLOCK() { return SIG_UNBLOCK; }
INLINE int __hsposix_SIG_SETMASK() { return SIG_SETMASK; }
#endif /* mingw32_TARGET_OS */

#endif /* !STANDALONE */

#endif /* __HSBASE_H__ */

