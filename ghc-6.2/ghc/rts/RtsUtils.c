/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The GHC Team, 1998-2002
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

/* gettimeofday isn't POSIX */
/* #include "PosixSource.h" */

#include "Rts.h"
#include "RtsTypes.h"
#include "RtsAPI.h"
#include "RtsFlags.h"
#include "Hooks.h"
#include "RtsUtils.h"
#include "Ticky.h"

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_GETTIMEOFDAY
#include <sys/time.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* variable-argument internal error function. */

void
barf(char *s, ...)
{
#ifndef STANDALONE
  va_list ap;
  va_start(ap,s);
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  if (prog_argv != NULL && prog_name != NULL) {
    fprintf(stderr, "%s: internal error: ", prog_name);
  } else {
    fprintf(stderr, "internal error: ");
  }
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
  fprintf(stderr, "    Please report this as a bug to glasgow-haskell-bugs@haskell.org,\n    or http://www.sourceforge.net/projects/ghc/\n");
  fflush(stderr);
  stg_exit(EXIT_INTERNAL_ERROR);
  va_end(ap);
#else
  (void) kprintf(s, (int *)(&s + 1));
  abort();
#endif
}

void
prog_belch(char *s, ...)
{
#ifndef STANDALONE
  va_list ap;
  va_start(ap,s);
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  if (prog_argv != NULL && prog_name != NULL) {
    fprintf(stderr, "%s: ", prog_name);
  } 
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
  va_end(ap);
#else
  kprintf(s, (int *)(&s + 1));
#endif
}

void
belch(char *s, ...)
{
#ifndef STANDALONE
  va_list ap;
  va_start(ap,s);
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
  va_end(ap);
#else
  kprintf(s, (int *)(&s + 1));
#endif
}

/* result-checking malloc wrappers. */

void *
stgMallocBytes (int n, char *msg)
{
    char *space;

    if ((space = (char *) malloc((size_t) n)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      MallocFailHook((W_) n, msg); /*msg*/
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    return space;
}

void *
stgReallocBytes (void *p, int n, char *msg)
{
    char *space;

    if ((space = (char *) realloc(p, (size_t) n)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      MallocFailHook((W_) n, msg); /*msg*/
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    return space;
}

void *
stgCallocBytes (int n, int m, char *msg)
{
  int   i;
  int   sz = n * m;
  char* p  = stgMallocBytes(sz, msg);
  for (i = 0; i < sz; i++) p[i] = 0;
  return p;
}

/* To simplify changing the underlying allocator used
 * by stgMallocBytes(), provide stgFree() as well.
 */
void
stgFree(void* p)
{
  free(p);
}

void 
_stgAssert (char *filename, unsigned int linenum)
{
#ifndef STANDALONE
  fflush(stdout);
  fprintf(stderr, "ASSERTION FAILED: file %s, line %u\n", filename, linenum);
  fflush(stderr);
  abort();
#else
  kprintf("ASSERTION FAILED: file %s, line %u\n", filename, linenum);
  abort();
#endif
}

/* -----------------------------------------------------------------------------
   Stack overflow
   
   Not sure if this belongs here.
   -------------------------------------------------------------------------- */

void
stackOverflow(void)
{
  StackOverflowHook(RtsFlags.GcFlags.maxStkSize * sizeof(W_));

#if defined(TICKY_TICKY)
  if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif
}

void
heapOverflow(void)
{
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  OutOfHeapHook(0/*unknown request size*/, 
		RtsFlags.GcFlags.maxHeapSize * BLOCK_SIZE);
  
#if defined(TICKY_TICKY)
  if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif

  stg_exit(EXIT_HEAPOVERFLOW);
}

/* -----------------------------------------------------------------------------
   Out-of-line strlen.

   Used in addr2Integer because the C compiler on x86 chokes on
   strlen, trying to inline it with not enough registers available.
   -------------------------------------------------------------------------- */

nat stg_strlen(char *s)
{
   char *p = s;

   while (*p) p++;
   return p-s;
}


/* -----------------------------------------------------------------------------
   genSym stuff, used by GHC itself for its splitting unique supply.

   ToDo: put this somewhere sensible.
   -------------------------------------------------------------------------  */

static I_ __GenSymCounter = 0;

I_
genSymZh(void)
{
    return(__GenSymCounter++);
}
I_
resetGenSymZh(void) /* it's your funeral */
{
    __GenSymCounter=0;
    return(__GenSymCounter);
}

/* -----------------------------------------------------------------------------
   Get the current time as a string.  Used in profiling reports.
   -------------------------------------------------------------------------- */
#if (defined(PROFILING) || defined(DEBUG) || defined(PAR) || defined(GRAN)) \
	&& !defined(STANDALONE)
char *
time_str(void)
{
    static time_t now = 0;
    static char nowstr[26];

    if (now == 0) {
	time(&now);
	strcpy(nowstr, ctime(&now));
	strcpy(nowstr+16,nowstr+19);
	nowstr[21] = '\0';
    }
    return nowstr;
}
#endif

/* -----------------------------------------------------------------------------
 * Reset a file handle to blocking mode.  We do this for the standard
 * file descriptors before exiting, because the shell doesn't always
 * clean up for us.
 * -------------------------------------------------------------------------- */

#if !defined(mingw32_TARGET_OS) && !defined(STANDALONE)
void
resetNonBlockingFd(int fd)
{
  long fd_flags;

  /* clear the non-blocking flag on this file descriptor */
  fd_flags = fcntl(fd, F_GETFL);
  if (fd_flags & O_NONBLOCK) {
    fcntl(fd, F_SETFL, fd_flags & ~O_NONBLOCK);
  }
}

void
setNonBlockingFd(int fd)
{
  long fd_flags;

  /* clear the non-blocking flag on this file descriptor */
  fd_flags = fcntl(fd, F_GETFL);
  if (!(fd_flags & O_NONBLOCK)) {
    fcntl(fd, F_SETFL, fd_flags | O_NONBLOCK);
  }
}
#else
/* Stub defns -- async / non-blocking IO is not done 
 * via O_NONBLOCK and select() under Win32. 
 */
void resetNonBlockingFd(int fd STG_UNUSED) {}
void setNonBlockingFd(int fd STG_UNUSED) {}
#endif

#ifdef PAR
static ullong startTime = 0;

/* used in a parallel setup */
ullong
msTime(void)
{
# if defined(HAVE_GETCLOCK) && !defined(alpha_TARGET_ARCH) && !defined(hppa1_1_TARGET_ARCH)
    struct timespec tv;

    if (getclock(TIMEOFDAY, &tv) != 0) {
	fflush(stdout);
	fprintf(stderr, "Clock failed\n");
	stg_exit(EXIT_FAILURE);
    }
    return tv.tv_sec * LL(1000) + tv.tv_nsec / LL(1000000) - startTime;
# elif HAVE_GETTIMEOFDAY && !defined(alpha_TARGET_ARCH)
    struct timeval tv;
 
    if (gettimeofday(&tv, NULL) != 0) {
	fflush(stdout);
	fprintf(stderr, "Clock failed\n");
	stg_exit(EXIT_FAILURE);
    }
    return tv.tv_sec * LL(1000) + tv.tv_usec / LL(1000) - startTime;
# else
    time_t t;
    if ((t = time(NULL)) == (time_t) -1) {
	fflush(stdout);
	fprintf(stderr, "Clock failed\n");
	stg_exit(EXIT_FAILURE);
    }
    return t * LL(1000) - startTime;
# endif
}
#endif /* PAR */

#ifndef STANDALONE
/* -----------------------------------------------------------------------------
   Print large numbers, with punctuation.
   -------------------------------------------------------------------------- */

char *
ullong_format_string(ullong x, char *s, rtsBool with_commas)
{
    if (x < (ullong)1000) 
	sprintf(s, "%lu", (lnat)x);
    else if (x < (ullong)1000000)
	sprintf(s, (with_commas) ? "%lu,%3.3lu" : "%lu%3.3lu",
		(lnat)((x)/(ullong)1000),
		(lnat)((x)%(ullong)1000));
    else if (x < (ullong)1000000000)
	sprintf(s, (with_commas) ? "%lu,%3.3lu,%3.3lu" :  "%lu%3.3lu%3.3lu",
		(lnat)((x)/(ullong)1000000),
		(lnat)((x)/(ullong)1000%(ullong)1000),
		(lnat)((x)%(ullong)1000));
    else
	sprintf(s, (with_commas) ? "%lu,%3.3lu,%3.3lu,%3.3lu" : "%lu%3.3lu%3.3lu%3.3lu",
		(lnat)((x)/(ullong)1000000000),
		(lnat)((x)/(ullong)1000000%(ullong)1000),
		(lnat)((x)/(ullong)1000%(ullong)1000), 
		(lnat)((x)%(ullong)1000));
    return s;
}
#endif

// Can be used as a breakpoint to set on every heap check failure.
#ifdef DEBUG
void
heapCheckFail( void )
{
}
#endif

