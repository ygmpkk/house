/* -----------------------------------------------------------------------------
 * $Id: TSO.h,v 1.31.2.1 2003/11/10 12:01:51 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * The definitions for Thread State Objects.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TSO_H
#define TSO_H

#if defined(GRAN) || defined(PAR)

#if DEBUG
#define TSO_MAGIC 4321
#endif

typedef struct {
  StgInt   pri;
  StgInt   magic;
  StgInt   sparkname;
  rtsTime  startedat;
  rtsBool  exported;
  StgInt   basicblocks;
  StgInt   allocs;
  rtsTime  exectime;
  rtsTime  fetchtime;
  rtsTime  fetchcount;
  rtsTime  blocktime;
  StgInt   blockcount;
  rtsTime  blockedat;
  StgInt   globalsparks;
  StgInt   localsparks;
  rtsTime  clock;
} StgTSOStatBuf;
#endif

#if defined(PROFILING)
typedef struct {
  CostCentreStack *CCCS;	/* thread's current CCS */
} StgTSOProfInfo;
#else /* !PROFILING */
typedef struct {
} StgTSOProfInfo;
#endif /* PROFILING */

#if defined(PAR)
typedef StgTSOStatBuf StgTSOParInfo;
#else /* !PAR */
typedef struct {
} StgTSOParInfo;
#endif /* PAR */

#if defined(DIST)
typedef struct {
  StgThreadPriority  priority;   
  StgInt             revalTid;   /* ToDo: merge both into 1 word */
  StgInt             revalSlot;
} StgTSODistInfo;
#else /* !DIST */
typedef struct {
} StgTSODistInfo;
#endif /* DIST */

#if defined(GRAN)
typedef StgTSOStatBuf StgTSOGranInfo;
#else /* !GRAN */
typedef struct {
} StgTSOGranInfo;
#endif /* GRAN */


#if defined(TICKY)
typedef struct {
} StgTSOTickyInfo;
#else /* !TICKY_TICKY */
typedef struct {
} StgTSOTickyInfo;
#endif /* TICKY_TICKY */

typedef enum {
    tso_state_runnable,
    tso_state_stopped
} StgTSOState;

/*
 * The what_next field of a TSO indicates how the thread is to be run. 
 */
typedef enum {
  ThreadRunGHC,			/* return to address on top of stack */
  ThreadInterpret,		/* interpret this thread */
  ThreadKilled,			/* thread has died, don't run it */
  ThreadRelocated,		/* thread has moved, link points to new locn */
  ThreadComplete		/* thread has finished */
} StgTSOWhatNext;

/*
 * Thread IDs are 32 bits.
 */
typedef StgWord32 StgThreadID;

/*
 * This type is returned to the scheduler by a thread that has
 * stopped for one reason or another.
 */

typedef enum {
  HeapOverflow,			/* might also be StackOverflow */
  StackOverflow,
  ThreadYielding,
  ThreadBlocked,
  ThreadFinished
} StgThreadReturnCode;

/*
 * We distinguish between the various classes of threads in the system.
 */

typedef enum {
  AdvisoryPriority,
  MandatoryPriority,
  RevalPriority
} StgThreadPriority;

/* 
 * Threads may be blocked for several reasons.  A blocked thread will
 * have the reason in the why_blocked field of the TSO, and some
 * further info (such as the closure the thread is blocked on, or the
 * file descriptor if the thread is waiting on I/O) in the block_info
 * field.
 */

typedef enum {
  NotBlocked,
  BlockedOnMVar,
  BlockedOnBlackHole,
  BlockedOnException,
  BlockedOnRead,
  BlockedOnWrite,
  BlockedOnDelay
#ifdef STANDALONE
  , BlockedWaitingInterrupts
#endif
#if defined(mingw32_TARGET_OS)
  , BlockedOnDoProc
#endif
#if defined(PAR)
  , BlockedOnGA  // blocked on a remote closure represented by a Global Address
  , BlockedOnGA_NoSend // same as above but without sending a Fetch message
#endif
#if defined(RTS_SUPPORTS_THREADS)
  , BlockedOnCCall
  , BlockedOnCCall_NoUnblockExc // same as above but don't unblock async exceptions
  				// in resumeThread()
#endif
} StgTSOBlockReason;

#if defined(mingw32_TARGET_OS)
/* results from an async I/O request + it's ID. */
typedef struct {
  unsigned int reqID;
  int          len;
  int          errCode;
} StgAsyncIOResult;
#endif

typedef union {
  StgClosure *closure;
  struct StgTSO_ *tso;
  int fd;
#if defined(mingw32_TARGET_OS)
  StgAsyncIOResult* async_result;
#endif
  unsigned int target;
} StgTSOBlockInfo;

/*
 * TSOs live on the heap, and therefore look just like heap objects.
 * Large TSOs will live in their own "block group" allocated by the
 * storage manager, and won't be copied during garbage collection.
 */

/* 
 * ToDo: make this structure sensible on a non-32-bit arch.
 */

typedef struct StgTSO_ {
  StgHeader          header;

  struct StgTSO_*    link;	     /* Links threads onto blocking queues */
  StgMutClosure *    mut_link;	     /* TSO's are mutable of course! */
  struct StgTSO_*    global_link;    /* Links all threads together */
  
  StgTSOWhatNext     what_next   : 16;
  StgTSOBlockReason  why_blocked : 16;
  StgTSOBlockInfo    block_info;
  struct StgTSO_*    blocked_exceptions;
  StgThreadID        id;
#ifndef STANDALONE
  int                saved_errno;
#endif
  
  StgTSOTickyInfo    ticky; 
  StgTSOProfInfo     prof;
  StgTSOParInfo      par;
  StgTSOGranInfo     gran;
  StgTSODistInfo     dist;
    
  /* The thread stack... */
  StgWord    	     stack_size;     /* stack size in *words* */
  StgWord            max_stack_size; /* maximum stack size in *words* */
  StgPtr             sp;
  
  StgWord            stack[FLEXIBLE_ARRAY];
} StgTSO;

/* -----------------------------------------------------------------------------
   Invariants:

   An active thread has the following properties:

      tso->stack < tso->sp < tso->stack+tso->stack_size
      tso->stack_size <= tso->max_stack_size
      
      RESERVED_STACK_WORDS is large enough for any heap-check or
      stack-check failure.

      The size of the TSO struct plus the stack is either
        (a) smaller than a block, or
	(b) a multiple of BLOCK_SIZE

	tso->why_blocked       tso->block_info      location
        ----------------------------------------------------------------------
	NotBlocked             NULL                 runnable_queue, or running
	
        BlockedOnBlackHole     the BLACKHOLE_BQ     the BLACKHOLE_BQ's queue
	
        BlockedOnMVar          the MVAR             the MVAR's queue
	
        BlockedOnException     the TSO              TSO->blocked_exception

        BlockedOnRead          NULL                 blocked_queue
        BlockedOnWrite         NULL		    blocked_queue
        BlockedOnDelay         NULL                 blocked_queue
	BlockedOnGA            closure TSO blocks on   BQ of that closure
	BlockedOnGA_NoSend     closure TSO blocks on   BQ of that closure

      tso->link == END_TSO_QUEUE, if the thread is currently running.

   A zombie thread has the following properties:
      
      tso->what_next == ThreadComplete or ThreadKilled
      tso->link     ==  (could be on some queue somewhere)
      tso->su       ==  tso->stack + tso->stack_size
      tso->sp       ==  tso->stack + tso->stack_size - 1 (i.e. top stack word)
      tso->sp[0]    ==  return value of thread, if what_next == ThreadComplete,
                        exception             , if what_next == ThreadKilled

      (tso->sp is left pointing at the top word on the stack so that
      the return value or exception will be retained by a GC).

   tso->blocked_exceptions is either:

      NULL             if async exceptions are unblocked.

      END_TSO_QUEUE    if async exceptions are blocked, but no threads
                       are currently waiting to deliver.

      (StgTSO *)tso    if threads are currently awaiting delivery of
                       exceptions to this thread.

   The 2 cases BlockedOnGA and BlockedOnGA_NoSend are needed in a GUM
   setup only. They mark a TSO that has entered a FETCH_ME or
   FETCH_ME_BQ closure, respectively; only the first TSO hitting the 
   closure will send a Fetch message.
   Currently we have no separate code for blocking on an RBH; we use the
   BlockedOnBlackHole case for that.   -- HWL

 ---------------------------------------------------------------------------- */

/* Workaround for a bug/quirk in gcc on certain architectures.
 * symptom is that (&tso->stack - &tso->header) /=  sizeof(StgTSO)
 * in other words, gcc pads the structure at the end.
 */

extern StgTSO dummy_tso;

#define TSO_STRUCT_SIZE \
   ((char *)&dummy_tso.stack - (char *)&dummy_tso.header)

#define TSO_STRUCT_SIZEW (TSO_STRUCT_SIZE / sizeof(W_))

#endif /* TSO_H */
