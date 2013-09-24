/* ---------------------------------------------------------------------------
 * $Id: Schedule.c,v 1.1 2004/11/29 21:09:26 hallgren Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Scheduler
 *
 * Different GHC ways use this scheduler quite differently (see comments below)
 * Here is the global picture:
 *
 * WAY  Name     CPP flag  What's it for
 * --------------------------------------
 * mp   GUM      PAR          Parallel execution on a distributed memory machine
 * s    SMP      SMP          Parallel execution on a shared memory machine
 * mg   GranSim  GRAN         Simulation of parallel execution
 * md   GUM/GdH  DIST         Distributed execution (based on GUM)
 *
 * --------------------------------------------------------------------------*/

//@node Main scheduling code, , ,
//@section Main scheduling code

/* 
 * Version with scheduler monitor support for SMPs (WAY=s):

   This design provides a high-level API to create and schedule threads etc.
   as documented in the SMP design document.

   It uses a monitor design controlled by a single mutex to exercise control
   over accesses to shared data structures, and builds on the Posix threads
   library.

   The majority of state is shared.  In order to keep essential per-task state,
   there is a Capability structure, which contains all the information
   needed to run a thread: its STG registers, a pointer to its TSO, a
   nursery etc.  During STG execution, a pointer to the capability is
   kept in a register (BaseReg).

   In a non-SMP build, there is one global capability, namely MainRegTable.

   SDM & KH, 10/99

 * Version with support for distributed memory parallelism aka GUM (WAY=mp):

   The main scheduling loop in GUM iterates until a finish message is received.
   In that case a global flag @receivedFinish@ is set and this instance of
   the RTS shuts down. See ghc/rts/parallel/HLComms.c:processMessages()
   for the handling of incoming messages, such as PP_FINISH.
   Note that in the parallel case we have a system manager that coordinates
   different PEs, each of which are running one instance of the RTS.
   See ghc/rts/parallel/SysMan.c for the main routine of the parallel program.
   From this routine processes executing ghc/rts/Main.c are spawned. -- HWL

 * Version with support for simulating parallel execution aka GranSim (WAY=mg):

   The main scheduling code in GranSim is quite different from that in std
   (concurrent) Haskell: while concurrent Haskell just iterates over the
   threads in the runnable queue, GranSim is event driven, i.e. it iterates
   over the events in the global event queue.  -- HWL
*/

//@menu
//* Includes::			
//* Variables and Data structures::  
//* Main scheduling loop::	
//* Suspend and Resume::	
//* Run queue code::		
//* Garbage Collextion Routines::  
//* Blocking Queue Routines::	
//* Exception Handling Routines::  
//* Debugging Routines::	
//* Index::			
//@end menu

//@node Includes, Variables and Data structures, Main scheduling code, Main scheduling code
//@subsection Includes

#include "PosixSource.h"
#include "Rts.h"
#include "SchedAPI.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Storage.h"
#include "StgRun.h"
#include "StgStartup.h"
#include "Hooks.h"
#define COMPILING_SCHEDULER
#include "Schedule.h"
#include "StgMiscClosures.h"
#include "Storage.h"
#include "Interpreter.h"
#include "Exception.h"
#include "Printer.h"
#include "Signals.h"
#include "Sanity.h"
#include "Stats.h"
#include "Timer.h"
#include "Prelude.h"
#include "ThreadLabels.h"
#ifdef PROFILING
#include "Proftimer.h"
#include "ProfHeap.h"
#endif
#if defined(GRAN) || defined(PAR)
# include "GranSimRts.h"
# include "GranSim.h"
# include "ParallelRts.h"
# include "Parallel.h"
# include "ParallelDebug.h"
# include "FetchMe.h"
# include "HLC.h"
#endif
#include "Sparks.h"
#include "Capability.h"
#include "OSThreads.h"
#include  "Task.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef THREADED_RTS
#define USED_IN_THREADED_RTS
#else
#define USED_IN_THREADED_RTS STG_UNUSED
#endif

#ifdef RTS_SUPPORTS_THREADS
#define USED_WHEN_RTS_SUPPORTS_THREADS
#else
#define USED_WHEN_RTS_SUPPORTS_THREADS STG_UNUSED
#endif

//@node Variables and Data structures, Prototypes, Includes, Main scheduling code
//@subsection Variables and Data structures

/* Main thread queue.
 * Locks required: sched_mutex.
 */
StgMainThread *main_threads = NULL;

/* Thread queues.
 * Locks required: sched_mutex.
 */
#if defined(GRAN)

StgTSO* ActiveTSO = NULL; /* for assigning system costs; GranSim-Light only */
/* rtsTime TimeOfNextEvent, EndOfTimeSlice;            now in GranSim.c */

/* 
   In GranSim we have a runnable and a blocked queue for each processor.
   In order to minimise code changes new arrays run_queue_hds/tls
   are created. run_queue_hd is then a short cut (macro) for
   run_queue_hds[CurrentProc] (see GranSim.h).
   -- HWL
*/
StgTSO *run_queue_hds[MAX_PROC], *run_queue_tls[MAX_PROC];
StgTSO *blocked_queue_hds[MAX_PROC], *blocked_queue_tls[MAX_PROC];
StgTSO *ccalling_threadss[MAX_PROC];
/* We use the same global list of threads (all_threads) in GranSim as in
   the std RTS (i.e. we are cheating). However, we don't use this list in
   the GranSim specific code at the moment (so we are only potentially
   cheating).  */

#else /* !GRAN */

StgTSO *run_queue_hd = NULL;
StgTSO *run_queue_tl = NULL;
StgTSO *blocked_queue_hd = NULL;
StgTSO *blocked_queue_tl = NULL;
StgTSO *sleeping_queue = NULL;    /* perhaps replace with a hash table? */

#endif

/* Linked list of all threads.
 * Used for detecting garbage collected threads.
 */
StgTSO *all_threads = NULL;

/* When a thread performs a safe C call (_ccall_GC, using old
 * terminology), it gets put on the suspended_ccalling_threads
 * list. Used by the garbage collector.
 */
static StgTSO *suspended_ccalling_threads;

static StgTSO *threadStackOverflow(StgTSO *tso);

/* KH: The following two flags are shared memory locations.  There is no need
       to lock them, since they are only unset at the end of a scheduler
       operation.
*/

/* flag set by signal handler to precipitate a context switch */
//@cindex context_switch
nat context_switch = 0;

/* if this flag is set as well, give up execution */
//@cindex interrupted
rtsBool interrupted = rtsFalse;

/* Next thread ID to allocate.
 * Locks required: thread_id_mutex
 */
//@cindex next_thread_id
static StgThreadID next_thread_id = 1;

/*
 * Pointers to the state of the current thread.
 * Rule of thumb: if CurrentTSO != NULL, then we're running a Haskell
 * thread.  If CurrentTSO == NULL, then we're at the scheduler level.
 */
 
/* The smallest stack size that makes any sense is:
 *    RESERVED_STACK_WORDS    (so we can get back from the stack overflow)
 *  + sizeofW(StgStopFrame)   (the stg_stop_thread_info frame)
 *  + 1                       (the closure to enter)
 *  + 1			      (stg_ap_v_ret)
 *  + 1			      (spare slot req'd by stg_ap_v_ret)
 *
 * A thread with this stack will bomb immediately with a stack
 * overflow, which will increase its stack size.  
 */

#define MIN_STACK_WORDS (RESERVED_STACK_WORDS + sizeofW(StgStopFrame) + 3)


#if defined(GRAN)
StgTSO *CurrentTSO;
#endif

/*  This is used in `TSO.h' and gcc 2.96 insists that this variable actually 
 *  exists - earlier gccs apparently didn't.
 *  -= chak
 */
StgTSO dummy_tso;

static rtsBool ready_to_gc;

/*
 * Set to TRUE when entering a shutdown state (via shutdownHaskellAndExit()) --
 * in an MT setting, needed to signal that a worker thread shouldn't hang around
 * in the scheduler when it is out of work.
 */
static rtsBool shutting_down_scheduler = rtsFalse;

void            addToBlockedQueue ( StgTSO *tso );

static void     schedule          ( StgMainThread *mainThread, Capability *initialCapability );
       void     interruptStgRts   ( void );

static void     detectBlackHoles  ( void );

#if defined(RTS_SUPPORTS_THREADS)
/* ToDo: carefully document the invariants that go together
 *       with these synchronisation objects.
 */
Mutex     sched_mutex       = INIT_MUTEX_VAR;
Mutex     term_mutex        = INIT_MUTEX_VAR;

/*
 * A heavyweight solution to the problem of protecting
 * the thread_id from concurrent update.
 */
Mutex     thread_id_mutex   = INIT_MUTEX_VAR;


# if defined(SMP)
static Condition gc_pending_cond = INIT_COND_VAR;
nat await_death;
# endif

#endif /* RTS_SUPPORTS_THREADS */

#if defined(PAR)
StgTSO *LastTSO;
rtsTime TimeOfLastYield;
rtsBool emitSchedule = rtsTrue;
#endif

#if DEBUG
static char *whatNext_strs[] = {
  "ThreadRunGHC",
  "ThreadInterpret",
  "ThreadKilled",
  "ThreadRelocated",
  "ThreadComplete"
};
#endif

#if defined(PAR)
StgTSO * createSparkThread(rtsSpark spark);
StgTSO * activateSpark (rtsSpark spark);  
#endif

/*
 * The thread state for the main thread.
// ToDo: check whether not needed any more
StgTSO   *MainTSO;
 */

#if defined(RTS_SUPPORTS_THREADS)
static rtsBool startingWorkerThread = rtsFalse;

static void taskStart(void);
static void
taskStart(void)
{
  Capability *cap;
  
  ACQUIRE_LOCK(&sched_mutex);
  startingWorkerThread = rtsFalse;
  waitForWorkCapability(&sched_mutex, &cap, NULL);
  RELEASE_LOCK(&sched_mutex);
  
  schedule(NULL,cap);
}

void
startSchedulerTaskIfNecessary(void)
{
  if(run_queue_hd != END_TSO_QUEUE
    || blocked_queue_hd != END_TSO_QUEUE
    || sleeping_queue != END_TSO_QUEUE)
  {
    if(!startingWorkerThread)
    { // we don't want to start another worker thread
      // just because the last one hasn't yet reached the
      // "waiting for capability" state
      startingWorkerThread = rtsTrue;
      startTask(taskStart);
    }
  }
}
#endif

//@node Main scheduling loop, Suspend and Resume, Prototypes, Main scheduling code
//@subsection Main scheduling loop

/* ---------------------------------------------------------------------------
   Main scheduling loop.

   We use round-robin scheduling, each thread returning to the
   scheduler loop when one of these conditions is detected:

      * out of heap space
      * timer expires (thread yields)
      * thread blocks
      * thread ends
      * stack overflow

   Locking notes:  we acquire the scheduler lock once at the beginning
   of the scheduler loop, and release it when
    
      * running a thread, or
      * waiting for work, or
      * waiting for a GC to complete.

   GRAN version:
     In a GranSim setup this loop iterates over the global event queue.
     This revolves around the global event queue, which determines what 
     to do next. Therefore, it's more complicated than either the 
     concurrent or the parallel (GUM) setup.

   GUM version:
     GUM iterates over incoming messages.
     It starts with nothing to do (thus CurrentTSO == END_TSO_QUEUE),
     and sends out a fish whenever it has nothing to do; in-between
     doing the actual reductions (shared code below) it processes the
     incoming messages and deals with delayed operations 
     (see PendingFetches).
     This is not the ugliest code you could imagine, but it's bloody close.

   ------------------------------------------------------------------------ */
//@cindex schedule
static void
schedule( StgMainThread *mainThread USED_WHEN_RTS_SUPPORTS_THREADS,
          Capability *initialCapability )
{
  StgTSO *t;
  Capability *cap = initialCapability;
  StgThreadReturnCode ret;
#if defined(GRAN)
  rtsEvent *event;
#elif defined(PAR)
  StgSparkPool *pool;
  rtsSpark spark;
  StgTSO *tso;
  GlobalTaskId pe;
  rtsBool receivedFinish = rtsFalse;
# if defined(DEBUG)
  nat tp_size, sp_size; // stats only
# endif
#endif
  rtsBool was_interrupted = rtsFalse;
  StgTSOWhatNext prev_what_next;
  
  ACQUIRE_LOCK(&sched_mutex);
 
#if defined(RTS_SUPPORTS_THREADS)
  //
  // in the threaded case, the capability is either passed in via the
  // initialCapability parameter, or initialized inside the scheduler
  // loop 
  //
  IF_DEBUG(scheduler,
	   sched_belch("### NEW SCHEDULER LOOP (main thr: %p, cap: %p)",
		       mainThread, initialCapability);
      );
#else
  /* simply initialise it in the non-threaded case */
  grabCapability(&cap);
#endif

#if defined(GRAN)
  /* set up first event to get things going */
  /* ToDo: assign costs for system setup and init MainTSO ! */
  new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc],
	    ContinueThread, 
	    CurrentTSO, (StgClosure*)NULL, (rtsSpark*)NULL);

  IF_DEBUG(gran,
	   fprintf(stderr, "GRAN: Init CurrentTSO (in schedule) = %p\n", CurrentTSO);
	   G_TSO(CurrentTSO, 5));

  if (RtsFlags.GranFlags.Light) {
    /* Save current time; GranSim Light only */
    CurrentTSO->gran.clock = CurrentTime[CurrentProc];
  }      

  event = get_next_event();

  while (event!=(rtsEvent*)NULL) {
    /* Choose the processor with the next event */
    CurrentProc = event->proc;
    CurrentTSO = event->tso;

#elif defined(PAR)

  while (!receivedFinish) {    /* set by processMessages */
                               /* when receiving PP_FINISH message         */ 

#else // everything except GRAN and PAR

  while (1) {

#endif

     IF_DEBUG(scheduler, printAllThreads());

#if defined(RTS_SUPPORTS_THREADS)
    //
    // Check to see whether there are any worker threads
    // waiting to deposit external call results. If so,
    // yield our capability... if we have a capability, that is.
    //
    if (cap != NULL) {
	yieldToReturningWorker(&sched_mutex, &cap,
			       mainThread ? &mainThread->bound_thread_cond
			                  : NULL);
    }

    // If we do not currently hold a capability, we wait for one
    if (cap == NULL) {
	waitForWorkCapability(&sched_mutex, &cap,
			      mainThread ? &mainThread->bound_thread_cond
			                 : NULL);
    }
#endif

    //
    // If we're interrupted (the user pressed ^C, or some other
    // termination condition occurred), kill all the currently running
    // threads.
    //
    if (interrupted) {
	IF_DEBUG(scheduler, sched_belch("interrupted"));
	interrupted = rtsFalse;
	was_interrupted = rtsTrue;
#if defined(RTS_SUPPORTS_THREADS)
	// In the threaded RTS, deadlock detection doesn't work,
	// so just exit right away.
	prog_belch("interrupted");
	releaseCapability(cap);
	RELEASE_LOCK(&sched_mutex);
	shutdownHaskellAndExit(EXIT_SUCCESS);
#else
	deleteAllThreads();
#endif
    }

    //
    // Go through the list of main threads and wake up any
    // clients whose computations have finished.  ToDo: this
    // should be done more efficiently without a linear scan
    // of the main threads list, somehow...
    //
#if defined(RTS_SUPPORTS_THREADS)
    { 
	StgMainThread *m, **prev;
	prev = &main_threads;
	for (m = main_threads; m != NULL; prev = &m->link, m = m->link) {
	  if (m->tso->what_next == ThreadComplete
	      || m->tso->what_next == ThreadKilled)
	  {
	    if (m == mainThread)
	    {
              if (m->tso->what_next == ThreadComplete)
              {
                if (m->ret)
                {
                  // NOTE: return val is tso->sp[1] (see StgStartup.hc)
                  *(m->ret) = (StgClosure *)m->tso->sp[1]; 
                }
                m->stat = Success;
              }
              else
              {
                if (m->ret)
                {
                  *(m->ret) = NULL;
                }
                if (was_interrupted)
                {
                  m->stat = Interrupted;
                }
                else
                {
                  m->stat = Killed;
                }
              }
              *prev = m->link;
	    
#ifdef DEBUG
	      removeThreadLabel((StgWord)m->tso->id);
#endif
              releaseCapability(cap);
              RELEASE_LOCK(&sched_mutex);
              return;
            }
            else
            {
                // The current OS thread can not handle the fact that
                // the Haskell thread "m" has ended.  "m" is bound;
                // the scheduler loop in it's bound OS thread has to
                // return, so let's pass our capability directly to
                // that thread.
		passCapability(&sched_mutex, cap, &m->bound_thread_cond);
		cap = NULL;
            }
          }
	}
    }
    
    // If we gave our capability away, go to the top to get it back
    if (cap == NULL) {
	continue;	
    }
      
#else /* not threaded */

# if defined(PAR)
    /* in GUM do this only on the Main PE */
    if (IAmMainThread)
# endif
    /* If our main thread has finished or been killed, return.
     */
    {
      StgMainThread *m = main_threads;
      if (m->tso->what_next == ThreadComplete
	  || m->tso->what_next == ThreadKilled) {
#ifdef DEBUG
	removeThreadLabel((StgWord)m->tso->id);
#endif
	main_threads = main_threads->link;
	if (m->tso->what_next == ThreadComplete) {
	    // We finished successfully, fill in the return value
	    // NOTE: return val is tso->sp[1] (see StgStartup.hc)
	    if (m->ret) { *(m->ret) = (StgClosure *)m->tso->sp[1]; };
	    m->stat = Success;
	    return;
	} else {
	  if (m->ret) { *(m->ret) = NULL; };
	  if (was_interrupted) {
	    m->stat = Interrupted;
	  } else {
	    m->stat = Killed;
	  }
	  return;
	}
      }
    }
#endif


#if 0 /* defined(SMP) */
    /* Top up the run queue from our spark pool.  We try to make the
     * number of threads in the run queue equal to the number of
     * free capabilities.
     *
     * Disable spark support in SMP for now, non-essential & requires
     * a little bit of work to make it compile cleanly. -- sof 1/02.
     */
    {
      nat n = getFreeCapabilities();
      StgTSO *tso = run_queue_hd;

      /* Count the run queue */
      while (n > 0 && tso != END_TSO_QUEUE) {
	tso = tso->link;
	n--;
      }

      for (; n > 0; n--) {
	StgClosure *spark;
	spark = findSpark(rtsFalse);
	if (spark == NULL) {
	  break; /* no more sparks in the pool */
	} else {
	  /* I'd prefer this to be done in activateSpark -- HWL */
	  /* tricky - it needs to hold the scheduler lock and
	   * not try to re-acquire it -- SDM */
	  createSparkThread(spark);	  
	  IF_DEBUG(scheduler,
		   sched_belch("==^^ turning spark of closure %p into a thread",
			       (StgClosure *)spark));
	}
      }
      /* We need to wake up the other tasks if we just created some
       * work for them.
       */
      if (getFreeCapabilities() - n > 1) {
   	  signalCondition( &thread_ready_cond );
      }
    }
#endif // SMP

#if defined(RTS_USER_SIGNALS)
    // check for signals each time around the scheduler
    if (signals_pending()) {
      RELEASE_LOCK(&sched_mutex); /* ToDo: kill */
      startSignalHandlers();
      ACQUIRE_LOCK(&sched_mutex);
    }
#endif

    /* Check whether any waiting threads need to be woken up.  If the
     * run queue is empty, and there are no other tasks running, we
     * can wait indefinitely for something to happen.
     */
    if ( !EMPTY_QUEUE(blocked_queue_hd) || !EMPTY_QUEUE(sleeping_queue) 
#if defined(RTS_SUPPORTS_THREADS) && !defined(SMP)
		|| EMPTY_RUN_QUEUE()
#endif
        )
    {
      awaitEvent( EMPTY_RUN_QUEUE()
#if defined(SMP)
	&& allFreeCapabilities()
#endif
	);
    }
    /* we can be interrupted while waiting for I/O... */
    if (interrupted) continue;

    /* 
     * Detect deadlock: when we have no threads to run, there are no
     * threads waiting on I/O or sleeping, and all the other tasks are
     * waiting for work, we must have a deadlock of some description.
     *
     * We first try to find threads blocked on themselves (ie. black
     * holes), and generate NonTermination exceptions where necessary.
     *
     * If no threads are black holed, we have a deadlock situation, so
     * inform all the main threads.
     */
#if !defined(PAR) && !defined(RTS_SUPPORTS_THREADS)
    if (   EMPTY_THREAD_QUEUES()
#if defined(RTS_SUPPORTS_THREADS)
	&& EMPTY_QUEUE(suspended_ccalling_threads)
#endif
#ifdef SMP
	&& allFreeCapabilities()
#endif
	)
    {
	IF_DEBUG(scheduler, sched_belch("deadlocked, forcing major GC..."));
#if defined(THREADED_RTS)
	/* and SMP mode ..? */
	releaseCapability(cap);
#endif
	// Garbage collection can release some new threads due to
	// either (a) finalizers or (b) threads resurrected because
	// they are about to be send BlockedOnDeadMVar.  Any threads
	// thus released will be immediately runnable.
	GarbageCollect(GetRoots,rtsTrue);

	if ( !EMPTY_RUN_QUEUE() ) { goto not_deadlocked; }

#ifdef STANDALONE
	/* Standalone means that we have interrupt handlers.
	 * We should then just wait for the next interrupt to arrive.
	 */
	IF_DEBUG(scheduler,
		 sched_belch("still deadlocked, waiting next interrupt..."));
	while ( !EMPTY_RUN_QUEUE() ) {
	  __asm__("hlt");
	  /* FIXME: Does that make sense ? */
	  goto not_deadlocked;
	}
#endif

	IF_DEBUG(scheduler, 
		 sched_belch("still deadlocked, checking for black holes..."));
	detectBlackHoles();

	if ( !EMPTY_RUN_QUEUE() ) { goto not_deadlocked; }

#if defined(RTS_USER_SIGNALS) && (!defined(STANDALONE))
	/* If we have user-installed signal handlers, then wait
	 * for signals to arrive rather then bombing out with a
	 * deadlock.
	 */
#if defined(RTS_SUPPORTS_THREADS) 
	if ( 0 ) { /* hmm..what to do? Simply stop waiting for
		      a signal with no runnable threads (or I/O
		      suspended ones) leads nowhere quick.
		      For now, simply shut down when we reach this
		      condition.
		      
		      ToDo: define precisely under what conditions
		      the Scheduler should shut down in an MT setting.
		   */
#else
	if ( anyUserHandlers() ) {
#endif
	    IF_DEBUG(scheduler, 
		     sched_belch("still deadlocked, waiting for signals..."));

	    awaitUserSignals();

	    // we might be interrupted...
	    if (interrupted) { continue; }

	    if (signals_pending()) {
		RELEASE_LOCK(&sched_mutex);
		startSignalHandlers();
		ACQUIRE_LOCK(&sched_mutex);
	    }
	    ASSERT(!EMPTY_RUN_QUEUE());
	    goto not_deadlocked;
	}
#endif  /* defined(RTS_USER_SIGNALS) */

	/* Probably a real deadlock.  Send the current main thread the
	 * Deadlock exception (or in the SMP build, send *all* main
	 * threads the deadlock exception, since none of them can make
	 * progress).
	 */
	{
	    StgMainThread *m;
#if defined(RTS_SUPPORTS_THREADS)
	    for (m = main_threads; m != NULL; m = m->link) {
		switch (m->tso->why_blocked) {
		case BlockedOnBlackHole:
		    raiseAsync(m->tso, (StgClosure *)NonTermination_closure);
		    break;
		case BlockedOnException:
		case BlockedOnMVar:
		    raiseAsync(m->tso, (StgClosure *)Deadlock_closure);
		    break;
		default:
		    barf("deadlock: main thread blocked in a strange way");
		}
	    }
#else
	    m = main_threads;
	    switch (m->tso->why_blocked) {
	    case BlockedOnBlackHole:
		raiseAsync(m->tso, (StgClosure *)NonTermination_closure);
		break;
	    case BlockedOnException:
	    case BlockedOnMVar:
		raiseAsync(m->tso, (StgClosure *)Deadlock_closure);
		break;
	    default:
		barf("deadlock: main thread blocked in a strange way");
	    }
#endif
	}

#if defined(RTS_SUPPORTS_THREADS)
	/* ToDo: revisit conditions (and mechanism) for shutting
	   down a multi-threaded world  */
	IF_DEBUG(scheduler, sched_belch("all done, i think...shutting down."));
	RELEASE_LOCK(&sched_mutex);
	shutdownHaskell();
	return;
#endif
    }
  not_deadlocked:

#elif defined(RTS_SUPPORTS_THREADS)
    /* ToDo: add deadlock detection in threaded RTS */
#elif defined(PAR)
    /* ToDo: add deadlock detection in GUM (similar to SMP) -- HWL */
#endif

#if defined(SMP)
    /* If there's a GC pending, don't do anything until it has
     * completed.
     */
    if (ready_to_gc) {
      IF_DEBUG(scheduler,sched_belch("waiting for GC"));
      waitCondition( &gc_pending_cond, &sched_mutex );
    }
#endif    

#if defined(RTS_SUPPORTS_THREADS)
#if defined(SMP)
    /* block until we've got a thread on the run queue and a free
     * capability.
     *
     */
    if ( EMPTY_RUN_QUEUE() ) {
      /* Give up our capability */
      releaseCapability(cap);

      /* If we're in the process of shutting down (& running the
       * a batch of finalisers), don't wait around.
       */
      if ( shutting_down_scheduler ) {
	RELEASE_LOCK(&sched_mutex);
	return;
      }
      IF_DEBUG(scheduler, sched_belch("waiting for work"));
      waitForWorkCapability(&sched_mutex, &cap, rtsTrue);
      IF_DEBUG(scheduler, sched_belch("work now available"));
    }
#else
    if ( EMPTY_RUN_QUEUE() ) {
      continue; // nothing to do
    }
#endif
#endif

#if defined(GRAN)
    if (RtsFlags.GranFlags.Light)
      GranSimLight_enter_system(event, &ActiveTSO); // adjust ActiveTSO etc

    /* adjust time based on time-stamp */
    if (event->time > CurrentTime[CurrentProc] &&
        event->evttype != ContinueThread)
      CurrentTime[CurrentProc] = event->time;
    
    /* Deal with the idle PEs (may issue FindWork or MoveSpark events) */
    if (!RtsFlags.GranFlags.Light)
      handleIdlePEs();

    IF_DEBUG(gran, fprintf(stderr, "GRAN: switch by event-type\n"));

    /* main event dispatcher in GranSim */
    switch (event->evttype) {
      /* Should just be continuing execution */
    case ContinueThread:
      IF_DEBUG(gran, fprintf(stderr, "GRAN: doing ContinueThread\n"));
      /* ToDo: check assertion
      ASSERT(run_queue_hd != (StgTSO*)NULL &&
	     run_queue_hd != END_TSO_QUEUE);
      */
      /* Ignore ContinueThreads for fetching threads (if synchr comm) */
      if (!RtsFlags.GranFlags.DoAsyncFetch &&
	  procStatus[CurrentProc]==Fetching) {
	belch("ghuH: Spurious ContinueThread while Fetching ignored; TSO %d (%p) [PE %d]",
	      CurrentTSO->id, CurrentTSO, CurrentProc);
	goto next_thread;
      }	
      /* Ignore ContinueThreads for completed threads */
      if (CurrentTSO->what_next == ThreadComplete) {
	belch("ghuH: found a ContinueThread event for completed thread %d (%p) [PE %d] (ignoring ContinueThread)", 
	      CurrentTSO->id, CurrentTSO, CurrentProc);
	goto next_thread;
      }	
      /* Ignore ContinueThreads for threads that are being migrated */
      if (PROCS(CurrentTSO)==Nowhere) { 
	belch("ghuH: trying to run the migrating TSO %d (%p) [PE %d] (ignoring ContinueThread)",
	      CurrentTSO->id, CurrentTSO, CurrentProc);
	goto next_thread;
      }
      /* The thread should be at the beginning of the run queue */
      if (CurrentTSO!=run_queue_hds[CurrentProc]) { 
	belch("ghuH: TSO %d (%p) [PE %d] is not at the start of the run_queue when doing a ContinueThread",
	      CurrentTSO->id, CurrentTSO, CurrentProc);
	break; // run the thread anyway
      }
      /*
      new_event(proc, proc, CurrentTime[proc],
		FindWork,
		(StgTSO*)NULL, (StgClosure*)NULL, (rtsSpark*)NULL);
      goto next_thread; 
      */ /* Catches superfluous CONTINUEs -- should be unnecessary */
      break; // now actually run the thread; DaH Qu'vam yImuHbej 

    case FetchNode:
      do_the_fetchnode(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case GlobalBlock:
      do_the_globalblock(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case FetchReply:
      do_the_fetchreply(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case UnblockThread:   /* Move from the blocked queue to the tail of */
      do_the_unblock(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case ResumeThread:  /* Move from the blocked queue to the tail of */
      /* the runnable queue ( i.e. Qu' SImqa'lu') */ 
      event->tso->gran.blocktime += 
	CurrentTime[CurrentProc] - event->tso->gran.blockedat;
      do_the_startthread(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case StartThread:
      do_the_startthread(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case MoveThread:
      do_the_movethread(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case MoveSpark:
      do_the_movespark(event);
      goto next_thread;             /* handle next event in event queue  */
      
    case FindWork:
      do_the_findwork(event);
      goto next_thread;             /* handle next event in event queue  */
      
    default:
      barf("Illegal event type %u\n", event->evttype);
    }  /* switch */
    
    /* This point was scheduler_loop in the old RTS */

    IF_DEBUG(gran, belch("GRAN: after main switch"));

    TimeOfLastEvent = CurrentTime[CurrentProc];
    TimeOfNextEvent = get_time_of_next_event();
    IgnoreEvents=(TimeOfNextEvent==0); // HWL HACK
    // CurrentTSO = ThreadQueueHd;

    IF_DEBUG(gran, belch("GRAN: time of next event is: %ld", 
			 TimeOfNextEvent));

    if (RtsFlags.GranFlags.Light) 
      GranSimLight_leave_system(event, &ActiveTSO); 

    EndOfTimeSlice = CurrentTime[CurrentProc]+RtsFlags.GranFlags.time_slice;

    IF_DEBUG(gran, 
	     belch("GRAN: end of time-slice is %#lx", EndOfTimeSlice));

    /* in a GranSim setup the TSO stays on the run queue */
    t = CurrentTSO;
    /* Take a thread from the run queue. */
    t = POP_RUN_QUEUE(); // take_off_run_queue(t);

    IF_DEBUG(gran, 
	     fprintf(stderr, "GRAN: About to run current thread, which is\n");
	     G_TSO(t,5));

    context_switch = 0; // turned on via GranYield, checking events and time slice

    IF_DEBUG(gran, 
	     DumpGranEvent(GR_SCHEDULE, t));

    procStatus[CurrentProc] = Busy;

#elif defined(PAR)
    if (PendingFetches != END_BF_QUEUE) {
        processFetches();
    }

    /* ToDo: phps merge with spark activation above */
    /* check whether we have local work and send requests if we have none */
    if (EMPTY_RUN_QUEUE()) {  /* no runnable threads */
      /* :-[  no local threads => look out for local sparks */
      /* the spark pool for the current PE */
      pool = &(MainRegTable.rSparks); // generalise to cap = &MainRegTable
      if (advisory_thread_count < RtsFlags.ParFlags.maxThreads &&
	  pool->hd < pool->tl) {
	/* 
	 * ToDo: add GC code check that we really have enough heap afterwards!!
	 * Old comment:
	 * If we're here (no runnable threads) and we have pending
	 * sparks, we must have a space problem.  Get enough space
	 * to turn one of those pending sparks into a
	 * thread... 
	 */

	spark = findSpark(rtsFalse);                /* get a spark */
	if (spark != (rtsSpark) NULL) {
	  tso = activateSpark(spark);       /* turn the spark into a thread */
	  IF_PAR_DEBUG(schedule,
		       belch("==== schedule: Created TSO %d (%p); %d threads active",
			     tso->id, tso, advisory_thread_count));

	  if (tso==END_TSO_QUEUE) { /* failed to activate spark->back to loop */
	    belch("==^^ failed to activate spark");
	    goto next_thread;
	  }               /* otherwise fall through & pick-up new tso */
	} else {
	  IF_PAR_DEBUG(verbose,
		       belch("==^^ no local sparks (spark pool contains only NFs: %d)", 
			     spark_queue_len(pool)));
	  goto next_thread;
	}
      }

      /* If we still have no work we need to send a FISH to get a spark
	 from another PE 
      */
      if (EMPTY_RUN_QUEUE()) {
      /* =8-[  no local sparks => look for work on other PEs */
	/*
	 * We really have absolutely no work.  Send out a fish
	 * (there may be some out there already), and wait for
	 * something to arrive.  We clearly can't run any threads
	 * until a SCHEDULE or RESUME arrives, and so that's what
	 * we're hoping to see.  (Of course, we still have to
	 * respond to other types of messages.)
	 */
	TIME now = msTime() /*CURRENT_TIME*/;
	IF_PAR_DEBUG(verbose, 
		     belch("--  now=%ld", now));
	IF_PAR_DEBUG(verbose,
		     if (outstandingFishes < RtsFlags.ParFlags.maxFishes &&
			 (last_fish_arrived_at!=0 &&
			  last_fish_arrived_at+RtsFlags.ParFlags.fishDelay > now)) {
		       belch("--$$ delaying FISH until %ld (last fish %ld, delay %ld, now %ld)",
			     last_fish_arrived_at+RtsFlags.ParFlags.fishDelay,
			     last_fish_arrived_at,
			     RtsFlags.ParFlags.fishDelay, now);
		     });
	
	if (outstandingFishes < RtsFlags.ParFlags.maxFishes &&
	    (last_fish_arrived_at==0 ||
	     (last_fish_arrived_at+RtsFlags.ParFlags.fishDelay <= now))) {
	  /* outstandingFishes is set in sendFish, processFish;
	     avoid flooding system with fishes via delay */
	  pe = choosePE();
	  sendFish(pe, mytid, NEW_FISH_AGE, NEW_FISH_HISTORY, 
		   NEW_FISH_HUNGER);

	  // Global statistics: count no. of fishes
	  if (RtsFlags.ParFlags.ParStats.Global &&
	      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	    globalParStats.tot_fish_mess++;
	  }
	}
      
	receivedFinish = processMessages();
	goto next_thread;
      }
    } else if (PacketsWaiting()) {  /* Look for incoming messages */
      receivedFinish = processMessages();
    }

    /* Now we are sure that we have some work available */
    ASSERT(run_queue_hd != END_TSO_QUEUE);

    /* Take a thread from the run queue, if we have work */
    t = POP_RUN_QUEUE();  // take_off_run_queue(END_TSO_QUEUE);
    IF_DEBUG(sanity,checkTSO(t));

    /* ToDo: write something to the log-file
    if (RTSflags.ParFlags.granSimStats && !sameThread)
        DumpGranEvent(GR_SCHEDULE, RunnableThreadsHd);

    CurrentTSO = t;
    */
    /* the spark pool for the current PE */
    pool = &(MainRegTable.rSparks); // generalise to cap = &MainRegTable

    IF_DEBUG(scheduler, 
	     belch("--=^ %d threads, %d sparks on [%#x]", 
		   run_queue_len(), spark_queue_len(pool), CURRENT_PROC));

# if 1
    if (0 && RtsFlags.ParFlags.ParStats.Full && 
	t && LastTSO && t->id != LastTSO->id && 
	LastTSO->why_blocked == NotBlocked && 
	LastTSO->what_next != ThreadComplete) {
      // if previously scheduled TSO not blocked we have to record the context switch
      DumpVeryRawGranEvent(TimeOfLastYield, CURRENT_PROC, CURRENT_PROC,
			   GR_DESCHEDULE, LastTSO, (StgClosure *)NULL, 0, 0);
    }

    if (RtsFlags.ParFlags.ParStats.Full && 
	(emitSchedule /* forced emit */ ||
        (t && LastTSO && t->id != LastTSO->id))) {
      /* 
	 we are running a different TSO, so write a schedule event to log file
	 NB: If we use fair scheduling we also have to write  a deschedule 
	     event for LastTSO; with unfair scheduling we know that the
	     previous tso has blocked whenever we switch to another tso, so
	     we don't need it in GUM for now
      */
      DumpRawGranEvent(CURRENT_PROC, CURRENT_PROC,
		       GR_SCHEDULE, t, (StgClosure *)NULL, 0, 0);
      emitSchedule = rtsFalse;
    }
     
# endif
#else /* !GRAN && !PAR */
  
    // grab a thread from the run queue
    ASSERT(run_queue_hd != END_TSO_QUEUE);
    t = POP_RUN_QUEUE();
    // Sanity check the thread we're about to run.  This can be
    // expensive if there is lots of thread switching going on...
    IF_DEBUG(sanity,checkTSO(t));
#endif

#ifdef THREADED_RTS
    {
      StgMainThread *m;
      for(m = main_threads; m; m = m->link)
      {
	if(m->tso == t)
	  break;
      }
      
      if(m)
      {
	if(m == mainThread)
	{
	  IF_DEBUG(scheduler,
	    sched_belch("### Running thread %d in bound thread", t->id));
	  // yes, the Haskell thread is bound to the current native thread
	}
	else
	{
	  IF_DEBUG(scheduler,
	    sched_belch("### thread %d bound to another OS thread", t->id));
	  // no, bound to a different Haskell thread: pass to that thread
	  PUSH_ON_RUN_QUEUE(t);
	  passCapability(&sched_mutex,cap,&m->bound_thread_cond);
	  cap = NULL;
	  continue;
	}
      }
      else
      {
	if(mainThread != NULL)
        // The thread we want to run is bound.
	{
	  IF_DEBUG(scheduler,
	    sched_belch("### this OS thread cannot run thread %d", t->id));
	  // no, the current native thread is bound to a different
	  // Haskell thread, so pass it to any worker thread
	  PUSH_ON_RUN_QUEUE(t);
	  passCapabilityToWorker(&sched_mutex, cap);
	  cap = NULL;
	  continue; 
	}
      }
    }
#endif

    cap->r.rCurrentTSO = t;
    
    /* context switches are now initiated by the timer signal, unless
     * the user specified "context switch as often as possible", with
     * +RTS -C0
     */
    if ((RtsFlags.ConcFlags.ctxtSwitchTicks == 0
	 && (run_queue_hd != END_TSO_QUEUE
	     || blocked_queue_hd != END_TSO_QUEUE
	     || sleeping_queue != END_TSO_QUEUE)))
	context_switch = 1;
    else
	context_switch = 0;

run_thread:

    RELEASE_LOCK(&sched_mutex);

    IF_DEBUG(scheduler, sched_belch("-->> running thread %ld %s ...", 
			      t->id, whatNext_strs[t->what_next]));

#ifdef PROFILING
    startHeapProfTimer();
#endif

    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    /* Run the current thread 
     */
    prev_what_next = t->what_next;
    switch (prev_what_next) {
    case ThreadKilled:
    case ThreadComplete:
	/* Thread already finished, return to scheduler. */
	ret = ThreadFinished;
	break;
    case ThreadRunGHC:
#ifndef STANDALONE
	errno = t->saved_errno;
#endif
	ret = StgRun((StgFunPtr) stg_returnToStackTop, &cap->r);
#ifndef STANDALONE
	t->saved_errno = errno;
#endif
	break;
    case ThreadInterpret:
	ret = interpretBCO(cap);
	break;
    default:
      barf("schedule: invalid what_next field");
    }
    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    
    /* Costs for the scheduler are assigned to CCS_SYSTEM */
#ifdef PROFILING
    stopHeapProfTimer();
    CCCS = CCS_SYSTEM;
#endif
    
    ACQUIRE_LOCK(&sched_mutex);
    
#ifdef RTS_SUPPORTS_THREADS
    IF_DEBUG(scheduler,fprintf(stderr,"sched (task %p): ", osThreadId()););
#elif !defined(GRAN) && !defined(PAR)
    IF_DEBUG(scheduler,fprintf(stderr,"sched: "););
#endif
    t = cap->r.rCurrentTSO;
    
#if defined(PAR)
    /* HACK 675: if the last thread didn't yield, make sure to print a 
       SCHEDULE event to the log file when StgRunning the next thread, even
       if it is the same one as before */
    LastTSO = t; 
    TimeOfLastYield = CURRENT_TIME;
#endif

    switch (ret) {
    case HeapOverflow:
#if defined(GRAN)
      IF_DEBUG(gran, DumpGranEvent(GR_DESCHEDULE, t));
      globalGranStats.tot_heapover++;
#elif defined(PAR)
      globalParStats.tot_heapover++;
#endif

      // did the task ask for a large block?
      if (cap->r.rHpAlloc > BLOCK_SIZE_W) {
	  // if so, get one and push it on the front of the nursery.
	  bdescr *bd;
	  nat blocks;
	  
	  blocks = (nat)BLOCK_ROUND_UP(cap->r.rHpAlloc * sizeof(W_)) / BLOCK_SIZE;

	  IF_DEBUG(scheduler,belch("--<< thread %ld (%s) stopped: requesting a large block (size %d)", 
				   t->id, whatNext_strs[t->what_next], blocks));

	  // don't do this if it would push us over the
	  // alloc_blocks_lim limit; we'll GC first.
	  if (alloc_blocks + blocks < alloc_blocks_lim) {

	      alloc_blocks += blocks;
	      bd = allocGroup( blocks );

	      // link the new group into the list
	      bd->link = cap->r.rCurrentNursery;
	      bd->u.back = cap->r.rCurrentNursery->u.back;
	      if (cap->r.rCurrentNursery->u.back != NULL) {
		  cap->r.rCurrentNursery->u.back->link = bd;
	      } else {
		  ASSERT(g0s0->blocks == cap->r.rCurrentNursery &&
			 g0s0->blocks == cap->r.rNursery);
		  cap->r.rNursery = g0s0->blocks = bd;
	      }		  
	      cap->r.rCurrentNursery->u.back = bd;

	      // initialise it as a nursery block.  We initialise the
	      // step, gen_no, and flags field of *every* sub-block in
	      // this large block, because this is easier than making
	      // sure that we always find the block head of a large
	      // block whenever we call Bdescr() (eg. evacuate() and
	      // isAlive() in the GC would both have to do this, at
	      // least).
	      { 
		  bdescr *x;
		  for (x = bd; x < bd + blocks; x++) {
		      x->step = g0s0;
		      x->gen_no = 0;
		      x->flags = 0;
		  }
	      }

	      // don't forget to update the block count in g0s0.
	      g0s0->n_blocks += blocks;
	      // This assert can be a killer if the app is doing lots
	      // of large block allocations.
	      ASSERT(countBlocks(g0s0->blocks) == g0s0->n_blocks);

	      // now update the nursery to point to the new block
	      cap->r.rCurrentNursery = bd;

	      // we might be unlucky and have another thread get on the
	      // run queue before us and steal the large block, but in that
	      // case the thread will just end up requesting another large
	      // block.
	      PUSH_ON_RUN_QUEUE(t);
	      break;
	  }
      }

      /* make all the running tasks block on a condition variable,
       * maybe set context_switch and wait till they all pile in,
       * then have them wait on a GC condition variable.
       */
      IF_DEBUG(scheduler,belch("--<< thread %ld (%s) stopped: HeapOverflow", 
			       t->id, whatNext_strs[t->what_next]));
      threadPaused(t);
#if defined(GRAN)
      ASSERT(!is_on_queue(t,CurrentProc));
#elif defined(PAR)
      /* Currently we emit a DESCHEDULE event before GC in GUM.
         ToDo: either add separate event to distinguish SYSTEM time from rest
	       or just nuke this DESCHEDULE (and the following SCHEDULE) */
      if (0 && RtsFlags.ParFlags.ParStats.Full) {
	DumpRawGranEvent(CURRENT_PROC, CURRENT_PROC,
			 GR_DESCHEDULE, t, (StgClosure *)NULL, 0, 0);
	emitSchedule = rtsTrue;
      }
#endif
      
      ready_to_gc = rtsTrue;
      context_switch = 1;		/* stop other threads ASAP */
      PUSH_ON_RUN_QUEUE(t);
      /* actual GC is done at the end of the while loop */
      break;
      
    case StackOverflow:
#if defined(GRAN)
      IF_DEBUG(gran, 
	       DumpGranEvent(GR_DESCHEDULE, t));
      globalGranStats.tot_stackover++;
#elif defined(PAR)
      // IF_DEBUG(par, 
      // DumpGranEvent(GR_DESCHEDULE, t);
      globalParStats.tot_stackover++;
#endif
      IF_DEBUG(scheduler,belch("--<< thread %ld (%s) stopped, StackOverflow", 
			       t->id, whatNext_strs[t->what_next]));
      /* just adjust the stack for this thread, then pop it back
       * on the run queue.
       */
      threadPaused(t);
      { 
	StgMainThread *m;
	/* enlarge the stack */
	StgTSO *new_t = threadStackOverflow(t);
	
	/* This TSO has moved, so update any pointers to it from the
	 * main thread stack.  It better not be on any other queues...
	 * (it shouldn't be).
	 */
	for (m = main_threads; m != NULL; m = m->link) {
	  if (m->tso == t) {
	    m->tso = new_t;
	  }
	}
	threadPaused(new_t);
	PUSH_ON_RUN_QUEUE(new_t);
      }
      break;

    case ThreadYielding:
#if defined(GRAN)
      IF_DEBUG(gran, 
	       DumpGranEvent(GR_DESCHEDULE, t));
      globalGranStats.tot_yields++;
#elif defined(PAR)
      // IF_DEBUG(par, 
      // DumpGranEvent(GR_DESCHEDULE, t);
      globalParStats.tot_yields++;
#endif
      /* put the thread back on the run queue.  Then, if we're ready to
       * GC, check whether this is the last task to stop.  If so, wake
       * up the GC thread.  getThread will block during a GC until the
       * GC is finished.
       */
      IF_DEBUG(scheduler,
               if (t->what_next != prev_what_next) {
		   belch("--<< thread %ld (%s) stopped to switch evaluators", 
			 t->id, whatNext_strs[t->what_next]);
               } else {
                   belch("--<< thread %ld (%s) stopped, yielding", 
			 t->id, whatNext_strs[t->what_next]);
               }
               );

      IF_DEBUG(sanity,
	       //belch("&& Doing sanity check on yielding TSO %ld.", t->id);
	       checkTSO(t));
      ASSERT(t->link == END_TSO_QUEUE);

      // Shortcut if we're just switching evaluators: don't bother
      // doing stack squeezing (which can be expensive), just run the
      // thread.
      if (t->what_next != prev_what_next) {
	  goto run_thread;
      }

      threadPaused(t);

#if defined(GRAN)
      ASSERT(!is_on_queue(t,CurrentProc));

      IF_DEBUG(sanity,
	       //belch("&& Doing sanity check on all ThreadQueues (and their TSOs).");
	       checkThreadQsSanity(rtsTrue));
#endif

#if defined(PAR)
      if (RtsFlags.ParFlags.doFairScheduling) { 
	/* this does round-robin scheduling; good for concurrency */
	APPEND_TO_RUN_QUEUE(t);
      } else {
	/* this does unfair scheduling; good for parallelism */
	PUSH_ON_RUN_QUEUE(t);
      }
#else
      // this does round-robin scheduling; good for concurrency
      APPEND_TO_RUN_QUEUE(t);
#endif

#if defined(GRAN)
      /* add a ContinueThread event to actually process the thread */
      new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc],
		ContinueThread,
		t, (StgClosure*)NULL, (rtsSpark*)NULL);
      IF_GRAN_DEBUG(bq, 
	       belch("GRAN: eventq and runnableq after adding yielded thread to queue again:");
	       G_EVENTQ(0);
	       G_CURR_THREADQ(0));
#endif /* GRAN */
      break;

    case ThreadBlocked:
#if defined(GRAN)
      IF_DEBUG(scheduler,
	       belch("--<< thread %ld (%p; %s) stopped, blocking on node %p [PE %d] with BQ: ", 
			       t->id, t, whatNext_strs[t->what_next], t->block_info.closure, (t->block_info.closure==(StgClosure*)NULL ? 99 : where_is(t->block_info.closure)));
	       if (t->block_info.closure!=(StgClosure*)NULL) print_bq(t->block_info.closure));

      // ??? needed; should emit block before
      IF_DEBUG(gran, 
	       DumpGranEvent(GR_DESCHEDULE, t)); 
      prune_eventq(t, (StgClosure *)NULL); // prune ContinueThreads for t
      /*
	ngoq Dogh!
      ASSERT(procStatus[CurrentProc]==Busy || 
	      ((procStatus[CurrentProc]==Fetching) && 
	      (t->block_info.closure!=(StgClosure*)NULL)));
      if (run_queue_hds[CurrentProc] == END_TSO_QUEUE &&
	  !(!RtsFlags.GranFlags.DoAsyncFetch &&
	    procStatus[CurrentProc]==Fetching)) 
	procStatus[CurrentProc] = Idle;
      */
#elif defined(PAR)
      IF_DEBUG(scheduler,
	       belch("--<< thread %ld (%p; %s) stopped, blocking on node %p with BQ: ", 
		     t->id, t, whatNext_strs[t->what_next], t->block_info.closure));
      IF_PAR_DEBUG(bq,

		   if (t->block_info.closure!=(StgClosure*)NULL) 
		     print_bq(t->block_info.closure));

      /* Send a fetch (if BlockedOnGA) and dump event to log file */
      blockThread(t);

      /* whatever we schedule next, we must log that schedule */
      emitSchedule = rtsTrue;

#else /* !GRAN */
      /* don't need to do anything.  Either the thread is blocked on
       * I/O, in which case we'll have called addToBlockedQueue
       * previously, or it's blocked on an MVar or Blackhole, in which
       * case it'll be on the relevant queue already.
       */
      IF_DEBUG(scheduler,
	       fprintf(stderr, "--<< thread %d (%s) stopped: ", 
		       t->id, whatNext_strs[t->what_next]);
	       printThreadBlockage(t);
	       fprintf(stderr, "\n"));
      fflush(stderr);

      /* Only for dumping event to log file 
	 ToDo: do I need this in GranSim, too?
      blockThread(t);
      */
#endif
      threadPaused(t);
      break;

    case ThreadFinished:
      /* Need to check whether this was a main thread, and if so, signal
       * the task that started it with the return value.  If we have no
       * more main threads, we probably need to stop all the tasks until
       * we get a new one.
       */
      /* We also end up here if the thread kills itself with an
       * uncaught exception, see Exception.hc.
       */
      IF_DEBUG(scheduler,belch("--++ thread %d (%s) finished", 
			       t->id, whatNext_strs[t->what_next]));
#if defined(GRAN)
      endThread(t, CurrentProc); // clean-up the thread
#elif defined(PAR)
      /* For now all are advisory -- HWL */
      //if(t->priority==AdvisoryPriority) ??
      advisory_thread_count--;
      
# ifdef DIST
      if(t->dist.priority==RevalPriority)
	FinishReval(t);
# endif
      
      if (RtsFlags.ParFlags.ParStats.Full &&
	  !RtsFlags.ParFlags.ParStats.Suppressed) 
	DumpEndEvent(CURRENT_PROC, t, rtsFalse /* not mandatory */);
#endif
      break;
      
    default:
      barf("schedule: invalid thread return code %d", (int)ret);
    }

#ifdef PROFILING
    // When we have +RTS -i0 and we're heap profiling, do a census at
    // every GC.  This lets us get repeatable runs for debugging.
    if (performHeapProfile ||
	(RtsFlags.ProfFlags.profileInterval==0 &&
	 RtsFlags.ProfFlags.doHeapProfile && ready_to_gc)) {
	GarbageCollect(GetRoots, rtsTrue);
	heapCensus();
	performHeapProfile = rtsFalse;
	ready_to_gc = rtsFalse;	// we already GC'd
    }
#endif

    if (ready_to_gc 
#ifdef SMP
	&& allFreeCapabilities() 
#endif
	) {
      /* everybody back, start the GC.
       * Could do it in this thread, or signal a condition var
       * to do it in another thread.  Either way, we need to
       * broadcast on gc_pending_cond afterward.
       */
#if defined(RTS_SUPPORTS_THREADS)
      IF_DEBUG(scheduler,sched_belch("doing GC"));
#endif
      GarbageCollect(GetRoots,rtsFalse);
      ready_to_gc = rtsFalse;
#ifdef SMP
      broadcastCondition(&gc_pending_cond);
#endif
#if defined(GRAN)
      /* add a ContinueThread event to continue execution of current thread */
      new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc],
		ContinueThread,
		t, (StgClosure*)NULL, (rtsSpark*)NULL);
      IF_GRAN_DEBUG(bq, 
	       fprintf(stderr, "GRAN: eventq and runnableq after Garbage collection:\n");
	       G_EVENTQ(0);
	       G_CURR_THREADQ(0));
#endif /* GRAN */
    }

#if defined(GRAN)
  next_thread:
    IF_GRAN_DEBUG(unused,
		  print_eventq(EventHd));

    event = get_next_event();
#elif defined(PAR)
  next_thread:
    /* ToDo: wait for next message to arrive rather than busy wait */
#endif /* GRAN */

  } /* end of while(1) */

  IF_PAR_DEBUG(verbose,
	       belch("== Leaving schedule() after having received Finish"));
}

/* ---------------------------------------------------------------------------
 * rtsSupportsBoundThreads(): is the RTS built to support bound threads?
 * used by Control.Concurrent for error checking.
 * ------------------------------------------------------------------------- */
 
StgBool
rtsSupportsBoundThreads(void)
{
#ifdef THREADED_RTS
  return rtsTrue;
#else
  return rtsFalse;
#endif
}

/* ---------------------------------------------------------------------------
 * isThreadBound(tso): check whether tso is bound to an OS thread.
 * ------------------------------------------------------------------------- */
 
StgBool
isThreadBound(StgTSO* tso USED_IN_THREADED_RTS)
{
#ifdef THREADED_RTS
  StgMainThread *m;
  for(m = main_threads; m; m = m->link)
  {
    if(m->tso == tso)
      return rtsTrue;
  }
#endif
  return rtsFalse;
}

/* ---------------------------------------------------------------------------
 * Singleton fork(). Do not copy any running threads.
 * ------------------------------------------------------------------------- */

static void 
deleteThreadImmediately(StgTSO *tso);

StgInt
forkProcess(HsStablePtr *entry)
{
#if !defined(mingw32_TARGET_OS) && !defined(STANDALONE)
  pid_t pid;
  StgTSO* t,*next;
  StgMainThread *m;
  SchedulerStatus rc;

  IF_DEBUG(scheduler,sched_belch("forking!"));
  rts_lock(); // This not only acquires sched_mutex, it also
              // makes sure that no other threads are running

  pid = fork();

  if (pid) { /* parent */

  /* just return the pid */
    rts_unlock();
    return pid;
    
  } else { /* child */
    
    
      // delete all threads
    run_queue_hd = run_queue_tl = END_TSO_QUEUE;
    
    for (t = all_threads; t != END_TSO_QUEUE; t = next) {
      next = t->link;

        // don't allow threads to catch the ThreadKilled exception
      deleteThreadImmediately(t);
    }
    
      // wipe the main thread list
    while((m = main_threads) != NULL) {
      main_threads = m->link;
#ifdef THREADED_RTS
      closeCondition(&m->bound_thread_cond);
#endif
      stgFree(m);
    }
    
#ifdef RTS_SUPPORTS_THREADS
    resetTaskManagerAfterFork();      // tell startTask() and friends that
    startingWorkerThread = rtsFalse;  // we have no worker threads any more
    resetWorkerWakeupPipeAfterFork();
#endif
    
    rc = rts_evalStableIO(entry, NULL);  // run the action
    rts_checkSchedStatus("forkProcess",rc);
    
    rts_unlock();
    
    hs_exit();                      // clean up and exit
    stg_exit(0);
  }
#else /* mingw32 */
  barf("forkProcess#: primop not implemented for mingw32, sorry!\n");
  return -1;
#endif /* mingw32 */
}

/* ---------------------------------------------------------------------------
 * deleteAllThreads():  kill all the live threads.
 *
 * This is used when we catch a user interrupt (^C), before performing
 * any necessary cleanups and running finalizers.
 *
 * Locks: sched_mutex held.
 * ------------------------------------------------------------------------- */
   
void
deleteAllThreads ( void )
{
  StgTSO* t, *next;
  IF_DEBUG(scheduler,sched_belch("deleting all threads"));
  for (t = all_threads; t != END_TSO_QUEUE; t = next) {
      next = t->global_link;
      deleteThread(t);
  }      
  run_queue_hd = run_queue_tl = END_TSO_QUEUE;
  blocked_queue_hd = blocked_queue_tl = END_TSO_QUEUE;
  sleeping_queue = END_TSO_QUEUE;
}

/* startThread and  insertThread are now in GranSim.c -- HWL */


//@node Suspend and Resume, Run queue code, Main scheduling loop, Main scheduling code
//@subsection Suspend and Resume

/* ---------------------------------------------------------------------------
 * Suspending & resuming Haskell threads.
 * 
 * When making a "safe" call to C (aka _ccall_GC), the task gives back
 * its capability before calling the C function.  This allows another
 * task to pick up the capability and carry on running Haskell
 * threads.  It also means that if the C call blocks, it won't lock
 * the whole system.
 *
 * The Haskell thread making the C call is put to sleep for the
 * duration of the call, on the susepended_ccalling_threads queue.  We
 * give out a token to the task, which it can use to resume the thread
 * on return from the C function.
 * ------------------------------------------------------------------------- */
   
StgInt
suspendThread( StgRegTable *reg, 
	       rtsBool concCall
#if !defined(RTS_SUPPORTS_THREADS) && !defined(DEBUG)
	       STG_UNUSED
#endif
	       )
{
  nat tok;
  Capability *cap;
#ifndef STANDALONE
  int saved_errno = errno;
#endif

  /* assume that *reg is a pointer to the StgRegTable part
   * of a Capability.
   */
  cap = (Capability *)((void *)reg - sizeof(StgFunTable));

  ACQUIRE_LOCK(&sched_mutex);

  IF_DEBUG(scheduler,
	   sched_belch("thread %d did a _ccall_gc (is_concurrent: %d)", cap->r.rCurrentTSO->id,concCall));

  // XXX this might not be necessary --SDM
  cap->r.rCurrentTSO->what_next = ThreadRunGHC;

  threadPaused(cap->r.rCurrentTSO);
  cap->r.rCurrentTSO->link = suspended_ccalling_threads;
  suspended_ccalling_threads = cap->r.rCurrentTSO;

#if defined(RTS_SUPPORTS_THREADS)
  if(cap->r.rCurrentTSO->blocked_exceptions == NULL)
  {
      cap->r.rCurrentTSO->why_blocked = BlockedOnCCall;
      cap->r.rCurrentTSO->blocked_exceptions = END_TSO_QUEUE;
  }
  else
  {
      cap->r.rCurrentTSO->why_blocked = BlockedOnCCall_NoUnblockExc;
  }
#endif

  /* Use the thread ID as the token; it should be unique */
  tok = cap->r.rCurrentTSO->id;

  /* Hand back capability */
  releaseCapability(cap);
  
#if defined(RTS_SUPPORTS_THREADS)
  /* Preparing to leave the RTS, so ensure there's a native thread/task
     waiting to take over.
  */
  IF_DEBUG(scheduler, sched_belch("worker (token %d): leaving RTS", tok));
#endif

  /* Other threads _might_ be available for execution; signal this */
  THREAD_RUNNABLE();
  RELEASE_LOCK(&sched_mutex);
  
#ifndef STANDALONE
  errno = saved_errno;
#endif
  return tok; 
}

StgRegTable *
resumeThread( StgInt tok,
	      rtsBool concCall STG_UNUSED )
{
  StgTSO *tso, **prev;
  Capability *cap;
#ifndef STANDALONE
  int saved_errno = errno;
#endif

#if defined(RTS_SUPPORTS_THREADS)
  /* Wait for permission to re-enter the RTS with the result. */
  ACQUIRE_LOCK(&sched_mutex);
  grabReturnCapability(&sched_mutex, &cap);

  IF_DEBUG(scheduler, sched_belch("worker (token %d): re-entering RTS", tok));
#else
  grabCapability(&cap);
#endif

  /* Remove the thread off of the suspended list */
  prev = &suspended_ccalling_threads;
  for (tso = suspended_ccalling_threads; 
       tso != END_TSO_QUEUE; 
       prev = &tso->link, tso = tso->link) {
    if (tso->id == (StgThreadID)tok) {
      *prev = tso->link;
      break;
    }
  }
  if (tso == END_TSO_QUEUE) {
    barf("resumeThread: thread not found");
  }
  tso->link = END_TSO_QUEUE;
  
#if defined(RTS_SUPPORTS_THREADS)
  if(tso->why_blocked == BlockedOnCCall)
  {
      awakenBlockedQueueNoLock(tso->blocked_exceptions);
      tso->blocked_exceptions = NULL;
  }
#endif
  
  /* Reset blocking status */
  tso->why_blocked  = NotBlocked;

  cap->r.rCurrentTSO = tso;
#if defined(RTS_SUPPORTS_THREADS)
  RELEASE_LOCK(&sched_mutex);
#endif
#ifndef STANDALONE
  errno = saved_errno;
#endif
  return &cap->r;
}


/* ---------------------------------------------------------------------------
 * Static functions
 * ------------------------------------------------------------------------ */
static void unblockThread(StgTSO *tso);

/* ---------------------------------------------------------------------------
 * Comparing Thread ids.
 *
 * This is used from STG land in the implementation of the
 * instances of Eq/Ord for ThreadIds.
 * ------------------------------------------------------------------------ */

int
cmp_thread(StgPtr tso1, StgPtr tso2) 
{ 
  StgThreadID id1 = ((StgTSO *)tso1)->id; 
  StgThreadID id2 = ((StgTSO *)tso2)->id;
 
  if (id1 < id2) return (-1);
  if (id1 > id2) return 1;
  return 0;
}

/* ---------------------------------------------------------------------------
 * Fetching the ThreadID from an StgTSO.
 *
 * This is used in the implementation of Show for ThreadIds.
 * ------------------------------------------------------------------------ */
int
rts_getThreadId(StgPtr tso) 
{
  return ((StgTSO *)tso)->id;
}

#ifdef DEBUG
void
labelThread(StgPtr tso, char *label)
{
  int len;
  void *buf;

  /* Caveat: Once set, you can only set the thread name to "" */
  len = strlen(label)+1;
  buf = stgMallocBytes(len * sizeof(char), "Schedule.c:labelThread()");
  strncpy(buf,label,len);
  /* Update will free the old memory for us */
  updateThreadLabel(((StgTSO *)tso)->id,buf);
}
#endif /* DEBUG */

/* ---------------------------------------------------------------------------
   Create a new thread.

   The new thread starts with the given stack size.  Before the
   scheduler can run, however, this thread needs to have a closure
   (and possibly some arguments) pushed on its stack.  See
   pushClosure() in Schedule.h.

   createGenThread() and createIOThread() (in SchedAPI.h) are
   convenient packaged versions of this function.

   currently pri (priority) is only used in a GRAN setup -- HWL
   ------------------------------------------------------------------------ */
//@cindex createThread
#if defined(GRAN)
/*   currently pri (priority) is only used in a GRAN setup -- HWL */
StgTSO *
createThread(nat size, StgInt pri)
#else
StgTSO *
createThread(nat size)
#endif
{

    StgTSO *tso;
    nat stack_size;

    /* First check whether we should create a thread at all */
#if defined(PAR)
  /* check that no more than RtsFlags.ParFlags.maxThreads threads are created */
  if (advisory_thread_count >= RtsFlags.ParFlags.maxThreads) {
    threadsIgnored++;
    belch("{createThread}Daq ghuH: refusing to create another thread; no more than %d threads allowed (currently %d)",
	  RtsFlags.ParFlags.maxThreads, advisory_thread_count);
    return END_TSO_QUEUE;
  }
  threadsCreated++;
#endif

#if defined(GRAN)
  ASSERT(!RtsFlags.GranFlags.Light || CurrentProc==0);
#endif

  // ToDo: check whether size = stack_size - TSO_STRUCT_SIZEW

  /* catch ridiculously small stack sizes */
  if (size < MIN_STACK_WORDS + TSO_STRUCT_SIZEW) {
    size = MIN_STACK_WORDS + TSO_STRUCT_SIZEW;
  }

  stack_size = size - TSO_STRUCT_SIZEW;

  tso = (StgTSO *)allocate(size);
  TICK_ALLOC_TSO(stack_size, 0);

  SET_HDR(tso, &stg_TSO_info, CCS_SYSTEM);
#if defined(GRAN)
  SET_GRAN_HDR(tso, ThisPE);
#endif

  // Always start with the compiled code evaluator
  tso->what_next = ThreadRunGHC;

  /* tso->id needs to be unique.  For now we use a heavyweight mutex to
   * protect the increment operation on next_thread_id.
   * In future, we could use an atomic increment instead.
   */
  ACQUIRE_LOCK(&thread_id_mutex);
  tso->id = next_thread_id++; 
  RELEASE_LOCK(&thread_id_mutex);

  tso->why_blocked  = NotBlocked;
  tso->blocked_exceptions = NULL;

#ifndef STANDALONE
  tso->saved_errno = 0;
#endif
  
  tso->stack_size   = stack_size;
  tso->max_stack_size = round_to_mblocks(RtsFlags.GcFlags.maxStkSize) 
                              - TSO_STRUCT_SIZEW;
  tso->sp           = (P_)&(tso->stack) + stack_size;

#ifdef PROFILING
  tso->prof.CCCS = CCS_MAIN;
#endif

  /* put a stop frame on the stack */
  tso->sp -= sizeofW(StgStopFrame);
  SET_HDR((StgClosure*)tso->sp,(StgInfoTable *)&stg_stop_thread_info,CCS_SYSTEM);
  // ToDo: check this
#if defined(GRAN)
  tso->link = END_TSO_QUEUE;
  /* uses more flexible routine in GranSim */
  insertThread(tso, CurrentProc);
#else
  /* In a non-GranSim setup the pushing of a TSO onto the runq is separated
   * from its creation
   */
#endif

#if defined(GRAN) 
  if (RtsFlags.GranFlags.GranSimStats.Full) 
    DumpGranEvent(GR_START,tso);
#elif defined(PAR)
  if (RtsFlags.ParFlags.ParStats.Full) 
    DumpGranEvent(GR_STARTQ,tso);
  /* HACk to avoid SCHEDULE 
     LastTSO = tso; */
#endif

  /* Link the new thread on the global thread list.
   */
  tso->global_link = all_threads;
  all_threads = tso;

#if defined(DIST)
  tso->dist.priority = MandatoryPriority; //by default that is...
#endif

#if defined(GRAN)
  tso->gran.pri = pri;
# if defined(DEBUG)
  tso->gran.magic = TSO_MAGIC; // debugging only
# endif
  tso->gran.sparkname   = 0;
  tso->gran.startedat   = CURRENT_TIME; 
  tso->gran.exported    = 0;
  tso->gran.basicblocks = 0;
  tso->gran.allocs      = 0;
  tso->gran.exectime    = 0;
  tso->gran.fetchtime   = 0;
  tso->gran.fetchcount  = 0;
  tso->gran.blocktime   = 0;
  tso->gran.blockcount  = 0;
  tso->gran.blockedat   = 0;
  tso->gran.globalsparks = 0;
  tso->gran.localsparks  = 0;
  if (RtsFlags.GranFlags.Light)
    tso->gran.clock  = Now; /* local clock */
  else
    tso->gran.clock  = 0;

  IF_DEBUG(gran,printTSO(tso));
#elif defined(PAR)
# if defined(DEBUG)
  tso->par.magic = TSO_MAGIC; // debugging only
# endif
  tso->par.sparkname   = 0;
  tso->par.startedat   = CURRENT_TIME; 
  tso->par.exported    = 0;
  tso->par.basicblocks = 0;
  tso->par.allocs      = 0;
  tso->par.exectime    = 0;
  tso->par.fetchtime   = 0;
  tso->par.fetchcount  = 0;
  tso->par.blocktime   = 0;
  tso->par.blockcount  = 0;
  tso->par.blockedat   = 0;
  tso->par.globalsparks = 0;
  tso->par.localsparks  = 0;
#endif

#if defined(GRAN)
  globalGranStats.tot_threads_created++;
  globalGranStats.threads_created_on_PE[CurrentProc]++;
  globalGranStats.tot_sq_len += spark_queue_len(CurrentProc);
  globalGranStats.tot_sq_probes++;
#elif defined(PAR)
  // collect parallel global statistics (currently done together with GC stats)
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    //fprintf(stderr, "Creating thread %d @ %11.2f\n", tso->id, usertime()); 
    globalParStats.tot_threads_created++;
  }
#endif 

#if defined(GRAN)
  IF_GRAN_DEBUG(pri,
		belch("==__ schedule: Created TSO %d (%p);",
		      CurrentProc, tso, tso->id));
#elif defined(PAR)
    IF_PAR_DEBUG(verbose,
		 belch("==__ schedule: Created TSO %d (%p); %d threads active",
		       tso->id, tso, advisory_thread_count));
#else
  IF_DEBUG(scheduler,sched_belch("created thread %ld, stack size = %lx words", 
				 tso->id, tso->stack_size));
#endif    
  return tso;
}

#if defined(PAR)
/* RFP:
   all parallel thread creation calls should fall through the following routine.
*/
StgTSO *
createSparkThread(rtsSpark spark) 
{ StgTSO *tso;
  ASSERT(spark != (rtsSpark)NULL);
  if (advisory_thread_count >= RtsFlags.ParFlags.maxThreads) 
  { threadsIgnored++;
    barf("{createSparkThread}Daq ghuH: refusing to create another thread; no more than %d threads allowed (currently %d)",
	  RtsFlags.ParFlags.maxThreads, advisory_thread_count);    
    return END_TSO_QUEUE;
  }
  else
  { threadsCreated++;
    tso = createThread(RtsFlags.GcFlags.initialStkSize);
    if (tso==END_TSO_QUEUE)	
      barf("createSparkThread: Cannot create TSO");
#if defined(DIST)
    tso->priority = AdvisoryPriority;
#endif
    pushClosure(tso,spark);
    PUSH_ON_RUN_QUEUE(tso);
    advisory_thread_count++;    
  }
  return tso;
}
#endif

/*
  Turn a spark into a thread.
  ToDo: fix for SMP (needs to acquire SCHED_MUTEX!)
*/
#if defined(PAR)
//@cindex activateSpark
StgTSO *
activateSpark (rtsSpark spark) 
{
  StgTSO *tso;

  tso = createSparkThread(spark);
  if (RtsFlags.ParFlags.ParStats.Full) {   
    //ASSERT(run_queue_hd == END_TSO_QUEUE); // I think ...
    IF_PAR_DEBUG(verbose,
		 belch("==^^ activateSpark: turning spark of closure %p (%s) into a thread",
		       (StgClosure *)spark, info_type((StgClosure *)spark)));
  }
  // ToDo: fwd info on local/global spark to thread -- HWL
  // tso->gran.exported =  spark->exported;
  // tso->gran.locked =   !spark->global;
  // tso->gran.sparkname = spark->name;

  return tso;
}
#endif

static SchedulerStatus waitThread_(/*out*/StgMainThread* m,
				   Capability *initialCapability
				   );


/* ---------------------------------------------------------------------------
 * scheduleThread()
 *
 * scheduleThread puts a thread on the head of the runnable queue.
 * This will usually be done immediately after a thread is created.
 * The caller of scheduleThread must create the thread using e.g.
 * createThread and push an appropriate closure
 * on this thread's stack before the scheduler is invoked.
 * ------------------------------------------------------------------------ */

static void scheduleThread_ (StgTSO* tso);

void
scheduleThread_(StgTSO *tso)
{
  // Precondition: sched_mutex must be held.

  /* Put the new thread on the head of the runnable queue.  The caller
   * better push an appropriate closure on this thread's stack
   * beforehand.  In the SMP case, the thread may start running as
   * soon as we release the scheduler lock below.
   */
  PUSH_ON_RUN_QUEUE(tso);
  THREAD_RUNNABLE();

#if 0
  IF_DEBUG(scheduler,printTSO(tso));
#endif
}

void scheduleThread(StgTSO* tso)
{
  ACQUIRE_LOCK(&sched_mutex);
  scheduleThread_(tso);
  RELEASE_LOCK(&sched_mutex);
}

SchedulerStatus
scheduleWaitThread(StgTSO* tso, /*[out]*/HaskellObj* ret, Capability *initialCapability)
{	// Precondition: sched_mutex must be held
  StgMainThread *m;

  m = stgMallocBytes(sizeof(StgMainThread), "waitThread");
  m->tso = tso;
  m->ret = ret;
  m->stat = NoStatus;
#if defined(RTS_SUPPORTS_THREADS)
#if defined(THREADED_RTS)
  initCondition(&m->bound_thread_cond);
#else
  initCondition(&m->wakeup);
#endif
#endif

  /* Put the thread on the main-threads list prior to scheduling the TSO.
     Failure to do so introduces a race condition in the MT case (as
     identified by Wolfgang Thaller), whereby the new task/OS thread 
     created by scheduleThread_() would complete prior to the thread
     that spawned it managed to put 'itself' on the main-threads list.
     The upshot of it all being that the worker thread wouldn't get to
     signal the completion of the its work item for the main thread to
     see (==> it got stuck waiting.)    -- sof 6/02.
  */
  IF_DEBUG(scheduler, sched_belch("waiting for thread (%d)", tso->id));
  
  m->link = main_threads;
  main_threads = m;

  scheduleThread_(tso);

  return waitThread_(m, initialCapability);
}

/* ---------------------------------------------------------------------------
 * initScheduler()
 *
 * Initialise the scheduler.  This resets all the queues - if the
 * queues contained any threads, they'll be garbage collected at the
 * next pass.
 *
 * ------------------------------------------------------------------------ */

#ifdef SMP
static void
term_handler(int sig STG_UNUSED)
{
  stat_workerStop();
  ACQUIRE_LOCK(&term_mutex);
  await_death--;
  RELEASE_LOCK(&term_mutex);
  shutdownThread();
}
#endif

void 
initScheduler(void)
{
#if defined(GRAN)
  nat i;

  for (i=0; i<=MAX_PROC; i++) {
    run_queue_hds[i]      = END_TSO_QUEUE;
    run_queue_tls[i]      = END_TSO_QUEUE;
    blocked_queue_hds[i]  = END_TSO_QUEUE;
    blocked_queue_tls[i]  = END_TSO_QUEUE;
    ccalling_threadss[i]  = END_TSO_QUEUE;
    sleeping_queue        = END_TSO_QUEUE;
  }
#else
  run_queue_hd      = END_TSO_QUEUE;
  run_queue_tl      = END_TSO_QUEUE;
  blocked_queue_hd  = END_TSO_QUEUE;
  blocked_queue_tl  = END_TSO_QUEUE;
  sleeping_queue    = END_TSO_QUEUE;
#endif 

  suspended_ccalling_threads  = END_TSO_QUEUE;

  main_threads = NULL;
  all_threads  = END_TSO_QUEUE;

  context_switch = 0;
  interrupted    = 0;

  RtsFlags.ConcFlags.ctxtSwitchTicks =
      RtsFlags.ConcFlags.ctxtSwitchTime / TICK_MILLISECS;
      
#if defined(RTS_SUPPORTS_THREADS)
  /* Initialise the mutex and condition variables used by
   * the scheduler. */
  initMutex(&sched_mutex);
  initMutex(&term_mutex);
  initMutex(&thread_id_mutex);

  initCondition(&thread_ready_cond);
#endif
  
#if defined(SMP)
  initCondition(&gc_pending_cond);
#endif

#if defined(RTS_SUPPORTS_THREADS)
  ACQUIRE_LOCK(&sched_mutex);
#endif

  /* Install the SIGHUP handler */
#if defined(SMP)
  {
    struct sigaction action,oact;

    action.sa_handler = term_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGTERM, &action, &oact) != 0) {
      barf("can't install TERM handler");
    }
  }
#endif

  /* A capability holds the state a native thread needs in
   * order to execute STG code. At least one capability is
   * floating around (only SMP builds have more than one).
   */
  initCapabilities();
  
#if defined(RTS_SUPPORTS_THREADS)
    /* start our haskell execution tasks */
# if defined(SMP)
    startTaskManager(RtsFlags.ParFlags.nNodes, taskStart);
# else
    startTaskManager(0,taskStart);
# endif
#endif

#if /* defined(SMP) ||*/ defined(PAR)
  initSparkPools();
#endif

#if defined(RTS_SUPPORTS_THREADS)
  RELEASE_LOCK(&sched_mutex);
#endif

}

void
exitScheduler( void )
{
#if defined(RTS_SUPPORTS_THREADS)
  stopTaskManager();
#endif
  shutting_down_scheduler = rtsTrue;
}

/* -----------------------------------------------------------------------------
   Managing the per-task allocation areas.
   
   Each capability comes with an allocation area.  These are
   fixed-length block lists into which allocation can be done.

   ToDo: no support for two-space collection at the moment???
   -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * waitThread is the external interface for running a new computation
 * and waiting for the result.
 *
 * In the non-SMP case, we create a new main thread, push it on the 
 * main-thread stack, and invoke the scheduler to run it.  The
 * scheduler will return when the top main thread on the stack has
 * completed or died, and fill in the necessary fields of the
 * main_thread structure.
 *
 * In the SMP case, we create a main thread as before, but we then
 * create a new condition variable and sleep on it.  When our new
 * main thread has completed, we'll be woken up and the status/result
 * will be in the main_thread struct.
 * -------------------------------------------------------------------------- */

int 
howManyThreadsAvail ( void )
{
   int i = 0;
   StgTSO* q;
   for (q = run_queue_hd; q != END_TSO_QUEUE; q = q->link)
      i++;
   for (q = blocked_queue_hd; q != END_TSO_QUEUE; q = q->link)
      i++;
   for (q = sleeping_queue; q != END_TSO_QUEUE; q = q->link)
      i++;
   return i;
}

void
finishAllThreads ( void )
{
   do {
      while (run_queue_hd != END_TSO_QUEUE) {
         waitThread ( run_queue_hd, NULL, NULL );
      }
      while (blocked_queue_hd != END_TSO_QUEUE) {
         waitThread ( blocked_queue_hd, NULL, NULL );
      }
      while (sleeping_queue != END_TSO_QUEUE) {
         waitThread ( blocked_queue_hd, NULL, NULL );
      }
   } while 
      (blocked_queue_hd != END_TSO_QUEUE || 
       run_queue_hd     != END_TSO_QUEUE ||
       sleeping_queue   != END_TSO_QUEUE);
}

SchedulerStatus
waitThread(StgTSO *tso, /*out*/StgClosure **ret, Capability *initialCapability)
{ 
  StgMainThread *m;
  SchedulerStatus stat;

  m = stgMallocBytes(sizeof(StgMainThread), "waitThread");
  m->tso = tso;
  m->ret = ret;
  m->stat = NoStatus;
#if defined(RTS_SUPPORTS_THREADS)
#if defined(THREADED_RTS)
  initCondition(&m->bound_thread_cond);
#else
  initCondition(&m->wakeup);
#endif
#endif

  /* see scheduleWaitThread() comment */
  ACQUIRE_LOCK(&sched_mutex);
  m->link = main_threads;
  main_threads = m;

  IF_DEBUG(scheduler, sched_belch("waiting for thread %d", tso->id));

  stat = waitThread_(m,initialCapability);
  
  RELEASE_LOCK(&sched_mutex);
  return stat;
}

static
SchedulerStatus
waitThread_(StgMainThread* m, Capability *initialCapability)
{
  SchedulerStatus stat;

  // Precondition: sched_mutex must be held.
  IF_DEBUG(scheduler, sched_belch("new main thread (%d)", m->tso->id));

#if defined(RTS_SUPPORTS_THREADS) && !defined(THREADED_RTS)
  {	// FIXME: does this still make sense?
	// It's not for the threaded rts => SMP only
    do {
      waitCondition(&m->wakeup, &sched_mutex);
    } while (m->stat == NoStatus);
  }
#elif defined(GRAN)
  /* GranSim specific init */
  CurrentTSO = m->tso;                // the TSO to run
  procStatus[MainProc] = Busy;        // status of main PE
  CurrentProc = MainProc;             // PE to run it on

  RELEASE_LOCK(&sched_mutex);
  schedule(m,initialCapability);
#else
  RELEASE_LOCK(&sched_mutex);
  schedule(m,initialCapability);
  ACQUIRE_LOCK(&sched_mutex);
  ASSERT(m->stat != NoStatus);
#endif

  stat = m->stat;

#if defined(RTS_SUPPORTS_THREADS)
#if defined(THREADED_RTS)
  closeCondition(&m->bound_thread_cond);
#else
  closeCondition(&m->wakeup);
#endif
#endif

  IF_DEBUG(scheduler, fprintf(stderr, "== sched: main thread (%d) finished\n", 
			      m->tso->id));
  stgFree(m);

  // Postcondition: sched_mutex still held
  return stat;
}

//@node Run queue code, Garbage Collextion Routines, Suspend and Resume, Main scheduling code
//@subsection Run queue code 

#if 0
/* 
   NB: In GranSim we have many run queues; run_queue_hd is actually a macro
       unfolding to run_queue_hds[CurrentProc], thus CurrentProc is an
       implicit global variable that has to be correct when calling these
       fcts -- HWL 
*/

/* Put the new thread on the head of the runnable queue.
 * The caller of createThread better push an appropriate closure
 * on this thread's stack before the scheduler is invoked.
 */
static /* inline */ void
add_to_run_queue(tso)
StgTSO* tso; 
{
  ASSERT(tso!=run_queue_hd && tso!=run_queue_tl);
  tso->link = run_queue_hd;
  run_queue_hd = tso;
  if (run_queue_tl == END_TSO_QUEUE) {
    run_queue_tl = tso;
  }
}

/* Put the new thread at the end of the runnable queue. */
static /* inline */ void
push_on_run_queue(tso)
StgTSO* tso; 
{
  ASSERT(get_itbl((StgClosure *)tso)->type == TSO);
  ASSERT(run_queue_hd!=NULL && run_queue_tl!=NULL);
  ASSERT(tso!=run_queue_hd && tso!=run_queue_tl);
  if (run_queue_hd == END_TSO_QUEUE) {
    run_queue_hd = tso;
  } else {
    run_queue_tl->link = tso;
  }
  run_queue_tl = tso;
}

/* 
   Should be inlined because it's used very often in schedule.  The tso
   argument is actually only needed in GranSim, where we want to have the
   possibility to schedule *any* TSO on the run queue, irrespective of the
   actual ordering. Therefore, if tso is not the nil TSO then we traverse
   the run queue and dequeue the tso, adjusting the links in the queue. 
*/
//@cindex take_off_run_queue
static /* inline */ StgTSO*
take_off_run_queue(StgTSO *tso) {
  StgTSO *t, *prev;

  /* 
     qetlaHbogh Qu' ngaSbogh ghomDaQ {tso} yIteq!

     if tso is specified, unlink that tso from the run_queue (doesn't have
     to be at the beginning of the queue); GranSim only 
  */
  if (tso!=END_TSO_QUEUE) {
    /* find tso in queue */
    for (t=run_queue_hd, prev=END_TSO_QUEUE; 
	 t!=END_TSO_QUEUE && t!=tso;
	 prev=t, t=t->link) 
      /* nothing */ ;
    ASSERT(t==tso);
    /* now actually dequeue the tso */
    if (prev!=END_TSO_QUEUE) {
      ASSERT(run_queue_hd!=t);
      prev->link = t->link;
    } else {
      /* t is at beginning of thread queue */
      ASSERT(run_queue_hd==t);
      run_queue_hd = t->link;
    }
    /* t is at end of thread queue */
    if (t->link==END_TSO_QUEUE) {
      ASSERT(t==run_queue_tl);
      run_queue_tl = prev;
    } else {
      ASSERT(run_queue_tl!=t);
    }
    t->link = END_TSO_QUEUE;
  } else {
    /* take tso from the beginning of the queue; std concurrent code */
    t = run_queue_hd;
    if (t != END_TSO_QUEUE) {
      run_queue_hd = t->link;
      t->link = END_TSO_QUEUE;
      if (run_queue_hd == END_TSO_QUEUE) {
	run_queue_tl = END_TSO_QUEUE;
      }
    }
  }
  return t;
}

#endif /* 0 */

//@node Garbage Collextion Routines, Blocking Queue Routines, Run queue code, Main scheduling code
//@subsection Garbage Collextion Routines

/* ---------------------------------------------------------------------------
   Where are the roots that we know about?

        - all the threads on the runnable queue
        - all the threads on the blocked queue
        - all the threads on the sleeping queue
	- all the thread currently executing a _ccall_GC
        - all the "main threads"
     
   ------------------------------------------------------------------------ */

/* This has to be protected either by the scheduler monitor, or by the
	garbage collection monitor (probably the latter).
	KH @ 25/10/99
*/

void
GetRoots(evac_fn evac)
{
#if defined(GRAN)
  {
    nat i;
    for (i=0; i<=RtsFlags.GranFlags.proc; i++) {
      if ((run_queue_hds[i] != END_TSO_QUEUE) && ((run_queue_hds[i] != NULL)))
	  evac((StgClosure **)&run_queue_hds[i]);
      if ((run_queue_tls[i] != END_TSO_QUEUE) && ((run_queue_tls[i] != NULL)))
	  evac((StgClosure **)&run_queue_tls[i]);
      
      if ((blocked_queue_hds[i] != END_TSO_QUEUE) && ((blocked_queue_hds[i] != NULL)))
	  evac((StgClosure **)&blocked_queue_hds[i]);
      if ((blocked_queue_tls[i] != END_TSO_QUEUE) && ((blocked_queue_tls[i] != NULL)))
	  evac((StgClosure **)&blocked_queue_tls[i]);
      if ((ccalling_threadss[i] != END_TSO_QUEUE) && ((ccalling_threadss[i] != NULL)))
	  evac((StgClosure **)&ccalling_threads[i]);
    }
  }

  markEventQueue();

#else /* !GRAN */
  if (run_queue_hd != END_TSO_QUEUE) {
      ASSERT(run_queue_tl != END_TSO_QUEUE);
      evac((StgClosure **)&run_queue_hd);
      evac((StgClosure **)&run_queue_tl);
  }
  
  if (blocked_queue_hd != END_TSO_QUEUE) {
      ASSERT(blocked_queue_tl != END_TSO_QUEUE);
      evac((StgClosure **)&blocked_queue_hd);
      evac((StgClosure **)&blocked_queue_tl);
  }
  
  if (sleeping_queue != END_TSO_QUEUE) {
      evac((StgClosure **)&sleeping_queue);
  }
#endif 

  if (suspended_ccalling_threads != END_TSO_QUEUE) {
      evac((StgClosure **)&suspended_ccalling_threads);
  }

#if defined(PAR) || defined(GRAN)
  markSparkQueue(evac);
#endif

#if defined(RTS_USER_SIGNALS)
  // mark the signal handlers (signals should be already blocked)
  markSignalHandlers(evac);
#endif

  // main threads which have completed need to be retained until they
  // are dealt with in the main scheduler loop.  They won't be
  // retained any other way: the GC will drop them from the
  // all_threads list, so we have to be careful to treat them as roots
  // here.
  { 
      StgMainThread *m;
      for (m = main_threads; m != NULL; m = m->link) {
	  switch (m->tso->what_next) {
	  case ThreadComplete:
	  case ThreadKilled:
	      evac((StgClosure **)&m->tso);
	      break;
	  default:
	      break;
	  }
      }
  }
}

/* -----------------------------------------------------------------------------
   performGC

   This is the interface to the garbage collector from Haskell land.
   We provide this so that external C code can allocate and garbage
   collect when called from Haskell via _ccall_GC.

   It might be useful to provide an interface whereby the programmer
   can specify more roots (ToDo).
   
   This needs to be protected by the GC condition variable above.  KH.
   -------------------------------------------------------------------------- */

static void (*extra_roots)(evac_fn);

void
performGC(void)
{
  /* Obligated to hold this lock upon entry */
  ACQUIRE_LOCK(&sched_mutex);
  GarbageCollect(GetRoots,rtsFalse);
  RELEASE_LOCK(&sched_mutex);
}

void
performMajorGC(void)
{
  ACQUIRE_LOCK(&sched_mutex);
  GarbageCollect(GetRoots,rtsTrue);
  RELEASE_LOCK(&sched_mutex);
}

static void
AllRoots(evac_fn evac)
{
    GetRoots(evac);		// the scheduler's roots
    extra_roots(evac);		// the user's roots
}

void
performGCWithRoots(void (*get_roots)(evac_fn))
{
  ACQUIRE_LOCK(&sched_mutex);
  extra_roots = get_roots;
  GarbageCollect(AllRoots,rtsFalse);
  RELEASE_LOCK(&sched_mutex);
}

/* -----------------------------------------------------------------------------
   Stack overflow

   If the thread has reached its maximum stack size, then raise the
   StackOverflow exception in the offending thread.  Otherwise
   relocate the TSO into a larger chunk of memory and adjust its stack
   size appropriately.
   -------------------------------------------------------------------------- */

static StgTSO *
threadStackOverflow(StgTSO *tso)
{
  nat new_stack_size, new_tso_size, stack_words;
  StgPtr new_sp;
  StgTSO *dest;

  IF_DEBUG(sanity,checkTSO(tso));
  if (tso->stack_size >= tso->max_stack_size) {

    IF_DEBUG(gc,
	     belch("@@ threadStackOverflow of TSO %d (%p): stack too large (now %ld; max is %ld",
		   tso->id, tso, tso->stack_size, tso->max_stack_size);
	     /* If we're debugging, just print out the top of the stack */
	     printStackChunk(tso->sp, stg_min(tso->stack+tso->stack_size, 
					      tso->sp+64)));

    /* Send this thread the StackOverflow exception */
    raiseAsync(tso, (StgClosure *)stackOverflow_closure);
    return tso;
  }

  /* Try to double the current stack size.  If that takes us over the
   * maximum stack size for this thread, then use the maximum instead.
   * Finally round up so the TSO ends up as a whole number of blocks.
   */
  new_stack_size = stg_min(tso->stack_size * 2, tso->max_stack_size);
  new_tso_size   = (nat)BLOCK_ROUND_UP(new_stack_size * sizeof(W_) + 
				       TSO_STRUCT_SIZE)/sizeof(W_);
  new_tso_size = round_to_mblocks(new_tso_size);  /* Be MBLOCK-friendly */
  new_stack_size = new_tso_size - TSO_STRUCT_SIZEW;

  IF_DEBUG(scheduler, fprintf(stderr,"== sched: increasing stack size from %d words to %d.\n", tso->stack_size, new_stack_size));

  dest = (StgTSO *)allocate(new_tso_size);
  TICK_ALLOC_TSO(new_stack_size,0);

  /* copy the TSO block and the old stack into the new area */
  memcpy(dest,tso,TSO_STRUCT_SIZE);
  stack_words = tso->stack + tso->stack_size - tso->sp;
  new_sp = (P_)dest + new_tso_size - stack_words;
  memcpy(new_sp, tso->sp, stack_words * sizeof(W_));

  /* relocate the stack pointers... */
  dest->sp         = new_sp;
  dest->stack_size = new_stack_size;
	
  /* Mark the old TSO as relocated.  We have to check for relocated
   * TSOs in the garbage collector and any primops that deal with TSOs.
   *
   * It's important to set the sp value to just beyond the end
   * of the stack, so we don't attempt to scavenge any part of the
   * dead TSO's stack.
   */
  tso->what_next = ThreadRelocated;
  tso->link = dest;
  tso->sp = (P_)&(tso->stack[tso->stack_size]);
  tso->why_blocked = NotBlocked;
  dest->mut_link = NULL;

  IF_PAR_DEBUG(verbose,
	       belch("@@ threadStackOverflow of TSO %d (now at %p): stack size increased to %ld",
		     tso->id, tso, tso->stack_size);
	       /* If we're debugging, just print out the top of the stack */
	       printStackChunk(tso->sp, stg_min(tso->stack+tso->stack_size, 
						tso->sp+64)));
  
  IF_DEBUG(sanity,checkTSO(tso));
#if 0
  IF_DEBUG(scheduler,printTSO(dest));
#endif

  return dest;
}

//@node Blocking Queue Routines, Exception Handling Routines, Garbage Collextion Routines, Main scheduling code
//@subsection Blocking Queue Routines

/* ---------------------------------------------------------------------------
   Wake up a queue that was blocked on some resource.
   ------------------------------------------------------------------------ */

#if defined(GRAN)
static inline void
unblockCount ( StgBlockingQueueElement *bqe, StgClosure *node )
{
}
#elif defined(PAR)
static inline void
unblockCount ( StgBlockingQueueElement *bqe, StgClosure *node )
{
  /* write RESUME events to log file and
     update blocked and fetch time (depending on type of the orig closure) */
  if (RtsFlags.ParFlags.ParStats.Full) {
    DumpRawGranEvent(CURRENT_PROC, CURRENT_PROC, 
		     GR_RESUMEQ, ((StgTSO *)bqe), ((StgTSO *)bqe)->block_info.closure,
		     0, 0 /* spark_queue_len(ADVISORY_POOL) */);
    if (EMPTY_RUN_QUEUE())
      emitSchedule = rtsTrue;

    switch (get_itbl(node)->type) {
	case FETCH_ME_BQ:
	  ((StgTSO *)bqe)->par.fetchtime += CURRENT_TIME-((StgTSO *)bqe)->par.blockedat;
	  break;
	case RBH:
	case FETCH_ME:
	case BLACKHOLE_BQ:
	  ((StgTSO *)bqe)->par.blocktime += CURRENT_TIME-((StgTSO *)bqe)->par.blockedat;
	  break;
#ifdef DIST
        case MVAR:
          break;
#endif	  
	default:
	  barf("{unblockOneLocked}Daq Qagh: unexpected closure in blocking queue");
	}
      }
}
#endif

#if defined(GRAN)
static StgBlockingQueueElement *
unblockOneLocked(StgBlockingQueueElement *bqe, StgClosure *node)
{
    StgTSO *tso;
    PEs node_loc, tso_loc;

    node_loc = where_is(node); // should be lifted out of loop
    tso = (StgTSO *)bqe;  // wastes an assignment to get the type right
    tso_loc = where_is((StgClosure *)tso);
    if (IS_LOCAL_TO(PROCS(node),tso_loc)) { // TSO is local
      /* !fake_fetch => TSO is on CurrentProc is same as IS_LOCAL_TO */
      ASSERT(CurrentProc!=node_loc || tso_loc==CurrentProc);
      CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.lunblocktime;
      // insertThread(tso, node_loc);
      new_event(tso_loc, tso_loc, CurrentTime[CurrentProc],
		ResumeThread,
		tso, node, (rtsSpark*)NULL);
      tso->link = END_TSO_QUEUE; // overwrite link just to be sure 
      // len_local++;
      // len++;
    } else { // TSO is remote (actually should be FMBQ)
      CurrentTime[CurrentProc] += RtsFlags.GranFlags.Costs.mpacktime +
                                  RtsFlags.GranFlags.Costs.gunblocktime +
	                          RtsFlags.GranFlags.Costs.latency;
      new_event(tso_loc, CurrentProc, CurrentTime[CurrentProc],
		UnblockThread,
		tso, node, (rtsSpark*)NULL);
      tso->link = END_TSO_QUEUE; // overwrite link just to be sure 
      // len++;
    }
    /* the thread-queue-overhead is accounted for in either Resume or UnblockThread */
    IF_GRAN_DEBUG(bq,
		  fprintf(stderr," %s TSO %d (%p) [PE %d] (block_info.closure=%p) (next=%p) ,",
			  (node_loc==tso_loc ? "Local" : "Global"), 
			  tso->id, tso, CurrentProc, tso->block_info.closure, tso->link));
    tso->block_info.closure = NULL;
    IF_DEBUG(scheduler,belch("-- Waking up thread %ld (%p)", 
			     tso->id, tso));
}
#elif defined(PAR)
static StgBlockingQueueElement *
unblockOneLocked(StgBlockingQueueElement *bqe, StgClosure *node)
{
    StgBlockingQueueElement *next;

    switch (get_itbl(bqe)->type) {
    case TSO:
      ASSERT(((StgTSO *)bqe)->why_blocked != NotBlocked);
      /* if it's a TSO just push it onto the run_queue */
      next = bqe->link;
      // ((StgTSO *)bqe)->link = END_TSO_QUEUE; // debugging?
      PUSH_ON_RUN_QUEUE((StgTSO *)bqe); 
      THREAD_RUNNABLE();
      unblockCount(bqe, node);
      /* reset blocking status after dumping event */
      ((StgTSO *)bqe)->why_blocked = NotBlocked;
      break;

    case BLOCKED_FETCH:
      /* if it's a BLOCKED_FETCH put it on the PendingFetches list */
      next = bqe->link;
      bqe->link = (StgBlockingQueueElement *)PendingFetches;
      PendingFetches = (StgBlockedFetch *)bqe;
      break;

# if defined(DEBUG)
      /* can ignore this case in a non-debugging setup; 
	 see comments on RBHSave closures above */
    case CONSTR:
      /* check that the closure is an RBHSave closure */
      ASSERT(get_itbl((StgClosure *)bqe) == &stg_RBH_Save_0_info ||
	     get_itbl((StgClosure *)bqe) == &stg_RBH_Save_1_info ||
	     get_itbl((StgClosure *)bqe) == &stg_RBH_Save_2_info);
      break;

    default:
      barf("{unblockOneLocked}Daq Qagh: Unexpected IP (%#lx; %s) in blocking queue at %#lx\n",
	   get_itbl((StgClosure *)bqe), info_type((StgClosure *)bqe), 
	   (StgClosure *)bqe);
# endif
    }
  IF_PAR_DEBUG(bq, fprintf(stderr, ", %p (%s)", bqe, info_type((StgClosure*)bqe)));
  return next;
}

#else /* !GRAN && !PAR */
#ifndef STANDALONE
static
#endif
StgTSO *
unblockOneLocked(StgTSO *tso)
{
  StgTSO *next;

  ASSERT(get_itbl(tso)->type == TSO);
  ASSERT(tso->why_blocked != NotBlocked);
  tso->why_blocked = NotBlocked;
  next = tso->link;
  PUSH_ON_RUN_QUEUE(tso);
  THREAD_RUNNABLE();
  IF_DEBUG(scheduler,sched_belch("waking up thread %ld", tso->id));
  return next;
}
#endif

#if defined(GRAN) || defined(PAR)
inline StgBlockingQueueElement *
unblockOne(StgBlockingQueueElement *bqe, StgClosure *node)
{
  ACQUIRE_LOCK(&sched_mutex);
  bqe = unblockOneLocked(bqe, node);
  RELEASE_LOCK(&sched_mutex);
  return bqe;
}
#else
inline StgTSO *
unblockOne(StgTSO *tso)
{
  ACQUIRE_LOCK(&sched_mutex);
  tso = unblockOneLocked(tso);
  RELEASE_LOCK(&sched_mutex);
  return tso;
}
#endif

#if defined(GRAN)
void 
awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node)
{
  StgBlockingQueueElement *bqe;
  PEs node_loc;
  nat len = 0; 

  IF_GRAN_DEBUG(bq, 
		belch("##-_ AwBQ for node %p on PE %d @ %ld by TSO %d (%p): ", \
		      node, CurrentProc, CurrentTime[CurrentProc], 
		      CurrentTSO->id, CurrentTSO));

  node_loc = where_is(node);

  ASSERT(q == END_BQ_QUEUE ||
	 get_itbl(q)->type == TSO ||   // q is either a TSO or an RBHSave
	 get_itbl(q)->type == CONSTR); // closure (type constructor)
  ASSERT(is_unique(node));

  /* FAKE FETCH: magically copy the node to the tso's proc;
     no Fetch necessary because in reality the node should not have been 
     moved to the other PE in the first place
  */
  if (CurrentProc!=node_loc) {
    IF_GRAN_DEBUG(bq, 
		  belch("## node %p is on PE %d but CurrentProc is %d (TSO %d); assuming fake fetch and adjusting bitmask (old: %#x)",
			node, node_loc, CurrentProc, CurrentTSO->id, 
			// CurrentTSO, where_is(CurrentTSO),
			node->header.gran.procs));
    node->header.gran.procs = (node->header.gran.procs) | PE_NUMBER(CurrentProc);
    IF_GRAN_DEBUG(bq, 
		  belch("## new bitmask of node %p is %#x",
			node, node->header.gran.procs));
    if (RtsFlags.GranFlags.GranSimStats.Global) {
      globalGranStats.tot_fake_fetches++;
    }
  }

  bqe = q;
  // ToDo: check: ASSERT(CurrentProc==node_loc);
  while (get_itbl(bqe)->type==TSO) { // q != END_TSO_QUEUE) {
    //next = bqe->link;
    /* 
       bqe points to the current element in the queue
       next points to the next element in the queue
    */
    //tso = (StgTSO *)bqe;  // wastes an assignment to get the type right
    //tso_loc = where_is(tso);
    len++;
    bqe = unblockOneLocked(bqe, node);
  }

  /* if this is the BQ of an RBH, we have to put back the info ripped out of
     the closure to make room for the anchor of the BQ */
  if (bqe!=END_BQ_QUEUE) {
    ASSERT(get_itbl(node)->type == RBH && get_itbl(bqe)->type == CONSTR);
    /*
    ASSERT((info_ptr==&RBH_Save_0_info) ||
	   (info_ptr==&RBH_Save_1_info) ||
	   (info_ptr==&RBH_Save_2_info));
    */
    /* cf. convertToRBH in RBH.c for writing the RBHSave closure */
    ((StgRBH *)node)->blocking_queue = (StgBlockingQueueElement *)((StgRBHSave *)bqe)->payload[0];
    ((StgRBH *)node)->mut_link       = (StgMutClosure *)((StgRBHSave *)bqe)->payload[1];

    IF_GRAN_DEBUG(bq,
		  belch("## Filled in RBH_Save for %p (%s) at end of AwBQ",
			node, info_type(node)));
  }

  /* statistics gathering */
  if (RtsFlags.GranFlags.GranSimStats.Global) {
    // globalGranStats.tot_bq_processing_time += bq_processing_time;
    globalGranStats.tot_bq_len += len;      // total length of all bqs awakened
    // globalGranStats.tot_bq_len_local += len_local;  // same for local TSOs only
    globalGranStats.tot_awbq++;             // total no. of bqs awakened
  }
  IF_GRAN_DEBUG(bq,
		fprintf(stderr,"## BQ Stats of %p: [%d entries] %s\n",
			node, len, (bqe!=END_BQ_QUEUE) ? "RBH" : ""));
}
#elif defined(PAR)
void 
awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node)
{
  StgBlockingQueueElement *bqe;

  ACQUIRE_LOCK(&sched_mutex);

  IF_PAR_DEBUG(verbose, 
	       belch("##-_ AwBQ for node %p on [%x]: ",
		     node, mytid));
#ifdef DIST  
  //RFP
  if(get_itbl(q)->type == CONSTR || q==END_BQ_QUEUE) {
    IF_PAR_DEBUG(verbose, belch("## ... nothing to unblock so lets just return. RFP (BUG?)"));
    return;
  }
#endif
  
  ASSERT(q == END_BQ_QUEUE ||
	 get_itbl(q)->type == TSO ||           
  	 get_itbl(q)->type == BLOCKED_FETCH || 
  	 get_itbl(q)->type == CONSTR); 

  bqe = q;
  while (get_itbl(bqe)->type==TSO || 
	 get_itbl(bqe)->type==BLOCKED_FETCH) {
    bqe = unblockOneLocked(bqe, node);
  }
  RELEASE_LOCK(&sched_mutex);
}

#else   /* !GRAN && !PAR */

#ifdef RTS_SUPPORTS_THREADS
void
awakenBlockedQueueNoLock(StgTSO *tso)
{
  while (tso != END_TSO_QUEUE) {
    tso = unblockOneLocked(tso);
  }
}
#endif

void
awakenBlockedQueue(StgTSO *tso)
{
  ACQUIRE_LOCK(&sched_mutex);
  while (tso != END_TSO_QUEUE) {
    tso = unblockOneLocked(tso);
  }
  RELEASE_LOCK(&sched_mutex);
}
#endif

//@node Exception Handling Routines, Debugging Routines, Blocking Queue Routines, Main scheduling code
//@subsection Exception Handling Routines

/* ---------------------------------------------------------------------------
   Interrupt execution
   - usually called inside a signal handler so it mustn't do anything fancy.   
   ------------------------------------------------------------------------ */

void
interruptStgRts(void)
{
    interrupted    = 1;
    context_switch = 1;
#ifdef RTS_SUPPORTS_THREADS
    wakeBlockedWorkerThread();
#endif
}

/* -----------------------------------------------------------------------------
   Unblock a thread

   This is for use when we raise an exception in another thread, which
   may be blocked.
   This has nothing to do with the UnblockThread event in GranSim. -- HWL
   -------------------------------------------------------------------------- */

#if defined(GRAN) || defined(PAR)
/*
  NB: only the type of the blocking queue is different in GranSim and GUM
      the operations on the queue-elements are the same
      long live polymorphism!

  Locks: sched_mutex is held upon entry and exit.

*/
static void
unblockThread(StgTSO *tso)
{
  StgBlockingQueueElement *t, **last;

  switch (tso->why_blocked) {

  case NotBlocked:
    return;  /* not blocked */

  case BlockedOnMVar:
    ASSERT(get_itbl(tso->block_info.closure)->type == MVAR);
    {
      StgBlockingQueueElement *last_tso = END_BQ_QUEUE;
      StgMVar *mvar = (StgMVar *)(tso->block_info.closure);

      last = (StgBlockingQueueElement **)&mvar->head;
      for (t = (StgBlockingQueueElement *)mvar->head; 
	   t != END_BQ_QUEUE; 
	   last = &t->link, last_tso = t, t = t->link) {
	if (t == (StgBlockingQueueElement *)tso) {
	  *last = (StgBlockingQueueElement *)tso->link;
	  if (mvar->tail == tso) {
	    mvar->tail = (StgTSO *)last_tso;
	  }
	  goto done;
	}
      }
      barf("unblockThread (MVAR): TSO not found");
    }

  case BlockedOnBlackHole:
    ASSERT(get_itbl(tso->block_info.closure)->type == BLACKHOLE_BQ);
    {
      StgBlockingQueue *bq = (StgBlockingQueue *)(tso->block_info.closure);

      last = &bq->blocking_queue;
      for (t = bq->blocking_queue; 
	   t != END_BQ_QUEUE; 
	   last = &t->link, t = t->link) {
	if (t == (StgBlockingQueueElement *)tso) {
	  *last = (StgBlockingQueueElement *)tso->link;
	  goto done;
	}
      }
      barf("unblockThread (BLACKHOLE): TSO not found");
    }

  case BlockedOnException:
    {
      StgTSO *target  = tso->block_info.tso;

      ASSERT(get_itbl(target)->type == TSO);

      if (target->what_next == ThreadRelocated) {
	  target = target->link;
	  ASSERT(get_itbl(target)->type == TSO);
      }

      ASSERT(target->blocked_exceptions != NULL);

      last = (StgBlockingQueueElement **)&target->blocked_exceptions;
      for (t = (StgBlockingQueueElement *)target->blocked_exceptions; 
	   t != END_BQ_QUEUE; 
	   last = &t->link, t = t->link) {
	ASSERT(get_itbl(t)->type == TSO);
	if (t == (StgBlockingQueueElement *)tso) {
	  *last = (StgBlockingQueueElement *)tso->link;
	  goto done;
	}
      }
      barf("unblockThread (Exception): TSO not found");
    }

  case BlockedOnRead:
  case BlockedOnWrite:
#ifdef STANDALONE
  case BlockedWaitingInterrupts:
#endif
#if defined(mingw32_TARGET_OS)
  case BlockedOnDoProc:
#endif
    {
      /* take TSO off blocked_queue */
      StgBlockingQueueElement *prev = NULL;
      for (t = (StgBlockingQueueElement *)blocked_queue_hd; t != END_BQ_QUEUE; 
	   prev = t, t = t->link) {
	if (t == (StgBlockingQueueElement *)tso) {
	  if (prev == NULL) {
	    blocked_queue_hd = (StgTSO *)t->link;
	    if ((StgBlockingQueueElement *)blocked_queue_tl == t) {
	      blocked_queue_tl = END_TSO_QUEUE;
	    }
	  } else {
	    prev->link = t->link;
	    if ((StgBlockingQueueElement *)blocked_queue_tl == t) {
	      blocked_queue_tl = (StgTSO *)prev;
	    }
	  }
	  goto done;
	}
      }
      barf("unblockThread (I/O): TSO not found");
    }

  case BlockedOnDelay:
    {
      /* take TSO off sleeping_queue */
      StgBlockingQueueElement *prev = NULL;
      for (t = (StgBlockingQueueElement *)sleeping_queue; t != END_BQ_QUEUE; 
	   prev = t, t = t->link) {
	if (t == (StgBlockingQueueElement *)tso) {
	  if (prev == NULL) {
	    sleeping_queue = (StgTSO *)t->link;
	  } else {
	    prev->link = t->link;
	  }
	  goto done;
	}
      }
      barf("unblockThread (delay): TSO not found");
    }

  default:
    barf("unblockThread");
  }

 done:
  tso->link = END_TSO_QUEUE;
  tso->why_blocked = NotBlocked;
  tso->block_info.closure = NULL;
  PUSH_ON_RUN_QUEUE(tso);
}
#else
static void
unblockThread(StgTSO *tso)
{
  StgTSO *t, **last;
  
  /* To avoid locking unnecessarily. */
  if (tso->why_blocked == NotBlocked) {
    return;
  }

  switch (tso->why_blocked) {

  case BlockedOnMVar:
    ASSERT(get_itbl(tso->block_info.closure)->type == MVAR);
    {
      StgTSO *last_tso = END_TSO_QUEUE;
      StgMVar *mvar = (StgMVar *)(tso->block_info.closure);

      last = &mvar->head;
      for (t = mvar->head; t != END_TSO_QUEUE; 
	   last = &t->link, last_tso = t, t = t->link) {
	if (t == tso) {
	  *last = tso->link;
	  if (mvar->tail == tso) {
	    mvar->tail = last_tso;
	  }
	  goto done;
	}
      }
      barf("unblockThread (MVAR): TSO not found");
    }

  case BlockedOnBlackHole:
    ASSERT(get_itbl(tso->block_info.closure)->type == BLACKHOLE_BQ);
    {
      StgBlockingQueue *bq = (StgBlockingQueue *)(tso->block_info.closure);

      last = &bq->blocking_queue;
      for (t = bq->blocking_queue; t != END_TSO_QUEUE; 
	   last = &t->link, t = t->link) {
	if (t == tso) {
	  *last = tso->link;
	  goto done;
	}
      }
      barf("unblockThread (BLACKHOLE): TSO not found");
    }

  case BlockedOnException:
    {
      StgTSO *target  = tso->block_info.tso;

      ASSERT(get_itbl(target)->type == TSO);

      while (target->what_next == ThreadRelocated) {
	  target = target->link;
	  ASSERT(get_itbl(target)->type == TSO);
      }
      
      ASSERT(target->blocked_exceptions != NULL);

      last = &target->blocked_exceptions;
      for (t = target->blocked_exceptions; t != END_TSO_QUEUE; 
	   last = &t->link, t = t->link) {
	ASSERT(get_itbl(t)->type == TSO);
	if (t == tso) {
	  *last = tso->link;
	  goto done;
	}
      }
      barf("unblockThread (Exception): TSO not found");
    }

  case BlockedOnRead:
  case BlockedOnWrite:
#ifdef STANDALONE
  case BlockedWaitingInterrupts:
#endif
#if defined(mingw32_TARGET_OS)
  case BlockedOnDoProc:
#endif
    {
      StgTSO *prev = NULL;
      for (t = blocked_queue_hd; t != END_TSO_QUEUE; 
	   prev = t, t = t->link) {
	if (t == tso) {
	  if (prev == NULL) {
	    blocked_queue_hd = t->link;
	    if (blocked_queue_tl == t) {
	      blocked_queue_tl = END_TSO_QUEUE;
	    }
	  } else {
	    prev->link = t->link;
	    if (blocked_queue_tl == t) {
	      blocked_queue_tl = prev;
	    }
	  }
	  goto done;
	}
      }
      barf("unblockThread (I/O): TSO not found");
    }

  case BlockedOnDelay:
    {
      StgTSO *prev = NULL;
      for (t = sleeping_queue; t != END_TSO_QUEUE; 
	   prev = t, t = t->link) {
	if (t == tso) {
	  if (prev == NULL) {
	    sleeping_queue = t->link;
	  } else {
	    prev->link = t->link;
	  }
	  goto done;
	}
      }
      barf("unblockThread (delay): TSO not found");
    }

  default:
    barf("unblockThread");
  }

 done:
  tso->link = END_TSO_QUEUE;
  tso->why_blocked = NotBlocked;
  tso->block_info.closure = NULL;
  PUSH_ON_RUN_QUEUE(tso);
}
#endif

/* -----------------------------------------------------------------------------
 * raiseAsync()
 *
 * The following function implements the magic for raising an
 * asynchronous exception in an existing thread.
 *
 * We first remove the thread from any queue on which it might be
 * blocked.  The possible blockages are MVARs and BLACKHOLE_BQs.
 *
 * We strip the stack down to the innermost CATCH_FRAME, building
 * thunks in the heap for all the active computations, so they can 
 * be restarted if necessary.  When we reach a CATCH_FRAME, we build
 * an application of the handler to the exception, and push it on
 * the top of the stack.
 * 
 * How exactly do we save all the active computations?  We create an
 * AP_STACK for every UpdateFrame on the stack.  Entering one of these
 * AP_STACKs pushes everything from the corresponding update frame
 * upwards onto the stack.  (Actually, it pushes everything up to the
 * next update frame plus a pointer to the next AP_STACK object.
 * Entering the next AP_STACK object pushes more onto the stack until we
 * reach the last AP_STACK object - at which point the stack should look
 * exactly as it did when we killed the TSO and we can continue
 * execution by entering the closure on top of the stack.
 *
 * We can also kill a thread entirely - this happens if either (a) the 
 * exception passed to raiseAsync is NULL, or (b) there's no
 * CATCH_FRAME on the stack.  In either case, we strip the entire
 * stack and replace the thread with a zombie.
 *
 * Locks: sched_mutex held upon entry nor exit.
 *
 * -------------------------------------------------------------------------- */
 
void 
deleteThread(StgTSO *tso)
{
  raiseAsync(tso,NULL);
}

static void 
deleteThreadImmediately(StgTSO *tso)
{ // for forkProcess only:
  // delete thread without giving it a chance to catch the KillThread exception

  if (tso->what_next == ThreadComplete || tso->what_next == ThreadKilled) {
      return;
  }
#if defined(RTS_SUPPORTS_THREADS)
  if (tso->why_blocked != BlockedOnCCall
      && tso->why_blocked != BlockedOnCCall_NoUnblockExc)
#endif
    unblockThread(tso);
  tso->what_next = ThreadKilled;
}

void
raiseAsyncWithLock(StgTSO *tso, StgClosure *exception)
{
  /* When raising async exs from contexts where sched_mutex isn't held;
     use raiseAsyncWithLock(). */
  ACQUIRE_LOCK(&sched_mutex);
  raiseAsync(tso,exception);
  RELEASE_LOCK(&sched_mutex);
}

void
raiseAsync(StgTSO *tso, StgClosure *exception)
{
    StgRetInfoTable *info;
    StgPtr sp;
  
    // Thread already dead?
    if (tso->what_next == ThreadComplete || tso->what_next == ThreadKilled) {
	return;
    }

    IF_DEBUG(scheduler, 
	     sched_belch("raising exception in thread %ld.", tso->id));
    
    // Remove it from any blocking queues
    unblockThread(tso);

    sp = tso->sp;
    
    // The stack freezing code assumes there's a closure pointer on
    // the top of the stack, so we have to arrange that this is the case...
    //
    if (sp[0] == (W_)&stg_enter_info) {
	sp++;
    } else {
	sp--;
	sp[0] = (W_)&stg_dummy_ret_closure;
    }

    while (1) {
	nat i;

	// 1. Let the top of the stack be the "current closure"
	//
	// 2. Walk up the stack until we find either an UPDATE_FRAME or a
	// CATCH_FRAME.
	//
	// 3. If it's an UPDATE_FRAME, then make an AP_STACK containing the
	// current closure applied to the chunk of stack up to (but not
	// including) the update frame.  This closure becomes the "current
	// closure".  Go back to step 2.
	//
	// 4. If it's a CATCH_FRAME, then leave the exception handler on
	// top of the stack applied to the exception.
	// 
	// 5. If it's a STOP_FRAME, then kill the thread.
	
	StgPtr frame;
	
	frame = sp + 1;
	info = get_ret_itbl((StgClosure *)frame);
	
	while (info->i.type != UPDATE_FRAME
	       && (info->i.type != CATCH_FRAME || exception == NULL)
	       && info->i.type != STOP_FRAME) {
	    frame += stack_frame_sizeW((StgClosure *)frame);
	    info = get_ret_itbl((StgClosure *)frame);
	}
	
	switch (info->i.type) {
	    
	case CATCH_FRAME:
	    // If we find a CATCH_FRAME, and we've got an exception to raise,
	    // then build the THUNK raise(exception), and leave it on
	    // top of the CATCH_FRAME ready to enter.
	    //
	{
#ifdef PROFILING
	    StgCatchFrame *cf = (StgCatchFrame *)frame;
#endif
	    StgClosure *raise;
	    
	    // we've got an exception to raise, so let's pass it to the
	    // handler in this frame.
	    //
	    raise = (StgClosure *)allocate(sizeofW(StgClosure)+1);
	    TICK_ALLOC_SE_THK(1,0);
	    SET_HDR(raise,&stg_raise_info,cf->header.prof.ccs);
	    raise->payload[0] = exception;
	    
	    // throw away the stack from Sp up to the CATCH_FRAME.
	    //
	    sp = frame - 1;
	    
	    /* Ensure that async excpetions are blocked now, so we don't get
	     * a surprise exception before we get around to executing the
	     * handler.
	     */
	    if (tso->blocked_exceptions == NULL) {
		tso->blocked_exceptions = END_TSO_QUEUE;
	    }
	    
	    /* Put the newly-built THUNK on top of the stack, ready to execute
	     * when the thread restarts.
	     */
	    sp[0] = (W_)raise;
	    sp[-1] = (W_)&stg_enter_info;
	    tso->sp = sp-1;
	    tso->what_next = ThreadRunGHC;
	    IF_DEBUG(sanity, checkTSO(tso));
	    return;
	}
	
	case UPDATE_FRAME:
	{
	    StgAP_STACK * ap;
	    nat words;
	    
	    // First build an AP_STACK consisting of the stack chunk above the
	    // current update frame, with the top word on the stack as the
	    // fun field.
	    //
	    words = frame - sp - 1;
	    ap = (StgAP_STACK *)allocate(PAP_sizeW(words));
	    
	    ap->size = words;
	    ap->fun  = (StgClosure *)sp[0];
	    sp++;
	    for(i=0; i < (nat)words; ++i) {
		ap->payload[i] = (StgClosure *)*sp++;
	    }
	    
	    SET_HDR(ap,&stg_AP_STACK_info,
		    ((StgClosure *)frame)->header.prof.ccs /* ToDo */); 
	    TICK_ALLOC_UP_THK(words+1,0);
	    
	    IF_DEBUG(scheduler,
		     fprintf(stderr,  "sched: Updating ");
		     printPtr((P_)((StgUpdateFrame *)frame)->updatee); 
		     fprintf(stderr,  " with ");
		     printObj((StgClosure *)ap);
		);

	    // Replace the updatee with an indirection - happily
	    // this will also wake up any threads currently
	    // waiting on the result.
	    //
	    // Warning: if we're in a loop, more than one update frame on
	    // the stack may point to the same object.  Be careful not to
	    // overwrite an IND_OLDGEN in this case, because we'll screw
	    // up the mutable lists.  To be on the safe side, don't
	    // overwrite any kind of indirection at all.  See also
	    // threadSqueezeStack in GC.c, where we have to make a similar
	    // check.
	    //
	    if (!closure_IND(((StgUpdateFrame *)frame)->updatee)) {
		// revert the black hole
		UPD_IND_NOLOCK(((StgUpdateFrame *)frame)->updatee,ap);
	    }
	    sp += sizeofW(StgUpdateFrame) - 1;
	    sp[0] = (W_)ap; // push onto stack
	    break;
	}
	
	case STOP_FRAME:
	    // We've stripped the entire stack, the thread is now dead.
	    sp += sizeofW(StgStopFrame);
	    tso->what_next = ThreadKilled;
	    tso->sp = sp;
	    return;
	    
	default:
	    barf("raiseAsync");
	}
    }
    barf("raiseAsync");
}

/* -----------------------------------------------------------------------------
   resurrectThreads is called after garbage collection on the list of
   threads found to be garbage.  Each of these threads will be woken
   up and sent a signal: BlockedOnDeadMVar if the thread was blocked
   on an MVar, or NonTermination if the thread was blocked on a Black
   Hole.

   Locks: sched_mutex isn't held upon entry nor exit.
   -------------------------------------------------------------------------- */

void
resurrectThreads( StgTSO *threads )
{
  StgTSO *tso, *next;

  for (tso = threads; tso != END_TSO_QUEUE; tso = next) {
    next = tso->global_link;
    tso->global_link = all_threads;
    all_threads = tso;
    IF_DEBUG(scheduler, sched_belch("resurrecting thread %d", tso->id));

    switch (tso->why_blocked) {
    case BlockedOnMVar:
    case BlockedOnException:
      /* Called by GC - sched_mutex lock is currently held. */
      raiseAsync(tso,(StgClosure *)BlockedOnDeadMVar_closure);
      break;
    case BlockedOnBlackHole:
      raiseAsync(tso,(StgClosure *)NonTermination_closure);
      break;
    case NotBlocked:
      /* This might happen if the thread was blocked on a black hole
       * belonging to a thread that we've just woken up (raiseAsync
       * can wake up threads, remember...).
       */
      continue;
    default:
      barf("resurrectThreads: thread blocked in a strange way");
    }
  }
}

/* -----------------------------------------------------------------------------
 * Blackhole detection: if we reach a deadlock, test whether any
 * threads are blocked on themselves.  Any threads which are found to
 * be self-blocked get sent a NonTermination exception.
 *
 * This is only done in a deadlock situation in order to avoid
 * performance overhead in the normal case.
 *
 * Locks: sched_mutex is held upon entry and exit.
 * -------------------------------------------------------------------------- */

static void
detectBlackHoles( void )
{
    StgTSO *tso = all_threads;
    StgClosure *frame;
    StgClosure *blocked_on;
    StgRetInfoTable *info;

    for (tso = all_threads; tso != END_TSO_QUEUE; tso = tso->global_link) {

	while (tso->what_next == ThreadRelocated) {
	    tso = tso->link;
	    ASSERT(get_itbl(tso)->type == TSO);
	}
      
	if (tso->why_blocked != BlockedOnBlackHole) {
	    continue;
	}
	blocked_on = tso->block_info.closure;

	frame = (StgClosure *)tso->sp;

	while(1) {
	    info = get_ret_itbl(frame);
	    switch (info->i.type) {
	    case UPDATE_FRAME:
		if (((StgUpdateFrame *)frame)->updatee == blocked_on) {
		    /* We are blocking on one of our own computations, so
		     * send this thread the NonTermination exception.  
		     */
		    IF_DEBUG(scheduler, 
			     sched_belch("thread %d is blocked on itself", tso->id));
		    raiseAsync(tso, (StgClosure *)NonTermination_closure);
		    goto done;
		}
		
		frame = (StgClosure *) ((StgUpdateFrame *)frame + 1);
		continue;

	    case STOP_FRAME:
		goto done;

		// normal stack frames; do nothing except advance the pointer
	    default:
		(StgPtr)frame += stack_frame_sizeW(frame);
	    }
	}   
	done: ;
    }
}

//@node Debugging Routines, Index, Exception Handling Routines, Main scheduling code
//@subsection Debugging Routines

/* -----------------------------------------------------------------------------
 * Debugging: why is a thread blocked
 * [Also provides useful information when debugging threaded programs
 *  at the Haskell source code level, so enable outside of DEBUG. --sof 7/02]
   -------------------------------------------------------------------------- */

static
void
printThreadBlockage(StgTSO *tso)
{
  switch (tso->why_blocked) {
  case BlockedOnRead:
    fprintf(stderr,"is blocked on read from fd %d", tso->block_info.fd);
    break;
  case BlockedOnWrite:
    fprintf(stderr,"is blocked on write to fd %d", tso->block_info.fd);
    break;
#ifdef STANDALONE
  case BlockedWaitingInterrupts:
    fprintf(stderr,"is blocked waiting interrupts");
    break;
#endif
#if defined(mingw32_TARGET_OS)
  case BlockedOnDoProc:
    fprintf(stderr,"is blocked on proc (request: %d)", tso->block_info.async_result->reqID);
    break;
#endif
  case BlockedOnDelay:
    fprintf(stderr,"is blocked until %d", tso->block_info.target);
    break;
  case BlockedOnMVar:
    fprintf(stderr,"is blocked on an MVar");
    break;
  case BlockedOnException:
    fprintf(stderr,"is blocked on delivering an exception to thread %d",
	    tso->block_info.tso->id);
    break;
  case BlockedOnBlackHole:
    fprintf(stderr,"is blocked on a black hole");
    break;
  case NotBlocked:
    fprintf(stderr,"is not blocked");
    break;
#if defined(PAR)
  case BlockedOnGA:
    fprintf(stderr,"is blocked on global address; local FM_BQ is %p (%s)",
	    tso->block_info.closure, info_type(tso->block_info.closure));
    break;
  case BlockedOnGA_NoSend:
    fprintf(stderr,"is blocked on global address (no send); local FM_BQ is %p (%s)",
	    tso->block_info.closure, info_type(tso->block_info.closure));
    break;
#endif
#if defined(RTS_SUPPORTS_THREADS)
  case BlockedOnCCall:
    fprintf(stderr,"is blocked on an external call");
    break;
  case BlockedOnCCall_NoUnblockExc:
    fprintf(stderr,"is blocked on an external call (exceptions were already blocked)");
    break;
#endif
  default:
    barf("printThreadBlockage: strange tso->why_blocked: %d for TSO %d (%d)",
	 tso->why_blocked, tso->id, tso);
  }
}

static
void
printThreadStatus(StgTSO *tso)
{
  switch (tso->what_next) {
  case ThreadKilled:
    fprintf(stderr,"has been killed");
    break;
  case ThreadComplete:
    fprintf(stderr,"has completed");
    break;
  default:
    printThreadBlockage(tso);
  }
}

void
printAllThreads(void)
{
  StgTSO *t;
  void *label;

# if defined(GRAN)
  char time_string[TIME_STR_LEN], node_str[NODE_STR_LEN];
  ullong_format_string(TIME_ON_PROC(CurrentProc), 
		       time_string, rtsFalse/*no commas!*/);

  fprintf(stderr, "all threads at [%s]:\n", time_string);
# elif defined(PAR)
  char time_string[TIME_STR_LEN], node_str[NODE_STR_LEN];
  ullong_format_string(CURRENT_TIME,
		       time_string, rtsFalse/*no commas!*/);

  fprintf(stderr,"all threads at [%s]:\n", time_string);
# else
  fprintf(stderr,"all threads:\n");
# endif

  for (t = all_threads; t != END_TSO_QUEUE; t = t->global_link) {
    fprintf(stderr, "\tthread %d @ %p ", t->id, (void *)t);
    label = lookupThreadLabel(t->id);
    if (label) fprintf(stderr,"[\"%s\"] ",(char *)label);
    printThreadStatus(t);
    fprintf(stderr,"\n");
  }
}
    
#ifdef DEBUG

/* 
   Print a whole blocking queue attached to node (debugging only).
*/
//@cindex print_bq
# if defined(PAR)
void 
print_bq (StgClosure *node)
{
  StgBlockingQueueElement *bqe;
  StgTSO *tso;
  rtsBool end;

  fprintf(stderr,"## BQ of closure %p (%s): ",
	  node, info_type(node));

  /* should cover all closures that may have a blocking queue */
  ASSERT(get_itbl(node)->type == BLACKHOLE_BQ ||
	 get_itbl(node)->type == FETCH_ME_BQ ||
	 get_itbl(node)->type == RBH ||
	 get_itbl(node)->type == MVAR);
    
  ASSERT(node!=(StgClosure*)NULL);         // sanity check

  print_bqe(((StgBlockingQueue*)node)->blocking_queue);
}

/* 
   Print a whole blocking queue starting with the element bqe.
*/
void 
print_bqe (StgBlockingQueueElement *bqe)
{
  rtsBool end;

  /* 
     NB: In a parallel setup a BQ of an RBH must end with an RBH_Save closure;
  */
  for (end = (bqe==END_BQ_QUEUE);
       !end; // iterate until bqe points to a CONSTR
       end = (get_itbl(bqe)->type == CONSTR) || (bqe->link==END_BQ_QUEUE), 
       bqe = end ? END_BQ_QUEUE : bqe->link) {
    ASSERT(bqe != END_BQ_QUEUE);                               // sanity check
    ASSERT(bqe != (StgBlockingQueueElement *)NULL);            // sanity check
    /* types of closures that may appear in a blocking queue */
    ASSERT(get_itbl(bqe)->type == TSO ||           
	   get_itbl(bqe)->type == BLOCKED_FETCH || 
	   get_itbl(bqe)->type == CONSTR); 
    /* only BQs of an RBH end with an RBH_Save closure */
    //ASSERT(get_itbl(bqe)->type != CONSTR || get_itbl(node)->type == RBH);

    switch (get_itbl(bqe)->type) {
    case TSO:
      fprintf(stderr," TSO %u (%x),",
	      ((StgTSO *)bqe)->id, ((StgTSO *)bqe));
      break;
    case BLOCKED_FETCH:
      fprintf(stderr," BF (node=%p, ga=((%x, %d, %x)),",
	      ((StgBlockedFetch *)bqe)->node, 
	      ((StgBlockedFetch *)bqe)->ga.payload.gc.gtid,
	      ((StgBlockedFetch *)bqe)->ga.payload.gc.slot,
	      ((StgBlockedFetch *)bqe)->ga.weight);
      break;
    case CONSTR:
      fprintf(stderr," %s (IP %p),",
	      (get_itbl(bqe) == &stg_RBH_Save_0_info ? "RBH_Save_0" :
	       get_itbl(bqe) == &stg_RBH_Save_1_info ? "RBH_Save_1" :
	       get_itbl(bqe) == &stg_RBH_Save_2_info ? "RBH_Save_2" :
	       "RBH_Save_?"), get_itbl(bqe));
      break;
    default:
      barf("Unexpected closure type %s in blocking queue", // of %p (%s)",
	   info_type((StgClosure *)bqe)); // , node, info_type(node));
      break;
    }
  } /* for */
  fputc('\n', stderr);
}
# elif defined(GRAN)
void 
print_bq (StgClosure *node)
{
  StgBlockingQueueElement *bqe;
  PEs node_loc, tso_loc;
  rtsBool end;

  /* should cover all closures that may have a blocking queue */
  ASSERT(get_itbl(node)->type == BLACKHOLE_BQ ||
	 get_itbl(node)->type == FETCH_ME_BQ ||
	 get_itbl(node)->type == RBH);
    
  ASSERT(node!=(StgClosure*)NULL);         // sanity check
  node_loc = where_is(node);

  fprintf(stderr,"## BQ of closure %p (%s) on [PE %d]: ",
	  node, info_type(node), node_loc);

  /* 
     NB: In a parallel setup a BQ of an RBH must end with an RBH_Save closure;
  */
  for (bqe = ((StgBlockingQueue*)node)->blocking_queue, end = (bqe==END_BQ_QUEUE);
       !end; // iterate until bqe points to a CONSTR
       end = (get_itbl(bqe)->type == CONSTR) || (bqe->link==END_BQ_QUEUE), bqe = end ? END_BQ_QUEUE : bqe->link) {
    ASSERT(bqe != END_BQ_QUEUE);             // sanity check
    ASSERT(bqe != (StgBlockingQueueElement *)NULL);  // sanity check
    /* types of closures that may appear in a blocking queue */
    ASSERT(get_itbl(bqe)->type == TSO ||           
	   get_itbl(bqe)->type == CONSTR); 
    /* only BQs of an RBH end with an RBH_Save closure */
    ASSERT(get_itbl(bqe)->type != CONSTR || get_itbl(node)->type == RBH);

    tso_loc = where_is((StgClosure *)bqe);
    switch (get_itbl(bqe)->type) {
    case TSO:
      fprintf(stderr," TSO %d (%p) on [PE %d],",
	      ((StgTSO *)bqe)->id, (StgTSO *)bqe, tso_loc);
      break;
    case CONSTR:
      fprintf(stderr," %s (IP %p),",
	      (get_itbl(bqe) == &stg_RBH_Save_0_info ? "RBH_Save_0" :
	       get_itbl(bqe) == &stg_RBH_Save_1_info ? "RBH_Save_1" :
	       get_itbl(bqe) == &stg_RBH_Save_2_info ? "RBH_Save_2" :
	       "RBH_Save_?"), get_itbl(bqe));
      break;
    default:
      barf("Unexpected closure type %s in blocking queue of %p (%s)",
	   info_type((StgClosure *)bqe), node, info_type(node));
      break;
    }
  } /* for */
  fputc('\n', stderr);
}
# elif !defined(STANDALONE)
/* 
   Nice and easy: only TSOs on the blocking queue
*/
void 
print_bq (StgClosure *node)
{
  StgTSO *tso;

  ASSERT(node!=(StgClosure*)NULL);         // sanity check
  for (tso = ((StgBlockingQueue*)node)->blocking_queue;
       tso != END_TSO_QUEUE; 
       tso=tso->link) {
    ASSERT(tso!=NULL && tso!=END_TSO_QUEUE);   // sanity check
    ASSERT(get_itbl(tso)->type == TSO);  // guess what, sanity check
    fprintf(stderr," TSO %d (%p),", tso->id, tso);
  }
  fputc('\n', stderr);
}
# endif

#if defined(PAR)
static nat
run_queue_len(void)
{
  nat i;
  StgTSO *tso;

  for (i=0, tso=run_queue_hd; 
       tso != END_TSO_QUEUE;
       i++, tso=tso->link)
    /* nothing */

  return i;
}
#endif

void
sched_belch(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
#ifdef RTS_SUPPORTS_THREADS
  fprintf(stderr, "sched (task %p): ", osThreadId());
#elif defined(PAR)
  fprintf(stderr, "== ");
#else
  fprintf(stderr, "sched: ");
#endif
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
  fflush(stderr);
  va_end(ap);
}

#endif /* DEBUG */


//@node Index,  , Debugging Routines, Main scheduling code
//@subsection Index

//@index
//* StgMainThread::  @cindex\s-+StgMainThread
//* awaken_blocked_queue::  @cindex\s-+awaken_blocked_queue
//* blocked_queue_hd::  @cindex\s-+blocked_queue_hd
//* blocked_queue_tl::  @cindex\s-+blocked_queue_tl
//* context_switch::  @cindex\s-+context_switch
//* createThread::  @cindex\s-+createThread
//* gc_pending_cond::  @cindex\s-+gc_pending_cond
//* initScheduler::  @cindex\s-+initScheduler
//* interrupted::  @cindex\s-+interrupted
//* next_thread_id::  @cindex\s-+next_thread_id
//* print_bq::  @cindex\s-+print_bq
//* run_queue_hd::  @cindex\s-+run_queue_hd
//* run_queue_tl::  @cindex\s-+run_queue_tl
//* sched_mutex::  @cindex\s-+sched_mutex
//* schedule::  @cindex\s-+schedule
//* take_off_run_queue::  @cindex\s-+take_off_run_queue
//* term_mutex::  @cindex\s-+term_mutex
//@end index
