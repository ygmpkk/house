/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The GHC Team 1998-2000
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

#define COMPILING_RTS_MAIN

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "SchedAPI.h"
#include "Schedule.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Prelude.h"
#include <stdlib.h>

#ifdef DEBUG
# include "Printer.h"   /* for printing        */
#endif

#ifdef INTERPRETER
# include "Assembler.h"
#endif

#ifdef PAR
# include "Parallel.h"
# include "ParallelRts.h"
# include "LLC.h"
#endif

#if defined(GRAN) || defined(PAR)
# include "GranSimRts.h"
#endif

#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif

extern void __stginit_ZCMain(void);

/* Hack: we assume that we're building a batch-mode system unless 
 * INTERPRETER is set
 */
#ifndef INTERPRETER /* Hack */
int main(int argc, char *argv[])
{
    int exit_status;
    SchedulerStatus status;
    /* all GranSim/GUM init is done in startupHaskell; sets IAmMainThread! */

    startupHaskell(argc,argv,__stginit_ZCMain);

    /* kick off the computation by creating the main thread with a pointer
       to mainIO_closure representing the computation of the overall program;
       then enter the scheduler with this thread and off we go;
      
       the same for GranSim (we have only one instance of this code)

       in a parallel setup, where we have many instances of this code
       running on different PEs, we should do this only for the main PE
       (IAmMainThread is set in startupHaskell) 
    */

#  if defined(PAR)

#   if defined(DEBUG)
    { /* a wait loop to allow attachment of gdb to UNIX threads */
      nat i, j, s;

      for (i=0, s=0; i<(nat)RtsFlags.ParFlags.wait; i++)
	for (j=0; j<1000000; j++) 
	  s += j % 65536;
    }
    IF_PAR_DEBUG(verbose,
		 belch("Passed wait loop"));
#   endif

    if (IAmMainThread == rtsTrue) {
      IF_PAR_DEBUG(verbose,
		   fprintf(stderr, "==== [%x] Main Thread Started ...\n", mytid));

      /* ToDo: Dump event for the main thread */
      status = rts_mainLazyIO((HaskellObj)mainIO_closure, NULL);
    } else {
      /* Just to show we're alive */
      IF_PAR_DEBUG(verbose,
		   fprintf(stderr, "== [%x] Non-Main PE enters scheduler via taskStart() without work ...\n",
			   mytid));
     
      /* all non-main threads enter the scheduler without work */
      taskStart();       
      status = Success;  // declare victory (see shutdownParallelSystem)
    }

#  elif defined(GRAN)

    /* ToDo: Dump event for the main thread */
    status = rts_mainLazyIO(mainIO_closure, NULL);

#  else /* !PAR && !GRAN */

    /* ToDo: want to start with a larger stack size */
    rts_lock();
    status = rts_evalLazyIO((HaskellObj)mainIO_closure, NULL);
    rts_unlock();

#  endif /* !PAR && !GRAN */

    /* check the status of the entire Haskell computation */
    switch (status) {
    case Killed:
      prog_belch("main thread exited (uncaught exception)");
      exit_status = EXIT_KILLED;
      break;
    case Interrupted:
      prog_belch("interrupted");
      exit_status = EXIT_INTERRUPTED;
      break;
    case Success:
      exit_status = EXIT_SUCCESS;
      break;
#if defined(PAR)
    case NoStatus:
      prog_belch("main thread PE killed; probably due to failure of another PE; check /tmp/pvml...");
      exit_status = EXIT_KILLED;
      break;
#endif 
    default:
      barf("main thread completed with invalid status");
    }
    shutdownHaskellAndExit(exit_status);
    return 0; /* never reached, keep gcc -Wall happy */
}
# endif /* BATCH_MODE */
