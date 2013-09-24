/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The AQUA Project, Glasgow University, 1994-1997
 * (c) The GHC Team, 1998-1999
 *
 * Functions for parsing the argument list.
 *
 * ---------------------------------------------------------------------------*/
//@menu
//* Includes::			
//* Constants::			
//* Static function decls::	
//* Command-line option parsing routines::  
//* GranSim specific options::	
//* Aux fcts::			
//@end menu
//*/

//@node Includes, Constants
//@subsection Includes

#include "PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "Timer.h"		/* CS_MIN_MILLISECS */
#include "Profiling.h"

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#include <stdlib.h>
#include <string.h>

extern struct RTS_FLAGS RtsFlags;

/*
 * Split argument lists
 */
int     prog_argc = 0;    /* an "int" so as to match normal "argc" */
char  **prog_argv = NULL;
char   *prog_name = NULL; /* 'basename' of prog_argv[0] */
int     rts_argc = 0;  /* ditto */
char   *rts_argv[MAX_RTS_ARGS];

//@node Constants, Static function decls, Includes
//@subsection Constants

/*
 * constants, used later 
 */
#define RTS 1
#define PGM 0

#if defined(GRAN)

static char *gran_debug_opts_strs[] = {
  "DEBUG (-bDe, -bD1): event_trace; printing event trace.\n",
  "DEBUG (-bDE, -bD2): event_stats; printing event statistics.\n",
  "DEBUG (-bDb, -bD4): bq; check blocking queues\n",
  "DEBUG (-bDG, -bD8): pack; routines for (un-)packing graph structures.\n",
  "DEBUG (-bDq, -bD16): checkSparkQ; check consistency of the spark queues.\n",
  "DEBUG (-bDf, -bD32): thunkStealing; print forwarding of fetches.\n",
  "DEBUG (-bDr, -bD64): randomSteal; stealing sparks/threads from random PEs.\n",
  "DEBUG (-bDF, -bD128): findWork; searching spark-pools (local & remote), thread queues for work.\n",
  "DEBUG (-bDu, -bD256): unused; currently unused flag.\n",
  "DEBUG (-bDS, -bD512): pri; priority sparking or scheduling.\n",
  "DEBUG (-bD:, -bD1024): checkLight; check GranSim-Light setup.\n",
  "DEBUG (-bDo, -bD2048): sortedQ; check whether spark/thread queues are sorted.\n",
  "DEBUG (-bDz, -bD4096): blockOnFetch; check for blocked on fetch.\n",
  "DEBUG (-bDP, -bD8192): packBuffer; routines handling pack buffer (GranSim internal!).\n",
  "DEBUG (-bDt, -bD16384): blockOnFetch_sanity; check for TSO asleep on fetch.\n",
};

/* one character codes for the available debug options */
static char gran_debug_opts_flags[] = {
  'e', 'E', 'b', 'G', 'q', 'f', 'r', 'F', 'u', 'S', ':', 'o', 'z', 'P', 't'
};

#elif defined(PAR)

static char *par_debug_opts_strs[] = {
  "DEBUG (-qDv, -qD1): verbose; be generally verbose with parallel related stuff.\n",
  "DEBUG (-qDq, -qD2): bq; print blocking queues.\n",
  "DEBUG (-qDs, -qD4): schedule; scheduling of parallel threads.\n",
  "DEBUG (-qDe, -qD8): free; free messages.\n",
  "DEBUG (-qDr, -qD16): resume; resume messages.\n",
  "DEBUG (-qDw, -qD32): weight; print weights and distrib GC stuff.\n",
  "DEBUG (-qDF, -qD64): fetch; fetch messages.\n",
  // "DEBUG (-qDa, -qD128): ack; ack messages.\n",
  "DEBUG (-qDf, -qD128): fish; fish messages.\n",
  //"DEBUG (-qDo, -qD512): forward; forwarding messages to other PEs.\n",
  "DEBUG (-qDl, -qD256): tables; print internal LAGA etc tables.\n",
  "DEBUG (-qDo, -qD512): packet; packets and graph structures when packing.\n",
  "DEBUG (-qDp, -qD1024): pack; packing and unpacking graphs.\n",
  "DEBUG (-qDz, -qD2048): paranoia; ridiculously detailed output (excellent for filling a partition).\n"
};

/* one character codes for the available debug options */
static char par_debug_opts_flags[] = {
  'v', 'q', 's', 'e', 'r', 'w', 'F', 'f', 'l', 'o', 'p', 'z'
};

#endif /* PAR */

#if !defined(STANDALONE) || defined(DEBUG)

//@node Static function decls, Command-line option parsing routines, Constants
//@subsection Static function decls

/* -----------------------------------------------------------------------------
   Static function decls
   -------------------------------------------------------------------------- */

static FILE *		/* return NULL on error */
open_stats_file (
    I_ arg,
    int argc, char *argv[],
    int rts_argc, char *rts_argv[],
    const char *FILENAME_FMT);

static I_ decode(const char *s);
static void bad_option(const char *s);

#if defined(GRAN)
static void enable_GranSimLight(void);
static void process_gran_option(int arg, int *rts_argc, char *rts_argv[], rtsBool *error);
static void set_GranSim_debug_options(nat n);
static void help_GranSim_debug_options(nat n);
#elif defined(PAR)
static void process_par_option(int arg, int *rts_argc, char *rts_argv[], rtsBool *error);
static void set_par_debug_options(nat n);
static void help_par_debug_options(nat n);
#endif

#endif /* !STANDALONE || DEBUG /*

//@node Command-line option parsing routines, GranSim specific options, Static function decls
//@subsection Command-line option parsing routines

/* -----------------------------------------------------------------------------
 * Command-line option parsing routines.
 * ---------------------------------------------------------------------------*/

void initRtsFlagsDefaults(void)
{
    RtsFlags.GcFlags.statsFile		= NULL;
    RtsFlags.GcFlags.giveStats		= NO_GC_STATS;

    RtsFlags.GcFlags.maxStkSize		= (1024 * 1024) / sizeof(W_);
    RtsFlags.GcFlags.initialStkSize	= 1024 / sizeof(W_);

    RtsFlags.GcFlags.minAllocAreaSize   = (256 * 1024)        / BLOCK_SIZE;
    RtsFlags.GcFlags.minOldGenSize      = (1024 * 1024)       / BLOCK_SIZE;
    RtsFlags.GcFlags.maxHeapSize	= 0;    /* off by default */
    RtsFlags.GcFlags.heapSizeSuggestion	= 0;    /* none */
    RtsFlags.GcFlags.pcFreeHeap		= 3;	/* 3% */
    RtsFlags.GcFlags.oldGenFactor       = 2;
#if defined(PAR)
    /* A hack currently needed for GUM -- HWL */
    RtsFlags.GcFlags.generations        = 1;
    RtsFlags.GcFlags.steps              = 2;
    RtsFlags.GcFlags.squeezeUpdFrames	= rtsFalse;
#else
    RtsFlags.GcFlags.generations        = 2;
    RtsFlags.GcFlags.steps              = 2;
    RtsFlags.GcFlags.squeezeUpdFrames	= rtsTrue;
#endif
    RtsFlags.GcFlags.compact            = rtsFalse;
    RtsFlags.GcFlags.compactThreshold   = 30.0;
#ifdef RTS_GTK_FRONTPANEL
    RtsFlags.GcFlags.frontpanel         = rtsFalse;
#endif

#ifdef DEBUG
    RtsFlags.DebugFlags.scheduler	= rtsFalse;
    RtsFlags.DebugFlags.interpreter	= rtsFalse;
    RtsFlags.DebugFlags.codegen		= rtsFalse;
    RtsFlags.DebugFlags.weak		= rtsFalse;
    RtsFlags.DebugFlags.gccafs		= rtsFalse;
    RtsFlags.DebugFlags.gc		= rtsFalse;
    RtsFlags.DebugFlags.block_alloc	= rtsFalse;
    RtsFlags.DebugFlags.sanity		= rtsFalse;
    RtsFlags.DebugFlags.stable		= rtsFalse;
    RtsFlags.DebugFlags.prof		= rtsFalse;
    RtsFlags.DebugFlags.gran		= rtsFalse;
    RtsFlags.DebugFlags.par		= rtsFalse;
    RtsFlags.DebugFlags.linker		= rtsFalse;
#endif

#if defined(PROFILING) || defined(PAR)
    RtsFlags.CcFlags.doCostCentres	= 0;
#endif /* PROFILING or PAR */

#ifdef PROFILING
    RtsFlags.ProfFlags.doHeapProfile      = rtsFalse;
    RtsFlags.ProfFlags.profileInterval    = 100;
    RtsFlags.ProfFlags.includeTSOs        = rtsFalse;
    RtsFlags.ProfFlags.showCCSOnException = rtsFalse;
    RtsFlags.ProfFlags.maxRetainerSetSize = 8;
    RtsFlags.ProfFlags.modSelector        = NULL;
    RtsFlags.ProfFlags.descrSelector      = NULL;
    RtsFlags.ProfFlags.typeSelector       = NULL;
    RtsFlags.ProfFlags.ccSelector         = NULL;
    RtsFlags.ProfFlags.ccsSelector        = NULL;
    RtsFlags.ProfFlags.retainerSelector   = NULL;
    RtsFlags.ProfFlags.bioSelector        = NULL;

#elif defined(DEBUG)
    RtsFlags.ProfFlags.doHeapProfile      = rtsFalse;
#endif

    RtsFlags.ConcFlags.ctxtSwitchTime	= CS_MIN_MILLISECS;  /* In milliseconds */

#ifdef SMP
    RtsFlags.ParFlags.nNodes	        = 1;
#endif

#ifdef PAR
    RtsFlags.ParFlags.ParStats.Full   	  = rtsFalse;
    RtsFlags.ParFlags.ParStats.Suppressed = rtsFalse;
    RtsFlags.ParFlags.ParStats.Binary 	  = rtsFalse;
    RtsFlags.ParFlags.ParStats.Sparks 	  = rtsFalse;
    RtsFlags.ParFlags.ParStats.Heap   	  = rtsFalse;
    RtsFlags.ParFlags.ParStats.NewLogfile = rtsFalse;
    RtsFlags.ParFlags.ParStats.Global     = rtsFalse;

    RtsFlags.ParFlags.outputDisabled	= rtsFalse;
#ifdef DIST
    RtsFlags.ParFlags.doFairScheduling  = rtsTrue;  /* fair sched by def */
#else
    RtsFlags.ParFlags.doFairScheduling  = rtsFalse;  /* unfair sched by def */
#endif
    RtsFlags.ParFlags.packBufferSize	= 1024;
    RtsFlags.ParFlags.thunksToPack      = 1; /* 0 ... infinity; */
    RtsFlags.ParFlags.globalising       = 1; /* 0 ... everything */
    RtsFlags.ParFlags.maxThreads        = 1024;
    RtsFlags.ParFlags.maxFishes        = MAX_FISHES;
    RtsFlags.ParFlags.fishDelay         = FISH_DELAY;
#endif

#if defined(PAR) || defined(SMP)
    RtsFlags.ParFlags.maxLocalSparks	= 4096;
#endif /* PAR || SMP */

#if defined(GRAN)
    /* ToDo: check defaults for GranSim and GUM */
    RtsFlags.GcFlags.maxStkSize		= (1024 * 1024) / sizeof(W_);
    RtsFlags.GcFlags.initialStkSize	= 1024 / sizeof(W_);

    RtsFlags.GranFlags.maxThreads	= 65536; // refers to mandatory threads
    RtsFlags.GranFlags.GranSimStats.Full	= rtsFalse;
    RtsFlags.GranFlags.GranSimStats.Suppressed	= rtsFalse;
    RtsFlags.GranFlags.GranSimStats.Binary      = rtsFalse;
    RtsFlags.GranFlags.GranSimStats.Sparks      = rtsFalse;
    RtsFlags.GranFlags.GranSimStats.Heap        = rtsFalse;
    RtsFlags.GranFlags.GranSimStats.NewLogfile  = rtsFalse;
    RtsFlags.GranFlags.GranSimStats.Global      = rtsFalse;

    RtsFlags.GranFlags.packBufferSize	= 1024;
    RtsFlags.GranFlags.packBufferSize_internal = GRANSIM_DEFAULT_PACK_BUFFER_SIZE;

    RtsFlags.GranFlags.proc         = MAX_PROC;
    RtsFlags.GranFlags.Fishing      = rtsFalse;
    RtsFlags.GranFlags.maxFishes   = MAX_FISHES;
    RtsFlags.GranFlags.time_slice   = GRAN_TIME_SLICE;
    RtsFlags.GranFlags.Light        = rtsFalse;

    RtsFlags.GranFlags.Costs.latency =             LATENCY;          
    RtsFlags.GranFlags.Costs.additional_latency =  ADDITIONAL_LATENCY; 
    RtsFlags.GranFlags.Costs.fetchtime =           FETCHTIME; 
    RtsFlags.GranFlags.Costs.lunblocktime =        LOCALUNBLOCKTIME; 
    RtsFlags.GranFlags.Costs.gunblocktime =        GLOBALUNBLOCKTIME;
    RtsFlags.GranFlags.Costs.mpacktime =           MSGPACKTIME;      
    RtsFlags.GranFlags.Costs.munpacktime =         MSGUNPACKTIME;
    RtsFlags.GranFlags.Costs.mtidytime =           MSGTIDYTIME;

    RtsFlags.GranFlags.Costs.threadcreatetime =         THREADCREATETIME;
    RtsFlags.GranFlags.Costs.threadqueuetime =          THREADQUEUETIME;
    RtsFlags.GranFlags.Costs.threaddescheduletime =     THREADDESCHEDULETIME;
    RtsFlags.GranFlags.Costs.threadscheduletime =       THREADSCHEDULETIME;
    RtsFlags.GranFlags.Costs.threadcontextswitchtime =  THREADCONTEXTSWITCHTIME;

    RtsFlags.GranFlags.Costs.arith_cost =         ARITH_COST;       
    RtsFlags.GranFlags.Costs.branch_cost =        BRANCH_COST; 
    RtsFlags.GranFlags.Costs.load_cost =          LOAD_COST;        
    RtsFlags.GranFlags.Costs.store_cost =         STORE_COST; 
    RtsFlags.GranFlags.Costs.float_cost =         FLOAT_COST;       

    RtsFlags.GranFlags.Costs.heapalloc_cost =     HEAPALLOC_COST;

    RtsFlags.GranFlags.Costs.pri_spark_overhead = PRI_SPARK_OVERHEAD;        
    RtsFlags.GranFlags.Costs.pri_sched_overhead = PRI_SCHED_OVERHEAD;        

    RtsFlags.GranFlags.DoFairSchedule           = rtsFalse;             
    RtsFlags.GranFlags.DoAsyncFetch             = rtsFalse;        
    RtsFlags.GranFlags.DoStealThreadsFirst      = rtsFalse;        
    RtsFlags.GranFlags.DoAlwaysCreateThreads    = rtsFalse;      
    RtsFlags.GranFlags.DoBulkFetching           = rtsFalse;             
    RtsFlags.GranFlags.DoThreadMigration        = rtsFalse;          
    RtsFlags.GranFlags.FetchStrategy            = 2;                     
    RtsFlags.GranFlags.PreferSparksOfLocalNodes = rtsFalse;   
    RtsFlags.GranFlags.DoPrioritySparking       = rtsFalse;         
    RtsFlags.GranFlags.DoPriorityScheduling     = rtsFalse;       
    RtsFlags.GranFlags.SparkPriority            = 0;
    RtsFlags.GranFlags.SparkPriority2           = 0; 
    RtsFlags.GranFlags.RandomPriorities         = rtsFalse;           
    RtsFlags.GranFlags.InversePriorities        = rtsFalse;          
    RtsFlags.GranFlags.IgnorePriorities         = rtsFalse;           
    RtsFlags.GranFlags.ThunksToPack             = 0;                      
    RtsFlags.GranFlags.RandomSteal              = rtsTrue;
#endif

#ifdef TICKY_TICKY
    RtsFlags.TickyFlags.showTickyStats	 = rtsFalse;
    RtsFlags.TickyFlags.tickyFile	 = NULL;
#endif
}

#ifdef DEBUG

# ifdef STANDALONE

/* These functions are available in support libraries. */

extern int iswblank(wint_t wc);
extern double atof_hop(const char *nptr);
#  define isspace(X) iswblank(X)
#  define atof atof_hop

# endif

static const char *
usage_text[] = {
"",
"Usage: <prog> <args> [+RTS <rtsopts> | -RTS <args>] ... --RTS <args>",
"",
"   +RTS    Indicates run time system options follow",
"   -RTS    Indicates program arguments follow",
"  --RTS    Indicates that ALL subsequent arguments will be given to the",
"           program (including any of these RTS flags)",
"",
"The following run time system options are available:",
"",
"  -?       Prints this message and exits; the program is not executed",
"",
"  -K<size> Sets the maximum stack size (default 1M)  Egs: -K32k   -K512k",
"  -k<size> Sets the initial thread stack size (default 1k)  Egs: -k4k   -k2m",
"",
"  -A<size> Sets the minimum allocation area size (default 256k) Egs: -A1m -A10k",
"  -M<size> Sets the maximum heap size (default unlimited)  Egs: -M256k -M1G",
"  -H<size> Sets the minimum heap size (default 0M)   Egs: -H24m  -H1G",
"  -m<n>    Minimum % of heap which must be available (default 3%)",
"  -G<n>    Number of generations (default: 2)",
"  -T<n>    Number of steps in younger generations (default: 2)",
"  -c<n>    Auto-enable compaction of the oldest generation when live data is",
"           at least <n>% of the maximum heap size set with -M (default: 30%)",
"  -c       Enable compaction for all major collections",
"",
"  -t<file> One-line GC statistics  (default file: <program>.stat)",
"  -s<file> Summary  GC statistics  (with -Sstderr going to stderr)",
"  -S<file> Detailed GC statistics",
#ifdef RTS_GTK_FRONTPANEL
"  -f       Display front panel (requires X11 & GTK+)",
#endif
"",
"",
"  -Z       Don't squeeze out update frames on stack overflow",
"  -B       Sound the bell at the start of each garbage collection",
#if defined(PROFILING) || defined(PAR)
"",
"  -px      Time/allocation profile (XML)  (output file <program>.prof)",
"  -p       Time/allocation profile        (output file <program>.prof)",
"  -P       More detailed Time/Allocation profile",
"  -Pa      Give information about *all* cost centres",

# if defined(PROFILING)
"",
"  -hx            Heap residency profile (XML)   (output file <program>.prof)",
"  -h<break-down> Heap residency profile (hp2ps) (output file <program>.hp)",
"     break-down: c = cost centre stack (default)",
"                 m = module",
"                 d = closure description",
"                 y = type description",
"                 r = retainer",
"                 b = biography (LAG,DRAG,VOID,USE)",
"  A subset of closures may be selected thusly:",
"    -hc<cc>,...  specific cost centre(s) (top of stack only)",
"    -hC<cc>,...  specific cost centre(s) (anywhere in stack)",
"    -hm<mod>...  all cost centres from the specified modules(s)",
"    -hd<des>,... closures with specified closure descriptions",
"    -hy<typ>...  closures with specified type descriptions",
"    -hr<cc>...   closures with specified retainers",
"    -hb<bio>...  closures with specified biographies (lag,drag,void,use)",
"",
"  -R<size>       Set the maximum retainer set size (default: 8)",
"",
"  -i<sec>        Time between heap samples (seconds, default: 0.1)",
"",
"  -xt            Include threads (TSOs) in a heap profile",
"",
"  -xc      Show current cost centre stack on raising an exception",
# endif
#endif /* PROFILING or PAR */
#if !defined(PROFILING) && defined(DEBUG)
"",
"  -h<break-down> Debugging Heap residency profile",
"                 (output file <program>.hp)",
"     break-down: L = closure label (default)",
"                 T = closure type (constructor, thunk etc.)",
#endif
"",
#if defined(TICKY_TICKY)
"  -r<file>  Produce reduction profiling statistics (with -rstderr for stderr)",
"",
#endif
#if defined(PAR)
"  -N<n>     Use <n> PVMish processors in parallel (default: 2)",
/* NB: the -N<n> is implemented by the driver!! */
#endif
"  -C<secs>  Context-switch interval in seconds",
"                (0 or no argument means switch as often as possible)",
"                the default is .02 sec; resolution is .02 sec",
"",
#if defined(DEBUG)
"  -Ds  DEBUG: scheduler",
"  -Di  DEBUG: interpreter",
"  -Dc  DEBUG: codegen",
"  -Dw  DEBUG: weak",
"  -DG  DEBUG: gccafs",
"  -Dg  DEBUG: gc",
"  -Db  DEBUG: block",
"  -DS  DEBUG: sanity",
"  -Dt  DEBUG: stable",
"  -Dp  DEBUG: prof",
"  -Dr  DEBUG: gran",
"  -DP  DEBUG: par",
"  -Dl  DEBUG: linker",
"",
#endif // DEBUG
#if defined(SMP)
"  -N<n>     Use <n> OS threads (default: 1)",
#endif
#if defined(SMP) || defined(PAR)
"  -e<size>  Size of spark pools (default 100)",
#endif
#if defined(PAR)
"  -t<num>   Set maximum number of advisory threads per PE (default 32)",
"  -qP       Enable activity profile (output files in ~/<program>*.gr)",
"  -qQ<size> Set pack-buffer size (default: 1024)",
"  -qd       Turn on PVM-ish debugging",
"  -qO       Disable output for performance measurement",
#endif
#if defined(SMP) || defined(PAR)
"  -e<n>     Maximum number of outstanding local sparks (default: 4096)",
#endif
#if defined(PAR)
"  -d        Turn on PVM-ish debugging",
"  -O        Disable output for performance measurement",
#endif /* PAR */
#if defined(GRAN)  /* ToDo: fill in decent Docu here */
"  -b...     All GranSim options start with -b; see GranSim User's Guide for details",
#endif
"",
"RTS options may also be specified using the GHCRTS environment variable.",
"",
"Other RTS options may be available for programs compiled a different way.",
"The GHC User's Guide has full details.",
"",
0
};

static inline rtsBool
strequal(const char *a, const char * b)
{
    return(strcmp(a, b) == 0);
}

static void
splitRtsFlags(char *s, int *rts_argc, char *rts_argv[])
{
    char *c1, *c2;

    c1 = s;
    do {
	while (isspace(*c1)) { c1++; };
	c2 = c1;
	while (!isspace(*c2) && *c2 != '\0') { c2++; };
	
	if (c1 == c2) { break; }
	
	if (*rts_argc < MAX_RTS_ARGS-1) {
	    s = stgMallocBytes(c2-c1+1, "RtsFlags.c:splitRtsFlags()");
	    strncpy(s, c1, c2-c1);
	    s[c2-c1] = '\0';
	    rts_argv[(*rts_argc)++] = s;
	} else {
	    barf("too many RTS arguments (max %d)", MAX_RTS_ARGS-1);
	}
	
	c1 = c2;
    } while (*c1 != '\0');
}
    
void
setupRtsFlags(int *argc, char *argv[], int *rts_argc, char *rts_argv[])
{
    rtsBool error = rtsFalse;
    I_ mode;
    I_ arg, total_arg;
    char *last_slash;

    /* Remove directory from argv[0] -- default files in current directory */
    if ((last_slash = (char *) strrchr(argv[0], 
#if !defined(mingw32_TARGET_OS)
				       '/')
#else
				       '\\')
#endif
				       ) != NULL) {
	prog_name = last_slash+1;
    } else {
	prog_name = argv[0];
    }

    total_arg = *argc;
    arg = 1;

    *argc = 1;
    *rts_argc = 0;

    // process arguments from the ghc_rts_opts global variable first.
    // (arguments from the GHCRTS environment variable and the command
    // line override these).
    {
	if (ghc_rts_opts != NULL) {
	    splitRtsFlags(ghc_rts_opts, rts_argc, rts_argv);
	}
    }

    // process arguments from the GHCRTS environment variable next
    // (arguments from the command line override these).
    {
	char *ghc_rts = getenv("GHCRTS");

	if (ghc_rts != NULL) {
	    splitRtsFlags(ghc_rts, rts_argc, rts_argv);
	}
    }

    // Split arguments (argv) into PGM (argv) and RTS (rts_argv) parts
    //   argv[0] must be PGM argument -- leave in argv

    for (mode = PGM; arg < total_arg; arg++) {
	// The '--RTS' argument disables all future +RTS ... -RTS processing.
	if (strequal("--RTS", argv[arg])) {
	    arg++;
	    break;
	}
	// The '--' argument is passed through to the program, but
	// disables all further +RTS ... -RTS processing.
	else if (strequal("--", argv[arg])) {
	    break;
	}
	else if (strequal("+RTS", argv[arg])) {
	    mode = RTS;
	}
	else if (strequal("-RTS", argv[arg])) {
	    mode = PGM;
	}
	else if (mode == RTS && *rts_argc < MAX_RTS_ARGS-1) {
	    rts_argv[(*rts_argc)++] = argv[arg];
	}
	else if (mode == PGM) {
	    argv[(*argc)++] = argv[arg];
	}
	else {
	  barf("too many RTS arguments (max %d)", MAX_RTS_ARGS-1);
	}
    }
    // process remaining program arguments
    for (; arg < total_arg; arg++) {
	argv[(*argc)++] = argv[arg];
    }
    argv[*argc] = (char *) 0;
    rts_argv[*rts_argc] = (char *) 0;

    // Process RTS (rts_argv) part: mainly to determine statsfile
    for (arg = 0; arg < *rts_argc; arg++) {
      fprintf(stderr, "\nrts_arg: %s\n", rts_argv[arg]);
	if (rts_argv[arg][0] != '-') {
	    fflush(stdout);
	    prog_belch("unexpected RTS argument: %s", rts_argv[arg]);
	    error = rtsTrue;

        } else {
	    switch(rts_argv[arg][1]) {

	      /* process: general args, then PROFILING-only ones,
		 then CONCURRENT-only, PARallel-only, GRAN-only,
		 TICKY-only (same order as defined in RtsFlags.lh);
		 within those groups, mostly in case-insensitive
		 alphabetical order.
                 Final group is x*, which allows for more options.
	      */

#ifdef TICKY_TICKY
# define TICKY_BUILD_ONLY(x) x
#else
# define TICKY_BUILD_ONLY(x) \
prog_belch("GHC not built for: ticky-ticky stats"); \
error = rtsTrue;
#endif

#if defined(PROFILING) 
# define COST_CENTRE_USING_BUILD_ONLY(x) x
#else
# define COST_CENTRE_USING_BUILD_ONLY(x) \
prog_belch("GHC not built for: -prof or -parallel"); \
error = rtsTrue;
#endif

#ifdef PROFILING
# define PROFILING_BUILD_ONLY(x)   x
#else
# define PROFILING_BUILD_ONLY(x) \
prog_belch("GHC not built for: -prof"); \
error = rtsTrue;
#endif

#ifdef SMP
# define SMP_BUILD_ONLY(x)      x
#else
# define SMP_BUILD_ONLY(x) \
prog_belch("GHC not built for: -smp"); \
error = rtsTrue;
#endif

#ifdef PAR
# define PAR_BUILD_ONLY(x)      x
#else
# define PAR_BUILD_ONLY(x) \
prog_belch("GHC not built for: -parallel"); \
error = rtsTrue;
#endif

#if defined(SMP) || defined(PAR)
# define PAR_OR_SMP_BUILD_ONLY(x)      x
#else
# define PAR_OR_SMP_BUILD_ONLY(x) \
prog_belch("GHC not built for: -parallel or -smp"); \
error = rtsTrue;
#endif

#ifdef GRAN
# define GRAN_BUILD_ONLY(x)     x
#else
# define GRAN_BUILD_ONLY(x) \
prog_belch("GHC not built for: -gransim"); \
error = rtsTrue;
#endif

	      /* =========== GENERAL ========================== */
	      case '?':
		error = rtsTrue;
		break;

	      case 'A':
		RtsFlags.GcFlags.minAllocAreaSize
		  = decode(rts_argv[arg]+2) / BLOCK_SIZE;
		if (RtsFlags.GcFlags.minAllocAreaSize <= 0) {
		  bad_option(rts_argv[arg]);
		}
		break;

	      case 'B':
		RtsFlags.GcFlags.ringBell = rtsTrue;
		break;

	      case 'c':
		  if (rts_argv[arg][2] != '\0') {
		      RtsFlags.GcFlags.compactThreshold =
			  atof(rts_argv[arg]+2);
		  } else {
		      RtsFlags.GcFlags.compact = rtsTrue;
		  }
		  break;

	      case 'F':
	        RtsFlags.GcFlags.oldGenFactor = atof(rts_argv[arg]+2);
	      
		if (RtsFlags.GcFlags.oldGenFactor < 0)
		  bad_option( rts_argv[arg] );
		break;
	      
#ifdef DEBUG
	      case 'D':
	      { 
		  char *c;

		  for (c  = rts_argv[arg] + 2; *c != '\0'; c++) {
		      switch (*c) {
		      case 's':
			  RtsFlags.DebugFlags.scheduler = rtsTrue;
			  break;
		      case 'i':
			  RtsFlags.DebugFlags.interpreter = rtsTrue;
			  break;
		      case 'c':
			  RtsFlags.DebugFlags.codegen = rtsTrue;
			  break;
		      case 'w':
			  RtsFlags.DebugFlags.weak = rtsTrue;
			  break;
		      case 'G':
			  RtsFlags.DebugFlags.gccafs = rtsTrue;
			  break;
		      case 'g':
			  RtsFlags.DebugFlags.gc = rtsTrue;
			  break;
		      case 'b':
			  RtsFlags.DebugFlags.block_alloc = rtsTrue;
			  break;
		      case 'S':
			  RtsFlags.DebugFlags.sanity = rtsTrue;
			  break;
		      case 't':
			  RtsFlags.DebugFlags.stable = rtsTrue;
			  break;
		      case 'p':
			  RtsFlags.DebugFlags.prof = rtsTrue;
			  break;
		      case 'r':
			  RtsFlags.DebugFlags.gran = rtsTrue;
			  break;
		      case 'P':
			  RtsFlags.DebugFlags.par = rtsTrue;
			  break;
		      case 'l':
			  RtsFlags.DebugFlags.linker = rtsTrue;
			  break;
		      case 'a':
			  RtsFlags.DebugFlags.apply = rtsTrue;
			  break;
		      default:
			  bad_option( rts_argv[arg] );
		      }
		  }
		  break;
	      }
#endif

	      case 'K':
		RtsFlags.GcFlags.maxStkSize = 
		  decode(rts_argv[arg]+2) / sizeof(W_);

		if (RtsFlags.GcFlags.maxStkSize == 0) 
		  bad_option( rts_argv[arg] );
		break;

	      case 'k':
		RtsFlags.GcFlags.initialStkSize = 
		  decode(rts_argv[arg]+2) / sizeof(W_);

		if (RtsFlags.GcFlags.initialStkSize == 0) 
		  bad_option( rts_argv[arg] );
		break;

	      case 'M':
		RtsFlags.GcFlags.maxHeapSize = 
		  decode(rts_argv[arg]+2) / BLOCK_SIZE;
		/* user give size in *bytes* but "maxHeapSize" is in *blocks* */

		if (RtsFlags.GcFlags.maxHeapSize <= 0) {
		  bad_option(rts_argv[arg]);
		}
		break;

	      case 'm':
		RtsFlags.GcFlags.pcFreeHeap = atof(rts_argv[arg]+2);

		if (RtsFlags.GcFlags.pcFreeHeap < 0 || 
		    RtsFlags.GcFlags.pcFreeHeap > 100)
		  bad_option( rts_argv[arg] );
		break;

	      case 'G':
		RtsFlags.GcFlags.generations = decode(rts_argv[arg]+2);
		if (RtsFlags.GcFlags.generations < 1) {
		  bad_option(rts_argv[arg]);
		}
		break;

	      case 'T':
		RtsFlags.GcFlags.steps = decode(rts_argv[arg]+2);
		if (RtsFlags.GcFlags.steps < 1) {
		  bad_option(rts_argv[arg]);
		}
		break;

	      case 'H':
		RtsFlags.GcFlags.heapSizeSuggestion = 
		  decode(rts_argv[arg]+2) / BLOCK_SIZE;

		if (RtsFlags.GcFlags.heapSizeSuggestion <= 0) {
		  bad_option(rts_argv[arg]);
		}
		break;

#ifdef RTS_GTK_FRONTPANEL
	      case 'f':
		  RtsFlags.GcFlags.frontpanel = rtsTrue;
		  break;
#endif

	      case 'S':
		  RtsFlags.GcFlags.giveStats = VERBOSE_GC_STATS;
		  goto stats;

	      case 's':
		  RtsFlags.GcFlags.giveStats = SUMMARY_GC_STATS;
		  goto stats;

	      case 't':
		  RtsFlags.GcFlags.giveStats = ONELINE_GC_STATS;
		  goto stats;

	    stats:
#ifdef PAR
		/* Opening all those files would almost certainly fail... */
		// RtsFlags.ParFlags.ParStats.Full = rtsTrue;
		RtsFlags.GcFlags.statsFile = stderr; /* temporary; ToDo: rm */
#else
		  RtsFlags.GcFlags.statsFile
		      = open_stats_file(arg, *argc, argv,
					*rts_argc, rts_argv, STAT_FILENAME_FMT);
		  
		  if (RtsFlags.GcFlags.statsFile == NULL) error = rtsTrue;
#endif
		  break;

	      case 'Z':
		RtsFlags.GcFlags.squeezeUpdFrames = rtsFalse;
		break;

	      /* =========== PROFILING ========================== */

	      case 'P': /* detailed cost centre profiling (time/alloc) */
	      case 'p': /* cost centre profiling (time/alloc) */
		COST_CENTRE_USING_BUILD_ONLY(
		switch (rts_argv[arg][2]) {
		  case 'x':
		    RtsFlags.CcFlags.doCostCentres = COST_CENTRES_XML;
		    break;
		  case 'a':
		    RtsFlags.CcFlags.doCostCentres = COST_CENTRES_ALL;
		    break;
		  default:
		      if (rts_argv[arg][1] == 'P') {
			  RtsFlags.CcFlags.doCostCentres =
			      COST_CENTRES_VERBOSE;
		      } else {
			  RtsFlags.CcFlags.doCostCentres =
			      COST_CENTRES_SUMMARY;
		      }
		      break;
		}
		) break;

	      case 'R':
		  PROFILING_BUILD_ONLY(
		      RtsFlags.ProfFlags.maxRetainerSetSize = atof(rts_argv[arg]+2);
  	          ) break;

	      case 'h': /* serial heap profile */
#if !defined(PROFILING) && defined(DEBUG)
		switch (rts_argv[arg][2]) {
		  case '\0':
		  case 'L':
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_INFOPTR;
		    break;
		  case 'T':
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_CLOSURE_TYPE;
		    break;
		  default:
		    prog_belch("invalid heap profile option: %s",rts_argv[arg]);
		    error = rtsTrue;
		}
#else
		PROFILING_BUILD_ONLY(
		switch (rts_argv[arg][2]) {
		case '\0':
		case 'C':
		case 'c':
		case 'M':
		case 'm':
		case 'D':
		case 'd':
		case 'Y':
		case 'y':
		case 'R':
		case 'r':
		case 'B':
		case 'b':
		    if (rts_argv[arg][2] != '\0' && rts_argv[arg][3] != '\0') {
			{
			    char *left  = strchr(rts_argv[arg], '{');
			    char *right = strrchr(rts_argv[arg], '}');

			    // curly braces are optional, for
			    // backwards compat.
			    if (left)
				left = left+1;
			    else
				left = rts_argv[arg] + 3;

			    if (!right)
				right = rts_argv[arg] + strlen(rts_argv[arg]);

			    *right = '\0';

			    switch (rts_argv[arg][2]) {
			    case 'c': // cost centre label select
				RtsFlags.ProfFlags.ccSelector = left;
				break;
			    case 'C':
				RtsFlags.ProfFlags.ccsSelector = left;
				break;
			    case 'M':
			    case 'm': // cost centre module select
				RtsFlags.ProfFlags.modSelector = left;
				break;
			    case 'D':
			    case 'd': // closure descr select 
				RtsFlags.ProfFlags.descrSelector = left;
				break;
			    case 'Y':
			    case 'y': // closure type select
				RtsFlags.ProfFlags.typeSelector = left;
				break;
			    case 'R':
			    case 'r': // retainer select
				RtsFlags.ProfFlags.retainerSelector = left;
				break;
			    case 'B':
			    case 'b': // biography select
				RtsFlags.ProfFlags.bioSelector = left;
				break;
			    }
			}
			break;
		    }

		    if (RtsFlags.ProfFlags.doHeapProfile != 0) {
			prog_belch("multiple heap profile options");
			error = rtsTrue;
			break;
		    }

		    switch (rts_argv[arg][2]) {
		    case '\0':
		    case 'C':
		    case 'c':
			RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_CCS;
			break;
		    case 'M':
		    case 'm':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_MOD;
			  break;
		    case 'D':
		    case 'd':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_DESCR;
			  break;
		    case 'Y':
		    case 'y':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_TYPE;
			  break;
		    case 'R':
		    case 'r':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_RETAINER;
			  break;
		    case 'B':
		    case 'b':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_LDV;
			  break;
		    }
		    break;
		      
		default:
		    prog_belch("invalid heap profile option: %s",rts_argv[arg]);
		    error = rtsTrue;
		}
		) 
#endif // PROFILING
    	    	break;

#if defined(PROFILING) 
    	      case 'i':	/* heap sample interval */
		if (rts_argv[arg][2] == '\0') {
		  /* use default */
		} else {
		    I_ cst; /* tmp */

		    /* Convert to milliseconds */
		    cst = (I_) ((atof(rts_argv[arg]+2) * 1000));
		    cst = (cst / CS_MIN_MILLISECS) * CS_MIN_MILLISECS;
		    if (cst != 0 && cst < CS_MIN_MILLISECS)
			cst = CS_MIN_MILLISECS;

		    RtsFlags.ProfFlags.profileInterval = cst;
		}
		break;
#endif

	      /* =========== CONCURRENT ========================= */
    	      case 'C':	/* context switch interval */
		if (rts_argv[arg][2] == '\0')
    	    	    RtsFlags.ConcFlags.ctxtSwitchTime = 0;
		else {
		    I_ cst; /* tmp */

		    /* Convert to milliseconds */
		    cst = (I_) ((atof(rts_argv[arg]+2) * 1000));
		    cst = (cst / CS_MIN_MILLISECS) * CS_MIN_MILLISECS;
		    if (cst != 0 && cst < CS_MIN_MILLISECS)
			cst = CS_MIN_MILLISECS;

		    RtsFlags.ConcFlags.ctxtSwitchTime = cst;
		}
    	    	break;

#ifdef SMP
	      case 'N':
		SMP_BUILD_ONLY(
		if (rts_argv[arg][2] != '\0') {
		    RtsFlags.ParFlags.nNodes
		      = strtol(rts_argv[arg]+2, (char **) NULL, 10);
		    if (RtsFlags.ParFlags.nNodes <= 0) {
		      prog_belch("bad value for -N");
		      error = rtsTrue;
		    }
		}
		) break;
#endif
	      /* =========== PARALLEL =========================== */
	      case 'e':
		PAR_OR_SMP_BUILD_ONLY(
		if (rts_argv[arg][2] != '\0') {
		    RtsFlags.ParFlags.maxLocalSparks
		      = strtol(rts_argv[arg]+2, (char **) NULL, 10);
		    if (RtsFlags.ParFlags.maxLocalSparks <= 0) {
		      prog_belch("bad value for -e");
		      error = rtsTrue;
		    }
		}
		) break;

    	      case 'q':
		PAR_BUILD_ONLY(
		  process_par_option(arg, rts_argc, rts_argv, &error);
		) break;

	      /* =========== GRAN =============================== */

    	      case 'b':
		GRAN_BUILD_ONLY(
		  process_gran_option(arg, rts_argc, rts_argv, &error);
		) break;

	      /* =========== TICKY ============================== */

	      case 'r': /* Basic profiling stats */
		TICKY_BUILD_ONLY(

		RtsFlags.TickyFlags.showTickyStats = rtsTrue;
		RtsFlags.TickyFlags.tickyFile
		  = open_stats_file(arg, *argc, argv,
			*rts_argc, rts_argv, TICKY_FILENAME_FMT);

		if (RtsFlags.TickyFlags.tickyFile == NULL) error = rtsTrue;
	        ) break;

	      /* =========== EXTENDED OPTIONS =================== */

              case 'x': /* Extend the argument space */
                switch(rts_argv[arg][2]) {
                  case '\0':
		    prog_belch("incomplete RTS option: %s",rts_argv[arg]);
		    error = rtsTrue;
		    break;

                  case 'c': /* Debugging tool: show current cost centre on an exception */
                    PROFILING_BUILD_ONLY(
			RtsFlags.ProfFlags.showCCSOnException = rtsTrue;
			);
		    break;

		case 't':  /* Include memory used by TSOs in a heap profile */
		    PROFILING_BUILD_ONLY(
			RtsFlags.ProfFlags.includeTSOs = rtsTrue;
			);
		    break;

                  /* The option prefix '-xx' is reserved for future extension.  KSW 1999-11. */

	          default:
		    prog_belch("unknown RTS option: %s",rts_argv[arg]);
		    error = rtsTrue;
		    break;
                }
                break;  /* defensive programming */

	      /* =========== OH DEAR ============================ */
	      default:
		prog_belch("unknown RTS option: %s",rts_argv[arg]);
		error = rtsTrue;
		break;
	    }
	}
    }
    if (error) {
	const char **p;

        fflush(stdout);
	for (p = usage_text; *p; p++)
	    belch("%s", *p);
	stg_exit(EXIT_FAILURE);
    }
}

#if defined(GRAN)

//@node GranSim specific options, Aux fcts, Command-line option parsing routines
//@subsection GranSim specific options

static void
enable_GranSimLight(void) {

    fprintf(stderr,"GrAnSim Light enabled (infinite number of processors;  0 communication costs)\n");
    RtsFlags.GranFlags.Light=rtsTrue;
    RtsFlags.GranFlags.Costs.latency = 
	RtsFlags.GranFlags.Costs.fetchtime = 
	RtsFlags.GranFlags.Costs.additional_latency =
	RtsFlags.GranFlags.Costs.gunblocktime = 
	RtsFlags.GranFlags.Costs.lunblocktime =
	RtsFlags.GranFlags.Costs.threadcreatetime = 
	RtsFlags.GranFlags.Costs.threadqueuetime =
	RtsFlags.GranFlags.Costs.threadscheduletime = 
	RtsFlags.GranFlags.Costs.threaddescheduletime =
	RtsFlags.GranFlags.Costs.threadcontextswitchtime = 0;
  
    RtsFlags.GranFlags.Costs.mpacktime = 
	RtsFlags.GranFlags.Costs.munpacktime = 0;

    RtsFlags.GranFlags.DoFairSchedule = rtsTrue;
    RtsFlags.GranFlags.DoAsyncFetch = rtsFalse;
    RtsFlags.GranFlags.DoAlwaysCreateThreads = rtsTrue;
    /* FetchStrategy is irrelevant in GrAnSim-Light */

    /* GrAnSim Light often creates an abundance of parallel threads,
       each with its own stack etc. Therefore, it's in general a good
       idea to use small stack chunks (use the -o<size> option to 
       increase it again). 
    */
    // RtsFlags.ConcFlags.stkChunkSize = 100;

    RtsFlags.GranFlags.proc = 1; 
}

static void
process_gran_option(int arg, int *rts_argc, char *rts_argv[], rtsBool *error)
{
    if (rts_argv[arg][1] != 'b') /* All GranSim options start with -b */
      return;

    /* or a ridiculously idealised simulator */
    if(strcmp((rts_argv[arg]+2),"oring")==0) {
      RtsFlags.GranFlags.Costs.latency = 
	RtsFlags.GranFlags.Costs.fetchtime = 
	RtsFlags.GranFlags.Costs.additional_latency =
	RtsFlags.GranFlags.Costs.gunblocktime = 
	RtsFlags.GranFlags.Costs.lunblocktime =
	RtsFlags.GranFlags.Costs.threadcreatetime = 
	RtsFlags.GranFlags.Costs.threadqueuetime =
	RtsFlags.GranFlags.Costs.threadscheduletime = 
	RtsFlags.GranFlags.Costs.threaddescheduletime =
	RtsFlags.GranFlags.Costs.threadcontextswitchtime = 0;

      RtsFlags.GranFlags.Costs.mpacktime = 
	RtsFlags.GranFlags.Costs.munpacktime = 0;

      RtsFlags.GranFlags.Costs.arith_cost = 
	RtsFlags.GranFlags.Costs.float_cost = 
	RtsFlags.GranFlags.Costs.load_cost =
	RtsFlags.GranFlags.Costs.store_cost = 
	RtsFlags.GranFlags.Costs.branch_cost = 0;

      RtsFlags.GranFlags.Costs.heapalloc_cost = 1;

      /* ++RtsFlags.GranFlags.DoFairSchedule; */
      RtsFlags.GranFlags.DoStealThreadsFirst = rtsTrue;        /* -bZ */
      RtsFlags.GranFlags.DoThreadMigration   = rtsTrue;        /* -bM */
      RtsFlags.GranFlags.GranSimStats.Full   = rtsTrue;        /* -bP */
      return;
    }

      /* or a somewhat idealised simulator */
      if(strcmp((rts_argv[arg]+2),"onzo")==0) {
	RtsFlags.GranFlags.Costs.latency = 
	RtsFlags.GranFlags.Costs.fetchtime = 
	RtsFlags.GranFlags.Costs.additional_latency =
	RtsFlags.GranFlags.Costs.gunblocktime = 
	RtsFlags.GranFlags.Costs.lunblocktime =
	RtsFlags.GranFlags.Costs.threadcreatetime = 
	RtsFlags.GranFlags.Costs.threadqueuetime =
	RtsFlags.GranFlags.Costs.threadscheduletime = 
	RtsFlags.GranFlags.Costs.threaddescheduletime =
	RtsFlags.GranFlags.Costs.threadcontextswitchtime = 0;

	RtsFlags.GranFlags.Costs.mpacktime = 
	RtsFlags.GranFlags.Costs.munpacktime = 0;
	
	RtsFlags.GranFlags.Costs.heapalloc_cost = 1;

	/* RtsFlags.GranFlags.DoFairSchedule  = rtsTrue; */       /* -b-R */
	/* RtsFlags.GranFlags.DoStealThreadsFirst = rtsTrue; */   /* -b-T */
	RtsFlags.GranFlags.DoAsyncFetch = rtsTrue;         /* -bZ */
	RtsFlags.GranFlags.DoThreadMigration  = rtsTrue;          /* -bM */
	RtsFlags.GranFlags.GranSimStats.Full  = rtsTrue;          /* -bP */
#  if defined(GRAN_CHECK) && defined(GRAN)
	RtsFlags.GranFlags.Debug.event_stats = rtsTrue; /* print event statistics   */
#  endif
	return;
      }

      /* Communication and task creation cost parameters */
      switch(rts_argv[arg][2]) {
        case '.':
	  IgnoreYields = rtsTrue; // HWL HACK
	  break;

        case ':':
	  enable_GranSimLight();       /* set flags for GrAnSim-Light mode */
	  break;

        case 'l':
	  if (rts_argv[arg][3] != '\0')
	    {
	      RtsFlags.GranFlags.Costs.gunblocktime = 
	      RtsFlags.GranFlags.Costs.latency = decode(rts_argv[arg]+3);
	      RtsFlags.GranFlags.Costs.fetchtime = 2*RtsFlags.GranFlags.Costs.latency;
	    }
	  else
	    RtsFlags.GranFlags.Costs.latency = LATENCY;
	  break;

        case 'a':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.additional_latency = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.additional_latency = ADDITIONAL_LATENCY;
	  break;

        case 'm':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.mpacktime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.mpacktime = MSGPACKTIME;
	  break;

        case 'x':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.mtidytime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.mtidytime = 0;
	  break;

        case 'r':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.munpacktime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.munpacktime = MSGUNPACKTIME;
	  break;
	  
        case 'g':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.fetchtime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.fetchtime = FETCHTIME;
	  break;
	  
        case 'n':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.gunblocktime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.gunblocktime = GLOBALUNBLOCKTIME;
	  break;

        case 'u':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.lunblocktime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.lunblocktime = LOCALUNBLOCKTIME;
	  break;

	/* Thread-related metrics */
        case 't':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.threadcreatetime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.threadcreatetime = THREADCREATETIME;
	  break;
	  
        case 'q':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.threadqueuetime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.threadqueuetime = THREADQUEUETIME;
	  break;
	  
        case 'c':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.threadscheduletime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.threadscheduletime = THREADSCHEDULETIME;
	  
	  RtsFlags.GranFlags.Costs.threadcontextswitchtime = RtsFlags.GranFlags.Costs.threadscheduletime
	    + RtsFlags.GranFlags.Costs.threaddescheduletime;
	  break;

        case 'd':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.threaddescheduletime = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.threaddescheduletime = THREADDESCHEDULETIME;
	  
	  RtsFlags.GranFlags.Costs.threadcontextswitchtime = RtsFlags.GranFlags.Costs.threadscheduletime
	    + RtsFlags.GranFlags.Costs.threaddescheduletime;
	  break;

	/* Instruction Cost Metrics */
        case 'A':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.arith_cost = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.arith_cost = ARITH_COST;
	  break;

        case 'F':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.float_cost = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.float_cost = FLOAT_COST;
	  break;
		      
        case 'B':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.branch_cost = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.branch_cost = BRANCH_COST;
	  break;

        case 'L':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.load_cost = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.load_cost = LOAD_COST;
	  break;
	  
        case 'S':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.store_cost = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.store_cost = STORE_COST;
	  break;

        case 'H':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.heapalloc_cost = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.heapalloc_cost = 0;
	  break;

        case 'y':
	  RtsFlags.GranFlags.DoAsyncFetch = rtsTrue;
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.FetchStrategy = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.FetchStrategy = 2;
	  if (RtsFlags.GranFlags.FetchStrategy == 0)
	    RtsFlags.GranFlags.DoAsyncFetch = rtsFalse;
	  break;
	  
        case 'K':   /* sort overhead (per elem in spark list) */
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.pri_spark_overhead = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.pri_spark_overhead = PRI_SPARK_OVERHEAD;
	  fprintf(stderr,"Overhead for pri spark: %d (per elem).\n",
		         RtsFlags.GranFlags.Costs.pri_spark_overhead);
	  break;

        case 'O':  /* sort overhead (per elem in spark list) */
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.Costs.pri_sched_overhead = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.Costs.pri_sched_overhead = PRI_SCHED_OVERHEAD;
	  fprintf(stderr,"Overhead for pri sched: %d (per elem).\n",
		       RtsFlags.GranFlags.Costs.pri_sched_overhead);
	  break;

        /* General Parameters */
        case 'p':
	  if (rts_argv[arg][3] != '\0')
	    {
	      RtsFlags.GranFlags.proc = decode(rts_argv[arg]+3);
	      if (RtsFlags.GranFlags.proc==0) {
		  enable_GranSimLight(); /* set flags for GrAnSim-Light mode */
	      } else if (RtsFlags.GranFlags.proc > MAX_PROC || 
			 RtsFlags.GranFlags.proc < 1)
		{
		  fprintf(stderr,"setupRtsFlags: no more than %u processors allowed\n",
			  MAX_PROC);
		  *error = rtsTrue;
		}
	    }
	  else
	    RtsFlags.GranFlags.proc = MAX_PROC;
	  break;

        case 'f':
	  RtsFlags.GranFlags.Fishing = rtsTrue;
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.maxFishes = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.maxFishes = MAX_FISHES;
	  break;
	  
        case 'w':
	  if (rts_argv[arg][3] != '\0')
	    RtsFlags.GranFlags.time_slice = decode(rts_argv[arg]+3);
	  else
	    RtsFlags.GranFlags.time_slice = GRAN_TIME_SLICE;
	  break;
	  
        case 'C':
	  RtsFlags.GranFlags.DoAlwaysCreateThreads=rtsTrue;
	  RtsFlags.GranFlags.DoThreadMigration=rtsTrue;
	  break;

        case 'G':
	  fprintf(stderr,"Bulk fetching enabled.\n");
	  RtsFlags.GranFlags.DoBulkFetching=rtsTrue;
	  break;
	  
        case 'M':
	  fprintf(stderr,"Thread migration enabled.\n");
	  RtsFlags.GranFlags.DoThreadMigration=rtsTrue;
	  break;

        case 'R':
	  fprintf(stderr,"Fair Scheduling enabled.\n");
	  RtsFlags.GranFlags.DoFairSchedule=rtsTrue;
	  break;
	  
        case 'I':
	  fprintf(stderr,"Priority Scheduling enabled.\n");
	  RtsFlags.GranFlags.DoPriorityScheduling=rtsTrue;
	  break;

        case 'T':
	  RtsFlags.GranFlags.DoStealThreadsFirst=rtsTrue;
	  RtsFlags.GranFlags.DoThreadMigration=rtsTrue;
	  break;
	  
        case 'Z':
	  RtsFlags.GranFlags.DoAsyncFetch=rtsTrue;
	  break;
	  
/*          case 'z': */
/*  	  RtsFlags.GranFlags.SimplifiedFetch=rtsTrue; */
/*  	  break; */
	  
        case 'N':
	  RtsFlags.GranFlags.PreferSparksOfLocalNodes=rtsTrue;
	  break;
	  
        case 'b':
	  RtsFlags.GranFlags.GranSimStats.Binary=rtsTrue;
	  break;
	  
        case 'P':
	  /* format is -bP<c> where <c> is one char describing kind of profile */
	  RtsFlags.GranFlags.GranSimStats.Full = rtsTrue;
	  switch(rts_argv[arg][3]) {
	  case '\0': break; // nothing special, just an ordinary profile
	  case '0': RtsFlags.GranFlags.GranSimStats.Suppressed = rtsTrue;
	    break;
	  case 'b': RtsFlags.GranFlags.GranSimStats.Binary = rtsTrue;
	    break;
	  case 's': RtsFlags.GranFlags.GranSimStats.Sparks = rtsTrue;
	    break;
	  case 'h': RtsFlags.GranFlags.GranSimStats.Heap = rtsTrue;
	    break;
	  case 'n': RtsFlags.GranFlags.GranSimStats.NewLogfile = rtsTrue;
	    break;
	  case 'g': RtsFlags.GranFlags.GranSimStats.Global = rtsTrue;
	    break;
	  default: barf("Unknown option -bP%c", rts_argv[arg][3]);
	  }
	  break;

        case 's':
	  RtsFlags.GranFlags.GranSimStats.Sparks=rtsTrue;
	  break;

        case 'h':
	  RtsFlags.GranFlags.GranSimStats.Heap=rtsTrue;
	  break;

        case 'Y':   /* syntax: -bY<n>[,<n>]  n ... pos int */ 
	  if (rts_argv[arg][3] != '\0') {
	    char *arg0, *tmp;
	    
	    arg0 = rts_argv[arg]+3;
	    if ((tmp = strstr(arg0,","))==NULL) {
	      RtsFlags.GranFlags.SparkPriority = decode(arg0);
	      fprintf(stderr,"SparkPriority: %u.\n",RtsFlags.GranFlags.SparkPriority);
	    } else {
	      *(tmp++) = '\0'; 
	      RtsFlags.GranFlags.SparkPriority = decode(arg0);
	      RtsFlags.GranFlags.SparkPriority2 = decode(tmp);
	      fprintf(stderr,"SparkPriority: %u.\n",
		      RtsFlags.GranFlags.SparkPriority);
	      fprintf(stderr,"SparkPriority2:%u.\n",
		      RtsFlags.GranFlags.SparkPriority2);
	      if (RtsFlags.GranFlags.SparkPriority2 < 
		  RtsFlags.GranFlags.SparkPriority) {
		fprintf(stderr,"WARNING: 2nd pri < main pri (%u<%u); 2nd pri has no effect\n",
			RtsFlags.GranFlags.SparkPriority2,
			RtsFlags.GranFlags.SparkPriority);
	      }
	    }
	  } else {
	    /* plain pri spark is now invoked with -bX  
	       RtsFlags.GranFlags.DoPrioritySparking = 1;
	       fprintf(stderr,"PrioritySparking.\n");
	    */
	  }
	  break;

        case 'Q':
	  if (rts_argv[arg][3] != '\0') {
	    RtsFlags.GranFlags.ThunksToPack = decode(rts_argv[arg]+3);
	  } else {
	    RtsFlags.GranFlags.ThunksToPack = 1;
	  }
	  fprintf(stderr,"Thunks To Pack in one packet: %u.\n",
		  RtsFlags.GranFlags.ThunksToPack);
	  break;
		      
        case 'e':
	  RtsFlags.GranFlags.RandomSteal = rtsFalse;
	  fprintf(stderr,"Deterministic mode (no random stealing)\n");
		      break;

	  /* The following class of options contains eXperimental */
	  /* features in connection with exploiting granularity */
	  /* information. I.e. if -bY is chosen these options */
	  /* tell the RTS what to do with the supplied info --HWL */

        case 'W':
	  if (rts_argv[arg][3] != '\0') {
	    RtsFlags.GranFlags.packBufferSize_internal = decode(rts_argv[arg]+3);
	  } else {
	    RtsFlags.GranFlags.packBufferSize_internal = GRANSIM_DEFAULT_PACK_BUFFER_SIZE;
	  }
	  fprintf(stderr,"Size of GranSim internal pack buffer: %u.\n",
		  RtsFlags.GranFlags.packBufferSize_internal);
	  break;
  		      
        case 'X':
	  switch(rts_argv[arg][3]) {
	    
	    case '\0':
	      RtsFlags.GranFlags.DoPrioritySparking = 1;
	      fprintf(stderr,"Priority Sparking with Normal Priorities.\n");
	      RtsFlags.GranFlags.InversePriorities = rtsFalse; 
	      RtsFlags.GranFlags.RandomPriorities = rtsFalse;
	      RtsFlags.GranFlags.IgnorePriorities = rtsFalse;
	      break;
			
	    case 'I':
	      RtsFlags.GranFlags.DoPrioritySparking = 1;
	      fprintf(stderr,"Priority Sparking with Inverse Priorities.\n");
	      RtsFlags.GranFlags.InversePriorities++; 
	      break;
	      
	    case 'R': 
	      RtsFlags.GranFlags.DoPrioritySparking = 1;
	      fprintf(stderr,"Priority Sparking with Random Priorities.\n");
	      RtsFlags.GranFlags.RandomPriorities++;
	      break;
	      
	    case 'N':
	      RtsFlags.GranFlags.DoPrioritySparking = 1;
	      fprintf(stderr,"Priority Sparking with No Priorities.\n");
	      RtsFlags.GranFlags.IgnorePriorities++;
	      break;
	      
	    default:
	      bad_option( rts_argv[arg] );
	      break;
	  }
	  break;

        case '-':
	  switch(rts_argv[arg][3]) {
	    
	    case 'C':
	      RtsFlags.GranFlags.DoAlwaysCreateThreads=rtsFalse;
	      RtsFlags.GranFlags.DoThreadMigration=rtsFalse;
	      break;

	    case 'G':
	      RtsFlags.GranFlags.DoBulkFetching=rtsFalse;
	      break;
	      
	    case 'M':
	      RtsFlags.GranFlags.DoThreadMigration=rtsFalse;
	      break;

	    case 'R':
	      RtsFlags.GranFlags.DoFairSchedule=rtsFalse;
	      break;

	    case 'T':
	      RtsFlags.GranFlags.DoStealThreadsFirst=rtsFalse;
	      RtsFlags.GranFlags.DoThreadMigration=rtsFalse;
	      break;

	    case 'Z':
	      RtsFlags.GranFlags.DoAsyncFetch=rtsFalse;
	      break;
	      
	    case 'N':
	      RtsFlags.GranFlags.PreferSparksOfLocalNodes=rtsFalse;
			 break;
			 
	    case 'P':
	      RtsFlags.GranFlags.GranSimStats.Suppressed=rtsTrue;
	      break;

	    case 's':
	      RtsFlags.GranFlags.GranSimStats.Sparks=rtsFalse;
	      break;
	    
	    case 'h':
	      RtsFlags.GranFlags.GranSimStats.Heap=rtsFalse;
	      break;
	    
	    case 'b':
	      RtsFlags.GranFlags.GranSimStats.Binary=rtsFalse;
	      break;
			 
	    case 'X':
	      RtsFlags.GranFlags.DoPrioritySparking = rtsFalse;
	      break;

	    case 'Y':
	      RtsFlags.GranFlags.DoPrioritySparking = rtsFalse;
	      RtsFlags.GranFlags.SparkPriority = rtsFalse;
	      break;

	    case 'I':
	      RtsFlags.GranFlags.DoPriorityScheduling = rtsFalse;
	      break;

	    case 'e':
	      RtsFlags.GranFlags.RandomSteal = rtsFalse;
	      break;

	    default:
	      bad_option( rts_argv[arg] );
	      break;
	  }
	  break;

#  if defined(GRAN_CHECK) && defined(GRAN)
        case 'D':
	  switch(rts_argv[arg][3]) {
	    case 'Q':    /* Set pack buffer size (same as 'Q' in GUM) */
	      if (rts_argv[arg][4] != '\0') {
		RtsFlags.GranFlags.packBufferSize = decode(rts_argv[arg]+4);
		fprintf(stderr,"Pack buffer size: %d\n",
			RtsFlags.GranFlags.packBufferSize);
	      } else {
    	    	fprintf(stderr, "setupRtsFlags: missing size of PackBuffer (for -Q)\n");
    	    	*error = rtsTrue;
    	      }
	      break;

	  default:
	      if (isdigit(rts_argv[arg][3])) {/* Set all debugging options in one */
	    	/* hack warning: interpret the flags as a binary number */
	    	nat n = decode(rts_argv[arg]+3);
		set_GranSim_debug_options(n);
	      } else {
		nat i;
		for (i=0; i<=MAX_GRAN_DEBUG_OPTION; i++) 
		  if (rts_argv[arg][3] == gran_debug_opts_flags[i])
		    break;
		
		if (i==MAX_GRAN_DEBUG_OPTION+1) {
		  fprintf(stderr, "Valid GranSim debug options are:\n");
		  help_GranSim_debug_options(MAX_GRAN_DEBUG_MASK);
		  bad_option( rts_argv[arg] );
		} else { // flag found; now set it
		  set_GranSim_debug_options(GRAN_DEBUG_MASK(i));  // 2^i
		}
	      }
	      break;
	      
#if 0
	    case 'e':       /* event trace; also -bD1 */
	      fprintf(stderr,"DEBUG: event_trace; printing event trace.\n");
	      RtsFlags.GranFlags.Debug.event_trace = rtsTrue;
	      /* RtsFlags.GranFlags.event_trace=rtsTrue; */
	      break;
	      
	    case 'E':       /* event statistics; also -bD2 */
	      fprintf(stderr,"DEBUG: event_stats; printing event statistics.\n");
	      RtsFlags.GranFlags.Debug.event_stats = rtsTrue;
	      /* RtsFlags.GranFlags.Debug |= 0x20; print event statistics   */
	      break;
	      
	    case 'f':       /* thunkStealing; also -bD4 */
	      fprintf(stderr,"DEBUG: thunkStealing; printing forwarding of FETCHNODES.\n");
	      RtsFlags.GranFlags.Debug.thunkStealing = rtsTrue;
	      /* RtsFlags.GranFlags.Debug |= 0x2;  print fwd messages */
	      break;

	    case 'z':       /* blockOnFetch; also -bD8 */
	      fprintf(stderr,"DEBUG: blockOnFetch; check for blocked on fetch.\n");
	      RtsFlags.GranFlags.Debug.blockOnFetch = rtsTrue;
	      /* RtsFlags.GranFlags.Debug |= 0x4; debug non-reschedule-on-fetch */
	      break;
	      
	    case 't':       /* blockOnFetch_sanity; also -bD16 */  
	      fprintf(stderr,"DEBUG: blockOnFetch_sanity; check for TSO asleep on fetch.\n");
	      RtsFlags.GranFlags.Debug.blockOnFetch_sanity = rtsTrue;
	      /* RtsFlags.GranFlags.Debug |= 0x10; debug TSO asleep for fetch  */
	      break;

	    case 'S':       /* priSpark; also -bD32 */
	      fprintf(stderr,"DEBUG: priSpark; priority sparking.\n");
	      RtsFlags.GranFlags.Debug.priSpark = rtsTrue;
	      break;

	    case 's':       /* priSched; also -bD64 */
	      fprintf(stderr,"DEBUG: priSched; priority scheduling.\n");
	      RtsFlags.GranFlags.Debug.priSched = rtsTrue;
	      break;

	    case 'F':       /* findWork; also -bD128 */
	      fprintf(stderr,"DEBUG: findWork; searching spark-pools (local & remote), thread queues for work.\n");
	      RtsFlags.GranFlags.Debug.findWork = rtsTrue;
	      break;
	      
	    case 'g':       /* globalBlock; also -bD256 */
	      fprintf(stderr,"DEBUG: globalBlock; blocking on remote closures (FETCHMEs etc in GUM).\n");
	      RtsFlags.GranFlags.Debug.globalBlock = rtsTrue;
	      break;
	      
	    case 'G':       /* pack; also -bD512 */
	      fprintf(stderr,"DEBUG: pack; routines for (un-)packing graph structures.\n");
	      RtsFlags.GranFlags.Debug.pack = rtsTrue;
	      break;
	      
	    case 'P':       /* packBuffer; also -bD1024 */
	      fprintf(stderr,"DEBUG: packBuffer; routines handling pack buffer (GranSim internal!).\n");
	      RtsFlags.GranFlags.Debug.packBuffer = rtsTrue;
	      break;
	      
	    case 'o':       /* sortedQ; also -bD2048 */
	      fprintf(stderr,"DEBUG: sortedQ; check whether spark/thread queues are sorted.\n");
	      RtsFlags.GranFlags.Debug.sortedQ = rtsTrue;
	      break;
	      
	    case 'r':       /* randomSteal; also -bD4096 */
	      fprintf(stderr,"DEBUG: randomSteal; stealing sparks/threads from random PEs.\n");
	      RtsFlags.GranFlags.Debug.randomSteal = rtsTrue;
	      break;
	      
	    case 'q':       /* checkSparkQ; also -bD8192 */
	      fprintf(stderr,"DEBUG: checkSparkQ; check consistency of the spark queues.\n");
	      RtsFlags.GranFlags.Debug.checkSparkQ = rtsTrue;
	      break;
	      
	    case ':':       /* checkLight; also -bD16384 */
	      fprintf(stderr,"DEBUG: checkLight; check GranSim-Light setup.\n");
	      RtsFlags.GranFlags.Debug.checkLight = rtsTrue;
	      break;
	      
	    case 'b':       /* bq; also -bD32768 */
	      fprintf(stderr,"DEBUG: bq; check blocking queues\n");
	      RtsFlags.GranFlags.Debug.bq = rtsTrue;
	      break;
	      
	    case 'd':       /* all options turned on */
	      fprintf(stderr,"DEBUG: all options turned on.\n");
	      set_GranSim_debug_options(MAX_GRAN_DEBUG_MASK);
	      /* RtsFlags.GranFlags.Debug |= 0x40; */
	      break;

/*  	    case '\0': */
/*  	      RtsFlags.GranFlags.Debug = 1; */
/*  	      break; */
#endif

	  }
	  break;
#  endif  /* GRAN_CHECK */
      default:
	bad_option( rts_argv[arg] );
	break;
      }
}

/*
  Interpret n as a binary number masking GranSim debug options and set the 
  correxponding option. See gran_debug_opts_strs for explanations of the flags.
*/
static void
set_GranSim_debug_options(nat n) {
  nat i;

  for (i=0; i<=MAX_GRAN_DEBUG_OPTION; i++) 
    if ((n>>i)&1) {
      fprintf(stderr, gran_debug_opts_strs[i]);
      switch (i) {
        case 0: RtsFlags.GranFlags.Debug.event_trace   = rtsTrue;  break;
        case 1: RtsFlags.GranFlags.Debug.event_stats   = rtsTrue;  break;
        case 2: RtsFlags.GranFlags.Debug.bq            = rtsTrue;  break;
        case 3: RtsFlags.GranFlags.Debug.pack          = rtsTrue;  break;
        case 4: RtsFlags.GranFlags.Debug.checkSparkQ   = rtsTrue;  break;
        case 5: RtsFlags.GranFlags.Debug.thunkStealing = rtsTrue;  break;
        case 6: RtsFlags.GranFlags.Debug.randomSteal   = rtsTrue;  break;
        case 7: RtsFlags.GranFlags.Debug.findWork      = rtsTrue;  break;
        case 8: RtsFlags.GranFlags.Debug.unused        = rtsTrue;  break;
        case 9: RtsFlags.GranFlags.Debug.pri           = rtsTrue;  break;
        case 10: RtsFlags.GranFlags.Debug.checkLight   = rtsTrue;  break;
        case 11: RtsFlags.GranFlags.Debug.sortedQ      = rtsTrue;  break;
        case 12: RtsFlags.GranFlags.Debug.blockOnFetch = rtsTrue;  break;
        case 13: RtsFlags.GranFlags.Debug.packBuffer   = rtsTrue;  break;
        case 14: RtsFlags.GranFlags.Debug.blockOnFetch_sanity = rtsTrue;  break;
        default: barf("set_GranSim_debug_options: only %d debug options expected");
      } /* switch */
    } /* if */
}

/*
  Print one line explanation for each of the GranSim debug options specified
  in the bitmask n.
*/
static void
help_GranSim_debug_options(nat n) {
  nat i;

  for (i=0; i<=MAX_GRAN_DEBUG_OPTION; i++) 
    if ((n>>i)&1) 
      fprintf(stderr, gran_debug_opts_strs[i]);
}

# elif defined(PAR)

static void
process_par_option(int arg, int *rts_argc, char *rts_argv[], rtsBool *error)
{

  if (rts_argv[arg][1] != 'q') { /* All GUM options start with -q */
    belch("Warning: GUM option does not start with -q: %s", rts_argv[arg]);
    return;
  }

  /* Communication and task creation cost parameters */
  switch(rts_argv[arg][2]) {
  case 'e':  /* -qe<n>  ... allow <n> local sparks */
    if (rts_argv[arg][3] != '\0') { /* otherwise, stick w/ the default */
      RtsFlags.ParFlags.maxLocalSparks
	= strtol(rts_argv[arg]+3, (char **) NULL, 10);
      
      if (RtsFlags.ParFlags.maxLocalSparks <= 0) {
	belch("setupRtsFlags: bad value for -e\n");
	*error = rtsTrue;
      }
    }
    IF_PAR_DEBUG(verbose,
		 belch("-qe<n>: max %d local sparks", 
		       RtsFlags.ParFlags.maxLocalSparks));
    break;
  
  case 't':
    if (rts_argv[arg][3] != '\0') {
      RtsFlags.ParFlags.maxThreads
	= strtol(rts_argv[arg]+3, (char **) NULL, 10);
    } else {
      belch("setupRtsFlags: missing size for -qt\n");
      *error = rtsTrue;
    }
    IF_PAR_DEBUG(verbose,
		 belch("-qt<n>: max %d threads", 
		       RtsFlags.ParFlags.maxThreads));
    break;

  case 'f':
    if (rts_argv[arg][3] != '\0')
      RtsFlags.ParFlags.maxFishes = decode(rts_argv[arg]+3);
    else
      RtsFlags.ParFlags.maxFishes = MAX_FISHES;
    break;
    IF_PAR_DEBUG(verbose,
		 belch("-qf<n>: max %d fishes sent out at one time", 
		       RtsFlags.ParFlags.maxFishes));
    break;
  
  case 'F':
    if (rts_argv[arg][3] != '\0') {
      RtsFlags.ParFlags.fishDelay
	= strtol(rts_argv[arg]+3, (char **) NULL, 10);
    } else {
      belch("setupRtsFlags: missing fish delay time for -qF\n");
      *error = rtsTrue;
    }
    IF_PAR_DEBUG(verbose,
		 belch("-qF<n>: fish delay time %d us", 
		       RtsFlags.ParFlags.fishDelay));
    break;

  case 'O':
    RtsFlags.ParFlags.outputDisabled = rtsTrue;
    IF_PAR_DEBUG(verbose,
		 belch("-qO: output disabled"));
    break;
  
  case 'g': /* -qg<n> ... globalisation scheme */
    if (rts_argv[arg][3] != '\0') {
      RtsFlags.ParFlags.globalising = decode(rts_argv[arg]+3);
    } else {
      belch("setupRtsFlags: missing identifier for globalisation scheme (for -qg)\n");
      *error = rtsTrue;
    }
    IF_PAR_DEBUG(verbose,
		 belch("-qg<n>: globalisation scheme set to  %d", 
		       RtsFlags.ParFlags.globalising));
    break;

  case 'h': /* -qh<n> ... max number of thunks (except root) in packet */
    if (rts_argv[arg][3] != '\0') {
      RtsFlags.ParFlags.thunksToPack = decode(rts_argv[arg]+3);
    } else {
      belch("setupRtsFlags: missing number of thunks per packet (for -qh)\n");
      *error = rtsTrue;
    }
    IF_PAR_DEBUG(verbose,
		 belch("-qh<n>: thunks per packet set to %d", 
		       RtsFlags.ParFlags.thunksToPack));
    break;

  case 'P': /* -qP for writing a log file */
    //RtsFlags.ParFlags.ParStats.Full = rtsFalse;
    /* same encoding as in GranSim after -bP */	
    switch(rts_argv[arg][3]) {
    case '\0': RtsFlags.ParFlags.ParStats.Full = rtsTrue;
      break; // nothing special, just an ordinary profile
    case '0': RtsFlags.ParFlags.ParStats.Suppressed = rtsTrue;
	RtsFlags.ParFlags.ParStats.Full = rtsFalse;
      break;
    case 'b': RtsFlags.ParFlags.ParStats.Binary = rtsTrue;
      break;
    case 's': RtsFlags.ParFlags.ParStats.Sparks = rtsTrue;
      break;
      //case 'h': RtsFlags.parFlags.ParStats.Heap = rtsTrue;
      //  break;
    case 'n': RtsFlags.ParFlags.ParStats.NewLogfile = rtsTrue;
      break;
    case 'g': 
# if defined(PAR_TICKY)
      RtsFlags.ParFlags.ParStats.Global = rtsTrue;
# else 
      fprintf(stderr,"-qPg is only possible for a PAR_TICKY RTS, which this is not");
      stg_exit(EXIT_FAILURE);
# endif
      break;
    default: barf("Unknown option -qP%c", rts_argv[arg][2]);
    }
    IF_PAR_DEBUG(verbose,
		 belch("(-qP) writing to log-file (RtsFlags.ParFlags.ParStats.Full=%s)",
		       (RtsFlags.ParFlags.ParStats.Full ? "rtsTrue" : "rtsFalse")));
    break;
  
  case 'Q': /* -qQ<n> ... set pack buffer size to <n> */
    if (rts_argv[arg][3] != '\0') {
      RtsFlags.ParFlags.packBufferSize = decode(rts_argv[arg]+3);
    } else {
      belch("setupRtsFlags: missing size of PackBuffer (for -qQ)\n");
      *error = rtsTrue;
    }
    IF_PAR_DEBUG(verbose,
		 belch("-qQ<n>: pack buffer size set to %d", 
		       RtsFlags.ParFlags.packBufferSize));
    break;

  case 'R':
    RtsFlags.ParFlags.doFairScheduling = rtsTrue;
    IF_PAR_DEBUG(verbose,
		 belch("-qR: fair-ish scheduling"));
    break;
  
# if defined(DEBUG)  
  case 'w':
    if (rts_argv[arg][3] != '\0') {
      RtsFlags.ParFlags.wait
	= strtol(rts_argv[arg]+3, (char **) NULL, 10);
    } else {
      RtsFlags.ParFlags.wait = 1000;
    }
    IF_PAR_DEBUG(verbose,
		 belch("-qw<n>: length of wait loop after synchr before reduction: %d", 
		       RtsFlags.ParFlags.wait));
    break;

  case 'D':  /* -qD ... all the debugging options */
    if (isdigit(rts_argv[arg][3])) {/* Set all debugging options in one */
      /* hack warning: interpret the flags as a binary number */
      nat n = decode(rts_argv[arg]+3);
      set_par_debug_options(n);
    } else {
      nat i;
      for (i=0; i<=MAX_PAR_DEBUG_OPTION; i++) 
	if (rts_argv[arg][3] == par_debug_opts_flags[i])
	  break;
	
      if (i==MAX_PAR_DEBUG_OPTION+1) {
	fprintf(stderr, "Valid GUM debug options are:\n");
	help_par_debug_options(MAX_PAR_DEBUG_MASK);
	bad_option( rts_argv[arg] );
      } else { // flag found; now set it
	set_par_debug_options(PAR_DEBUG_MASK(i));  // 2^i
      }
    }
    break;
# endif
  default:
    belch("Unknown option -q%c (%d opts in total)", 
	  rts_argv[arg][2], *rts_argc);
    break;
  } /* switch */
}

/*
  Interpret n as a binary number masking Par debug options and set the 
  correxponding option. See par_debug_opts_strs for explanations of the flags.
*/
static void
set_par_debug_options(nat n) {
  nat i;

  for (i=0; i<=MAX_PAR_DEBUG_OPTION; i++) 
    if ((n>>i)&1) {
      fprintf(stderr, par_debug_opts_strs[i]);
      switch (i) {
        case 0: RtsFlags.ParFlags.Debug.verbose       = rtsTrue;  break;
        case 1: RtsFlags.ParFlags.Debug.bq            = rtsTrue;  break;
        case 2: RtsFlags.ParFlags.Debug.schedule      = rtsTrue;  break;
        case 3: RtsFlags.ParFlags.Debug.free          = rtsTrue;  break;
        case 4: RtsFlags.ParFlags.Debug.resume        = rtsTrue;  break;
        case 5: RtsFlags.ParFlags.Debug.weight        = rtsTrue;  break;
        case 6: RtsFlags.ParFlags.Debug.fetch         = rtsTrue;  break;
	  //case 7: RtsFlags.ParFlags.Debug.ack           = rtsTrue;  break;
        case 7: RtsFlags.ParFlags.Debug.fish          = rtsTrue;  break;
        case 8: RtsFlags.ParFlags.Debug.tables        = rtsTrue;  break;
        case 9: RtsFlags.ParFlags.Debug.packet        = rtsTrue;  break;
        case 10: RtsFlags.ParFlags.Debug.pack         = rtsTrue;  break;
        case 11: RtsFlags.ParFlags.Debug.paranoia     = rtsTrue;  break;
        default: barf("set_par_debug_options: only %d debug options expected",
		      MAX_PAR_DEBUG_OPTION);
      } /* switch */
    } /* if */
}

/*
  Print one line explanation for each of the GranSim debug options specified
  in the bitmask n.
*/
static void
help_par_debug_options(nat n) {
  nat i;

  for (i=0; i<=MAX_PAR_DEBUG_OPTION; i++) 
    if ((n>>i)&1) 
      fprintf(stderr, par_debug_opts_strs[i]);
}

#endif /* PAR */

//@node Aux fcts,  , GranSim specific options
//@subsection Aux fcts

static FILE *		/* return NULL on error */
open_stats_file (
    I_ arg,
    int argc, char *argv[],
    int rts_argc, char *rts_argv[],
    const char *FILENAME_FMT)
{
    FILE *f = NULL;

    if (strequal(rts_argv[arg]+2, "stderr")) /* use real stderr */
	f = stderr;
    else if (rts_argv[arg][2] != '\0')	    /* stats file specified */
	f = fopen(rts_argv[arg]+2,"w");
    else {
	char stats_filename[STATS_FILENAME_MAXLEN]; /* default <program>.<ext> */
	sprintf(stats_filename, FILENAME_FMT, argv[0]);
	f = fopen(stats_filename,"w");
    }
    if (f == NULL) {
	fprintf(stderr, "Can't open stats file %s\n", rts_argv[arg]+2);
    } else {
	/* Write argv and rtsv into start of stats file */
	I_ count;
	for(count = 0; count < argc; count++)
	    fprintf(f, "%s ", argv[count]);
	fprintf(f, "+RTS ");
	for(count = 0; count < rts_argc; count++)
	    fprintf(f, "%s ", rts_argv[count]);
	fprintf(f, "\n");
    }

    return(f);
}

static I_
decode(const char *s)
{
    I_ c;
    StgDouble m;

    if (!*s)
	return 0;

    m = atof(s);
    c = s[strlen(s)-1];

    if (c == 'g' || c == 'G')
	m *= 1000*1000*1000;	/* UNchecked! */
    else if (c == 'm' || c == 'M')
	m *= 1000*1000;			/* We do not use powers of 2 (1024) */
    else if (c == 'k' || c == 'K')	/* to avoid possible bad effects on */
	m *= 1000;			/* a direct-mapped cache.   	    */ 
    else if (c == 'w' || c == 'W')
	m *= sizeof(W_);

    return (I_)m;
}

static void
bad_option(const char *s)
{
  prog_belch("bad RTS option: %s", s);
  stg_exit(EXIT_FAILURE);
}

#endif /* !STANDALONE || DEBUG */
