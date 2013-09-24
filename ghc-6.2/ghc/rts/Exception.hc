/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The GHC Team, 1998-2000
 *
 * Exception support
 *
 * ---------------------------------------------------------------------------*/

#include "Stg.h"
#include "Rts.h"
#include "Exception.h"
#include "Schedule.h"
#include "StgRun.h"
#include "Storage.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#if defined(PAR)
# include "FetchMe.h"
#endif
#if defined(PROFILING)
# include "Profiling.h"
#endif

/* -----------------------------------------------------------------------------
   Exception Primitives

   A thread can request that asynchronous exceptions not be delivered
   ("blocked") for the duration of an I/O computation.  The primitive
   
	blockAsyncExceptions# :: IO a -> IO a

   is used for this purpose.  During a blocked section, asynchronous
   exceptions may be unblocked again temporarily:

	unblockAsyncExceptions# :: IO a -> IO a

   Furthermore, asynchronous exceptions are blocked automatically during
   the execution of an exception handler.  Both of these primitives
   leave a continuation on the stack which reverts to the previous
   state (blocked or unblocked) on exit.

   A thread which wants to raise an exception in another thread (using
   killThread#) must block until the target thread is ready to receive
   it.  The action of unblocking exceptions in a thread will release all
   the threads waiting to deliver exceptions to that thread.

   -------------------------------------------------------------------------- */

FN_(blockAsyncExceptionszh_fast)
{
  FB_
    /* Args: R1 :: IO a */
    STK_CHK_GEN( 2/* worst case */, R1_PTR, blockAsyncExceptionszh_fast);

    if (CurrentTSO->blocked_exceptions == NULL) {
      CurrentTSO->blocked_exceptions = END_TSO_QUEUE;
      /* avoid growing the stack unnecessarily */
      if (Sp[0] == (W_)&stg_blockAsyncExceptionszh_ret_info) {
	Sp++;
      } else {
	Sp--;
	Sp[0] = (W_)&stg_unblockAsyncExceptionszh_ret_info;
      }
    }
    Sp--;
    JMP_(stg_ap_v_ret);
  FE_
}

INFO_TABLE_RET( \
  stg_unblockAsyncExceptionszh_ret_info, \
  stg_unblockAsyncExceptionszh_ret_entry, \
  MK_SMALL_BITMAP(0/*framesize*/, 0/*bitmap*/), \
  0, 0, 0, RET_SMALL, , EF_, 0, 0 \
);

FN_(stg_unblockAsyncExceptionszh_ret_entry)
{
  FB_
    ASSERT(CurrentTSO->blocked_exceptions != NULL);
#if defined(GRAN)
      awakenBlockedQueue(CurrentTSO->blocked_exceptions, 
	                 (StgClosure*)NULL); 
#elif defined(PAR)
      /* we don't need node info (2nd arg) in this case
	 (note that CurrentTSO->block_info.closure isn't always set) */
      awakenBlockedQueue(CurrentTSO->blocked_exceptions, 
	                 (StgClosure*)NULL); 
#else
    awakenBlockedQueue(CurrentTSO->blocked_exceptions);
#endif
    CurrentTSO->blocked_exceptions = NULL;
#ifdef REG_R1
    Sp++;
    JMP_(ENTRY_CODE(Sp[0]));
#else
    Sp[1] = Sp[0];
    Sp++;
    JMP_(ENTRY_CODE(Sp[1]));
#endif
  FE_
}

FN_(unblockAsyncExceptionszh_fast)
{
  FB_
    /* Args: R1 :: IO a */
    STK_CHK_GEN(2, R1_PTR, unblockAsyncExceptionszh_fast);

    if (CurrentTSO->blocked_exceptions != NULL) {
#if defined(GRAN)
      awakenBlockedQueue(CurrentTSO->blocked_exceptions, 
	                 CurrentTSO->block_info.closure);
#elif defined(PAR)
      // is CurrentTSO->block_info.closure always set to the node
      // holding the blocking queue !? -- HWL
      awakenBlockedQueue(CurrentTSO->blocked_exceptions, 
	                 CurrentTSO->block_info.closure);
#else
      awakenBlockedQueue(CurrentTSO->blocked_exceptions);
#endif
      CurrentTSO->blocked_exceptions = NULL;

      /* avoid growing the stack unnecessarily */
      if (Sp[0] == (W_)&stg_unblockAsyncExceptionszh_ret_info) {
	Sp++;
      } else {
	Sp--;	
	Sp[0] = (W_)&stg_blockAsyncExceptionszh_ret_info;
      }
    }
    Sp--;
    JMP_(stg_ap_v_ret);
  FE_
}

INFO_TABLE_RET( \
  stg_blockAsyncExceptionszh_ret_info, \
  stg_blockAsyncExceptionszh_ret_entry, \
  MK_SMALL_BITMAP(0/*framesize*/, 0/*bitmap*/), \
  0, 0, 0, RET_SMALL, , EF_, 0, 0 \
);

FN_(stg_blockAsyncExceptionszh_ret_entry)
{
  FB_
    ASSERT(CurrentTSO->blocked_exceptions == NULL);
    CurrentTSO->blocked_exceptions = END_TSO_QUEUE;
#ifdef REG_R1
    Sp++;
    JMP_(ENTRY_CODE(Sp[0]));
#else
    Sp[1] = Sp[0];
    Sp++;
    JMP_(ENTRY_CODE(Sp[1]));
#endif
  FE_
}

FN_(killThreadzh_fast)
{
  FB_
  /* args: R1.p = TSO to kill, R2.p = Exception */

  /* This thread may have been relocated.
   * (see Schedule.c:threadStackOverflow)
   */
  while (R1.t->what_next == ThreadRelocated) {
    R1.t = R1.t->link;
  }

  /* If the target thread is currently blocking async exceptions,
   * we'll have to block until it's ready to accept them.  The
   * exception is interruptible threads - ie. those that are blocked
   * on some resource.
   */
  if (R1.t->blocked_exceptions != NULL && !interruptible(R1.t) ) {
    
    /* ToDo (SMP): locking if destination thread is currently
     * running...
     */
    CurrentTSO->link = R1.t->blocked_exceptions;
    R1.t->blocked_exceptions = CurrentTSO;

    CurrentTSO->why_blocked = BlockedOnException;
    CurrentTSO->block_info.tso = R1.t;
    
    BLOCK( R1_PTR | R2_PTR, killThreadzh_fast );
  }

  /* Killed threads turn into zombies, which might be garbage
   * collected at a later date.  That's why we don't have to
   * explicitly remove them from any queues they might be on.
   */

  /* We might have killed ourselves.  In which case, better be *very*
   * careful.  If the exception killed us, then return to the scheduler.
   * If the exception went to a catch frame, we'll just continue from
   * the handler.
   */
  if (R1.t == CurrentTSO) {
	SaveThreadState();	/* inline! */
	STGCALL2(raiseAsyncWithLock, R1.t, R2.cl);
	if (CurrentTSO->what_next == ThreadKilled) {
		R1.w = ThreadFinished;
		JMP_(StgReturn);
	} else {
		LoadThreadState();
		ASSERT(CurrentTSO->what_next == ThreadRunGHC);
		JMP_(ENTRY_CODE(Sp[0]));
	}
  } else {
	STGCALL2(raiseAsyncWithLock, R1.t, R2.cl);
  }

  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}


/* -----------------------------------------------------------------------------
   Catch frames
   -------------------------------------------------------------------------- */

#ifdef REG_R1
#define CATCH_FRAME_ENTRY_TEMPLATE(label,ret) 	\
   FN_(label);					\
   FN_(label)					\
   {						\
      FB_					\
      Sp += sizeofW(StgCatchFrame);		\
      JMP_(ret);				\
      FE_					\
   }
#else
#define CATCH_FRAME_ENTRY_TEMPLATE(label,ret) 	\
   FN_(label);					\
   FN_(label)					\
   {						\
      StgWord rval;				\
      FB_					\
      rval = Sp[0];				\
      Sp++;					\
      Sp += sizeofW(StgCatchFrame) - 1;		\
      Sp[0] = rval;				\
      JMP_(ret);				\
      FE_					\
   }
#endif

#ifdef REG_R1
#define SP_OFF 0
#else
#define SP_OFF 1
#endif

CATCH_FRAME_ENTRY_TEMPLATE(stg_catch_frame_ret,ENTRY_CODE(Sp[SP_OFF]));
CATCH_FRAME_ENTRY_TEMPLATE(stg_catch_frame_0_ret,RET_VEC(Sp[SP_OFF],0));
CATCH_FRAME_ENTRY_TEMPLATE(stg_catch_frame_1_ret,RET_VEC(Sp[SP_OFF],1));
CATCH_FRAME_ENTRY_TEMPLATE(stg_catch_frame_2_ret,RET_VEC(Sp[SP_OFF],2));
CATCH_FRAME_ENTRY_TEMPLATE(stg_catch_frame_3_ret,RET_VEC(Sp[SP_OFF],3));
CATCH_FRAME_ENTRY_TEMPLATE(stg_catch_frame_4_ret,RET_VEC(Sp[SP_OFF],4));
CATCH_FRAME_ENTRY_TEMPLATE(stg_catch_frame_5_ret,RET_VEC(Sp[SP_OFF],5));
CATCH_FRAME_ENTRY_TEMPLATE(stg_catch_frame_6_ret,RET_VEC(Sp[SP_OFF],6));
CATCH_FRAME_ENTRY_TEMPLATE(stg_catch_frame_7_ret,RET_VEC(Sp[SP_OFF],7));

#if defined(PROFILING)
#define CATCH_FRAME_BITMAP 7
#define CATCH_FRAME_WORDS  4
#else
#define CATCH_FRAME_BITMAP 1
#define CATCH_FRAME_WORDS  2
#endif

/* Catch frames are very similar to update frames, but when entering
 * one we just pop the frame off the stack and perform the correct
 * kind of return to the activation record underneath us on the stack.
 */

VEC_POLY_INFO_TABLE(stg_catch_frame, \
	MK_SMALL_BITMAP(CATCH_FRAME_WORDS, CATCH_FRAME_BITMAP), \
	NULL/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/, CATCH_FRAME,, EF_);

/* -----------------------------------------------------------------------------
 * The catch infotable
 *
 * This should be exactly the same as would be generated by this STG code
 *
 * catch = {x,h} \n {} -> catch#{x,h}
 *
 * It is used in deleteThread when reverting blackholes.
 * -------------------------------------------------------------------------- */

INFO_TABLE(stg_catch_info,stg_catch_entry,2,0,FUN,,EF_,0,0);
STGFUN(stg_catch_entry)
{
  FB_
  R2.cl = R1.cl->payload[1]; /* h */
  R1.cl = R1.cl->payload[0]; /* x */
  JMP_(catchzh_fast);
  FE_
}

FN_(catchzh_fast)
{
  StgCatchFrame *fp;
  FB_

    /* args: R1 = m :: IO a, R2 = handler :: Exception -> IO a */
    STK_CHK_GEN(sizeofW(StgCatchFrame) + 1, R1_PTR | R2_PTR, catchzh_fast);
  
    /* Set up the catch frame */
    Sp -= sizeofW(StgCatchFrame);
    fp = (StgCatchFrame *)Sp;
    SET_HDR(fp,(StgInfoTable *)&stg_catch_frame_info,CCCS);
    fp -> handler = R2.cl;
    fp -> exceptions_blocked = (CurrentTSO->blocked_exceptions != NULL);
    TICK_CATCHF_PUSHED();


/* Apply R1 to the realworld token */
    Sp--;
    JMP_(stg_ap_v_ret);
  FE_
}      

/* -----------------------------------------------------------------------------
 * The raise infotable
 * 
 * This should be exactly the same as would be generated by this STG code
 *
 *   raise = {err} \n {} -> raise#{err}
 *
 * It is used in raisezh_fast to update thunks on the update list
 * -------------------------------------------------------------------------- */

INFO_TABLE(stg_raise_info,stg_raise_entry,1,0,THUNK,,EF_,0,0);
STGFUN(stg_raise_entry)
{
  FB_
  R1.cl = R1.cl->payload[0];
  JMP_(raisezh_fast);
  FE_
}

FN_(raisezh_fast)
{
  StgClosure *handler;
  StgPtr p;
  StgClosure *raise_closure;
  FB_
    /* args : R1.p :: Exception */


#if defined(PROFILING)
    /* Debugging tool: on raising an  exception, show where we are. */

    /* ToDo: currently this is a hack.  Would be much better if
     * the info was only displayed for an *uncaught* exception.
     */
    if (RtsFlags.ProfFlags.showCCSOnException) {
      STGCALL2(fprintCCS,stderr,CCCS);
    }
#endif

    /* This closure represents the expression 'raise# E' where E
     * is the exception raise.  It is used to overwrite all the
     * thunks which are currently under evaluataion.
     */
    /*    
    // @LDV profiling
    // stg_raise_info has THUNK as its closure type. Since a THUNK takes at least
    // MIN_UPD_SIZE words in its payload, MIN_UPD_SIZE is more approprate than 1.
    // It seems that 1 does not cause any problem unless profiling is performed.
    // However, when LDV profiling goes on, we need to linearly scan small object pool,
    // where raise_closure is stored, so we should use MIN_UPD_SIZE.
    raise_closure = (StgClosure *)RET_STGCALL1(P_,allocate,
					       sizeofW(StgClosure)+1);
     */
    raise_closure = (StgClosure *)RET_STGCALL1(P_,allocate,
					       sizeofW(StgClosure)+MIN_UPD_SIZE);
    SET_HDR(raise_closure, &stg_raise_info, CCCS);
    raise_closure->payload[0] = R1.cl;

    // Walk up the stack, looking for the catch frame.  On the way,
    // we update any closures pointed to from update frames with the
    // raise closure that we just built.
    {		
	StgPtr next;
	StgRetInfoTable *info;

	p = Sp;
	while(1) {

	    info = get_ret_itbl((StgClosure *)p);
	    next = p + stack_frame_sizeW((StgClosure *)p);
	    switch (info->i.type) {

	    case UPDATE_FRAME:
		UPD_IND(((StgUpdateFrame *)p)->updatee,raise_closure);
		p = next;
		continue;

	    case CATCH_FRAME:
		/* found it! */
		break;

	    case STOP_FRAME:
		/* We've stripped the entire stack, the thread is now dead. */
		Sp = CurrentTSO->stack + CurrentTSO->stack_size - 1;
		Sp[0] = R1.w;		/* save the exception */
		CurrentTSO->what_next = ThreadKilled;
		SaveThreadState();	/* inline! */
		R1.w = ThreadFinished;
		JMP_(StgReturn);
		
	    default:
		p = next; 
		continue;
	    }
      
	    break;
	}
    }
    
    /* Ok, p points to the enclosing CATCH_FRAME.  Pop everything down to
     * and including this frame, update Su, push R1, and enter the handler.
     */
    handler = ((StgCatchFrame *)p)->handler;
    
    Sp = (P_)p + sizeofW(StgCatchFrame);

    /* Restore the blocked/unblocked state for asynchronous exceptions
     * at the CATCH_FRAME.  
     *
     * If exceptions were unblocked, arrange that they are unblocked
     * again after executing the handler by pushing an
     * unblockAsyncExceptions_ret stack frame.
     */
    if (! ((StgCatchFrame *)p)->exceptions_blocked) {
      *(--Sp) = (W_)&stg_unblockAsyncExceptionszh_ret_info;
    }

    /* Ensure that async excpetions are blocked when running the handler.
    */
    if (CurrentTSO->blocked_exceptions == NULL) {
      CurrentTSO->blocked_exceptions = END_TSO_QUEUE;
    }

    /* Call the handler, passing the exception value and a realworld
     * token as arguments.
     */
    Sp -= 2;
    Sp[1] = (W_)&stg_ap_v_info;
    Sp[0] = R1.w;
    R1.cl = handler;
    Sp--;
    JMP_(stg_ap_p_ret);
  FE_
}

FN_(raiseIOzh_fast)
{
  FB_
  /* Args :: R1.p :: Exception */
  JMP_(raisezh_fast);
  FE_
}
