/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The GHC Team, 1998-2002
 *
 * Entry code for various built-in closure types.
 *
 * ---------------------------------------------------------------------------*/

#include "Stg.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "StgMiscClosures.h"
#include "Storage.h"
#include "StoragePriv.h"
#include "Profiling.h"
#include "Prelude.h"
#include "Schedule.h"
#include "SMP.h"
#if defined(GRAN) || defined(PAR)
# include "GranSimRts.h"      /* for DumpRawGranEvent */
# include "StgRun.h"	/* for StgReturn and register saving */
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* ToDo: make the printing of panics more win32-friendly, i.e.,
 *       pop up some lovely message boxes (as well).
 */
#define DUMP_ERRMSG(msg) STGCALL2(fprintf,stderr,msg)

/*
  Template for the entry code of non-enterable closures.
*/

#define NON_ENTERABLE_ENTRY_CODE(type)		\
IF_(stg_##type##_entry)			\
{						\
  FB_						\
    STGCALL1(barf, #type " object entered!");	\
  FE_						\
}


/* -----------------------------------------------------------------------------
   Support for the bytecode interpreter.
   -------------------------------------------------------------------------- */

/* 9 bits of return code for constructors created by the interpreter. */
FN_(stg_interp_constr_entry) 
{ 
  /* R1 points at the constructor */
  FB_ 
    /* STGCALL2(fprintf,stderr,"stg_interp_constr_entry (direct return)!\n"); */
    /* Pointless, since SET_TAG doesn't do anything */
    SET_TAG( GET_TAG(GET_INFO(R1.cl))); 
    JMP_(ENTRY_CODE((P_)(*Sp))); 
  FE_ 
}

FN_(stg_interp_constr1_entry) { FB_ JMP_(RET_VEC((P_)(*Sp),0)); FE_ }
FN_(stg_interp_constr2_entry) { FB_ JMP_(RET_VEC((P_)(*Sp),1)); FE_ }
FN_(stg_interp_constr3_entry) { FB_ JMP_(RET_VEC((P_)(*Sp),2)); FE_ }
FN_(stg_interp_constr4_entry) { FB_ JMP_(RET_VEC((P_)(*Sp),3)); FE_ }
FN_(stg_interp_constr5_entry) { FB_ JMP_(RET_VEC((P_)(*Sp),4)); FE_ }
FN_(stg_interp_constr6_entry) { FB_ JMP_(RET_VEC((P_)(*Sp),5)); FE_ }
FN_(stg_interp_constr7_entry) { FB_ JMP_(RET_VEC((P_)(*Sp),6)); FE_ }
FN_(stg_interp_constr8_entry) { FB_ JMP_(RET_VEC((P_)(*Sp),7)); FE_ }
 
/* Some info tables to be used when compiled code returns a value to
   the interpreter, i.e. the interpreter pushes one of these onto the
   stack before entering a value.  What the code does is to
   impedance-match the compiled return convention (in R1p/R1n/F1/D1 etc) to
   the interpreter's convention (returned value is on top of stack),
   and then cause the scheduler to enter the interpreter.

   On entry, the stack (growing down) looks like this:

      ptr to BCO holding return continuation
      ptr to one of these info tables.
 
   The info table code, both direct and vectored, must:
      * push R1/F1/D1 on the stack, and its tag if necessary
      * push the BCO (so it's now on the stack twice)
      * Yield, ie, go to the scheduler.

   Scheduler examines the t.o.s, discovers it is a BCO, and proceeds
   directly to the bytecode interpreter.  That pops the top element
   (the BCO, containing the return continuation), and interprets it.
   Net result: return continuation gets interpreted, with the
   following stack:

      ptr to this BCO
      ptr to the info table just jumped thru
      return value

   which is just what we want -- the "standard" return layout for the
   interpreter.  Hurrah!

   Don't ask me how unboxed tuple returns are supposed to work.  We
   haven't got a good story about that yet.
*/

// When the returned value is a pointer in R1...
#define STG_CtoI_RET_R1p_Template(label)	\
   IF_(label)					\
   {						\
      FB_					\
      Sp -= 2;					\
      Sp[1] = R1.w;				\
      Sp[0] = (W_)&stg_enter_info;		\
      JMP_(stg_yield_to_interpreter);		\
      FE_					\
   }

STG_CtoI_RET_R1p_Template(stg_ctoi_ret_R1p_ret);
STG_CtoI_RET_R1p_Template(stg_ctoi_ret_R1p_0_ret);
STG_CtoI_RET_R1p_Template(stg_ctoi_ret_R1p_1_ret);
STG_CtoI_RET_R1p_Template(stg_ctoi_ret_R1p_2_ret);
STG_CtoI_RET_R1p_Template(stg_ctoi_ret_R1p_3_ret);
STG_CtoI_RET_R1p_Template(stg_ctoi_ret_R1p_4_ret);
STG_CtoI_RET_R1p_Template(stg_ctoi_ret_R1p_5_ret);
STG_CtoI_RET_R1p_Template(stg_ctoi_ret_R1p_6_ret);
STG_CtoI_RET_R1p_Template(stg_ctoi_ret_R1p_7_ret);

VEC_POLY_INFO_TABLE( stg_ctoi_ret_R1p, 0/* special layout */,
		     0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/, 
		     RET_BCO,, EF_);

// When the returned value is a pointer, but unlifted, in R1 ...
INFO_TABLE_RET( stg_ctoi_ret_R1unpt_info, stg_ctoi_ret_R1unpt_entry,
		0/* special layout */,
		0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/, RET_BCO,, IF_, 0, 0);
IF_(stg_ctoi_ret_R1unpt_entry)
{
   FB_
   Sp -= 2;
   Sp[1] = R1.w;
   Sp[0] = (W_)&stg_gc_unpt_r1_info;
   JMP_(stg_yield_to_interpreter);
   FE_
}

// When the returned value is a non-pointer in R1 ...
INFO_TABLE_RET( stg_ctoi_ret_R1n_info, stg_ctoi_ret_R1n_entry,
		0/* special layout */,
		0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/, RET_BCO,, IF_, 0, 0);
IF_(stg_ctoi_ret_R1n_entry)
{
   FB_
   Sp -= 2;
   Sp[1] = R1.w;
   Sp[0] = (W_)&stg_gc_unbx_r1_info;
   JMP_(stg_yield_to_interpreter);
   FE_
}


// When the returned value is in F1 ...
INFO_TABLE_RET( stg_ctoi_ret_F1_info, stg_ctoi_ret_F1_entry, 
		0/* special layout */,
		0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/, RET_BCO,, IF_, 0, 0);
IF_(stg_ctoi_ret_F1_entry)
{
   FB_
   Sp -= 2;
   ASSIGN_FLT(Sp+1, F1);
   Sp[0] = (W_)&stg_gc_f1_info;
   JMP_(stg_yield_to_interpreter);
   FE_
}

// When the returned value is in D1 ...
INFO_TABLE_RET( stg_ctoi_ret_D1_info, stg_ctoi_ret_D1_entry,
		0/* special layout */,
		0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/, RET_BCO,, IF_, 0, 0);
IF_(stg_ctoi_ret_D1_entry)
{
   FB_
   Sp -= 1 + sizeofW(StgDouble);
   ASSIGN_DBL(Sp+1, D1);
   Sp[0] = (W_)&stg_gc_d1_info;
   JMP_(stg_yield_to_interpreter);
   FE_
}

// When the returned value is in L1 ...
INFO_TABLE_RET( stg_ctoi_ret_L1_info, stg_ctoi_ret_L1_entry,
		0/* special layout */,
		0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/, RET_BCO,, IF_, 0, 0);
IF_(stg_ctoi_ret_L1_entry)
{
   FB_
   Sp -= 1 + sizeofW(StgInt64);
   ASSIGN_Word64(Sp+1, L1);
   Sp[0] = (W_)&stg_gc_l1_info;
   JMP_(stg_yield_to_interpreter);
   FE_
}

// When the returned value a VoidRep ...
INFO_TABLE_RET( stg_ctoi_ret_V_info, stg_ctoi_ret_V_entry,
		0/* special layout */,
		0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/, RET_BCO,, IF_, 0, 0);
IF_(stg_ctoi_ret_V_entry)
{
   FB_
   Sp--;
   Sp[0] = (W_)&stg_gc_void_info;
   JMP_(stg_yield_to_interpreter);
   FE_
}

// Dummy info table pushed on the top of the stack when the interpreter
// should apply the BCO on the stack to its arguments, also on the stack.
INFO_TABLE_RET( stg_apply_interp_info, stg_apply_interp_entry,
		0/* special layout */,
		0/*srt*/, 0/*srt_off*/, 0/*srt_bitmap*/, RET_BCO,, IF_, 0, 0);
IF_(stg_apply_interp_entry)
{
    FB_
    // Just in case we end up in here... (we shouldn't)
    JMP_(stg_yield_to_interpreter);
    FE_
}

/* -----------------------------------------------------------------------------
   Entry code for a BCO
   -------------------------------------------------------------------------- */

INFO_TABLE_FUN_GEN(stg_BCO_info,stg_BCO_entry,4,0,
	      0,0,0,  /* no SRT */
	      ARG_BCO, 0/*dummy arity*/, 0/*dummy bitmap*/, NULL/*slow_apply*/,
	      BCO,,EF_,"BCO","BCO");
FN_(stg_BCO_entry) {
  FB_
  // entering a BCO means "apply it", same as a function
  Sp -= 2;
  Sp[1] = R1.w;
  Sp[0] = (W_)&stg_apply_interp_info;
  JMP_(stg_yield_to_interpreter);
  FE_
}

/* -----------------------------------------------------------------------------
   Info tables for indirections.

   SPECIALISED INDIRECTIONS: we have a specialised indirection for each
   kind of return (direct, vectored 0-7), so that we can avoid entering
   the object when we know what kind of return it will do.  The update
   code (Updates.hc) updates objects with the appropriate kind of
   indirection.  We only do this for young-gen indirections.
   -------------------------------------------------------------------------- */

INFO_TABLE(stg_IND_info,stg_IND_entry,1,0,IND,,IF_,"IND","IND");
IF_(stg_IND_entry)
{
    FB_
    TICK_ENT_DYN_IND(Node);	/* tick */
    R1.p = (P_) ((StgInd*)R1.p)->indirectee;
    TICK_ENT_VIA_NODE();
    JMP_(GET_ENTRY(R1.cl));
    FE_
}

#define IND_SPEC(n,ret) \
INFO_TABLE(stg_IND_##n##_info,stg_IND_##n##_entry,1,0,IND,,IF_,"IND","IND"); \
IF_(stg_IND_##n##_entry)			\
{						\
    FB_						\
    TICK_ENT_DYN_IND(Node);	/* tick */	\
    R1.p = (P_) ((StgInd*)R1.p)->indirectee;	\
    TICK_ENT_VIA_NODE();			\
    JMP_(ret);					\
    FE_						\
}

IND_SPEC(direct, ENTRY_CODE(Sp[0]))
IND_SPEC(0, RET_VEC(Sp[0],0))
IND_SPEC(1, RET_VEC(Sp[0],1))
IND_SPEC(2, RET_VEC(Sp[0],2))
IND_SPEC(3, RET_VEC(Sp[0],3))
IND_SPEC(4, RET_VEC(Sp[0],4))
IND_SPEC(5, RET_VEC(Sp[0],5))
IND_SPEC(6, RET_VEC(Sp[0],6))
IND_SPEC(7, RET_VEC(Sp[0],7))

INFO_TABLE(stg_IND_STATIC_info,stg_IND_STATIC_entry,1,0,IND_STATIC,,IF_,"IND_STATIC","IND_STATIC");
IF_(stg_IND_STATIC_entry)
{
    FB_
    TICK_ENT_STATIC_IND(Node);	/* tick */
    R1.p = (P_) ((StgIndStatic*)R1.p)->indirectee;
    TICK_ENT_VIA_NODE();
    JMP_(GET_ENTRY(R1.cl));
    FE_
}

INFO_TABLE(stg_IND_PERM_info,stg_IND_PERM_entry,1,1,IND_PERM,,IF_,"IND_PERM","IND_PERM");
IF_(stg_IND_PERM_entry)
{
    FB_
    /* Don't add INDs to granularity cost */
    /* Dont: TICK_ENT_STATIC_IND(Node); for ticky-ticky; this ind is here only to help profiling */

#if defined(TICKY_TICKY) && !defined(PROFILING)
    /* TICKY_TICKY && !PROFILING means PERM_IND *replaces* an IND, rather than being extra  */
    TICK_ENT_PERM_IND(R1.p); /* tick */
#endif

    LDV_ENTER((StgInd *)R1.p);

    /* Enter PAP cost centre -- lexical scoping only */
    ENTER_CCS_PAP_CL(R1.cl);

    /* For ticky-ticky, change the perm_ind to a normal ind on first
     * entry, so the number of ent_perm_inds is the number of *thunks*
     * entered again, not the number of subsequent entries.
     *
     * Since this screws up cost centres, we die if profiling and
     * ticky_ticky are on at the same time.  KSW 1999-01.
     */

#ifdef TICKY_TICKY
#  ifdef PROFILING
#    error Profiling and ticky-ticky do not mix at present!
#  endif  /* PROFILING */
    SET_INFO((StgInd*)R1.p,&stg_IND_info);
#endif /* TICKY_TICKY */

    R1.p = (P_) ((StgInd*)R1.p)->indirectee;

    /* Dont: TICK_ENT_VIA_NODE(); for ticky-ticky; as above */

#if defined(TICKY_TICKY) && !defined(PROFILING)
    TICK_ENT_VIA_NODE();
#endif

    JMP_(GET_ENTRY(R1.cl));
    FE_
}  

INFO_TABLE(stg_IND_OLDGEN_info,stg_IND_OLDGEN_entry,1,1,IND_OLDGEN,,IF_,"IND_OLDGEN","IND_OLDGEN");
IF_(stg_IND_OLDGEN_entry)
{
    FB_
    TICK_ENT_STATIC_IND(Node);	/* tick */
    R1.p = (P_) ((StgInd*)R1.p)->indirectee;
    TICK_ENT_VIA_NODE();
    JMP_(GET_ENTRY(R1.cl));
    FE_
}

INFO_TABLE(stg_IND_OLDGEN_PERM_info,stg_IND_OLDGEN_PERM_entry,1,1,IND_OLDGEN_PERM,,IF_,"IND_OLDGEN_PERM","IND_OLDGEN_PERM");
IF_(stg_IND_OLDGEN_PERM_entry)
{
    FB_
    /* Dont: TICK_ENT_STATIC_IND(Node); for ticky-ticky; this ind is here only to help profiling */

#if defined(TICKY_TICKY) && !defined(PROFILING)
    /* TICKY_TICKY && !PROFILING means PERM_IND *replaces* an IND, rather than being extra  */
    TICK_ENT_PERM_IND(R1.p); /* tick */
#endif

    LDV_ENTER((StgInd *)R1.p);

    /* Enter PAP cost centre -- lexical scoping only */
    ENTER_CCS_PAP_CL(R1.cl);

    /* see comment in IND_PERM */
#ifdef TICKY_TICKY
#  ifdef PROFILING
#    error Profiling and ticky-ticky do not mix at present!
#  endif  /* PROFILING */
    SET_INFO((StgInd*)R1.p,&stg_IND_OLDGEN_info);
#endif /* TICKY_TICKY */

    R1.p = (P_) ((StgInd*)R1.p)->indirectee;
    TICK_ENT_VIA_NODE();
    JMP_(GET_ENTRY(R1.cl));
    FE_
}

/* -----------------------------------------------------------------------------
   Entry code for a black hole.

   Entering a black hole normally causes a cyclic data dependency, but
   in the concurrent world, black holes are synchronization points,
   and they are turned into blocking queues when there are threads
   waiting for the evaluation of the closure to finish.
   -------------------------------------------------------------------------- */

/* Note: a BLACKHOLE and BLACKHOLE_BQ must be big enough to be
 * overwritten with an indirection/evacuee/catch.  Thus we claim it
 * has 1 non-pointer word of payload (in addition to the pointer word
 * for the blocking queue in a BQ), which should be big enough for an
 * old-generation indirection. 
 */

INFO_TABLE(stg_BLACKHOLE_info, stg_BLACKHOLE_entry,0,2,BLACKHOLE,,IF_,"BLACKHOLE","BLACKHOLE");
IF_(stg_BLACKHOLE_entry)
{
  FB_
#if defined(GRAN)
    /* Before overwriting TSO_LINK */
    STGCALL3(GranSimBlock,CurrentTSO,CurrentProc,(StgClosure *)R1.p /*Node*/);
#endif

#ifdef SMP
    {
      bdescr *bd = Bdescr(R1.p);
      if (bd->u.back != (bdescr *)BaseReg) {
	if (bd->gen_no >= 1 || bd->step->no >= 1) {
	  CMPXCHG(R1.cl->header.info, &stg_BLACKHOLE_info, &stg_WHITEHOLE_info);
	} else {
	  EXTFUN_RTS(stg_gc_enter_1_hponly);
	  JMP_(stg_gc_enter_1_hponly);
	}
      }
    }
#endif
    TICK_ENT_BH();

    // Actually this is not necessary because R1.p is about to be destroyed.
    LDV_ENTER((StgClosure *)R1.p);

    /* Put ourselves on the blocking queue for this black hole */
#if defined(GRAN) || defined(PAR)
    // in fact, only difference is the type of the end-of-queue marker!
    CurrentTSO->link = END_BQ_QUEUE;
    ((StgBlockingQueue *)R1.p)->blocking_queue = (StgBlockingQueueElement *)CurrentTSO;
#else
    CurrentTSO->link = END_TSO_QUEUE;
    ((StgBlockingQueue *)R1.p)->blocking_queue = CurrentTSO;
#endif
    // jot down why and on what closure we are blocked
    CurrentTSO->why_blocked = BlockedOnBlackHole;
    CurrentTSO->block_info.closure = R1.cl;

    /* Change the BLACKHOLE into a BLACKHOLE_BQ */
#ifdef PROFILING

    // The size remains the same, so we call LDV_recordDead() - no need to fill slop.
    LDV_recordDead((StgClosure *)R1.p, BLACKHOLE_sizeW());
#endif
    // 
    // Todo: maybe use SET_HDR() and remove LDV_recordCreate()?
    // 
    ((StgBlockingQueue *)R1.p)->header.info = &stg_BLACKHOLE_BQ_info;
#ifdef PROFILING
    LDV_recordCreate((StgClosure *)R1.p);
#endif

    // closure is mutable since something has just been added to its BQ
    recordMutable((StgMutClosure *)R1.cl);

    // PAR: dumping of event now done in blockThread -- HWL

    // stg_gen_block is too heavyweight, use a specialised one
    BLOCK_NP(1);
  FE_
}

INFO_TABLE(stg_BLACKHOLE_BQ_info, stg_BLACKHOLE_BQ_entry,1,1,BLACKHOLE_BQ,,IF_,"BLACKHOLE","BLACKHOLE");
IF_(stg_BLACKHOLE_BQ_entry)
{
  FB_
#if defined(GRAN)
    /* Before overwriting TSO_LINK */
    STGCALL3(GranSimBlock,CurrentTSO,CurrentProc,(StgClosure *)R1.p /*Node*/);
#endif

#ifdef SMP
    {
      bdescr *bd = Bdescr(R1.p);
      if (bd->u.back != (bdescr *)BaseReg) {
	if (bd->gen_no >= 1 || bd->step->no >= 1) {
	  CMPXCHG(R1.cl->header.info, &stg_BLACKHOLE_info, &stg_WHITEHOLE_info);
	} else {
	  EXTFUN_RTS(stg_gc_enter_1_hponly);
	  JMP_(stg_gc_enter_1_hponly);
	}
      }
    }
#endif

    TICK_ENT_BH();
    LDV_ENTER((StgClosure *)R1.p);

    /* Put ourselves on the blocking queue for this black hole */
    CurrentTSO->link = ((StgBlockingQueue *)R1.p)->blocking_queue;
    ((StgBlockingQueue *)R1.p)->blocking_queue = CurrentTSO;
    /* jot down why and on what closure we are blocked */
    CurrentTSO->why_blocked = BlockedOnBlackHole;
    CurrentTSO->block_info.closure = R1.cl;
#ifdef SMP
    ((StgBlockingQueue *)R1.p)->header.info = &stg_BLACKHOLE_BQ_info;
#endif

    /* PAR: dumping of event now done in blockThread -- HWL */

    /* stg_gen_block is too heavyweight, use a specialised one */
    BLOCK_NP(1);
  FE_
}

/*
   Revertible black holes are needed in the parallel world, to handle
   negative acknowledgements of messages containing updatable closures.
   The idea is that when the original message is transmitted, the closure
   is turned into a revertible black hole...an object which acts like a
   black hole when local threads try to enter it, but which can be reverted
   back to the original closure if necessary.

   It's actually a lot like a blocking queue (BQ) entry, because revertible
   black holes are initially set up with an empty blocking queue.
*/

#if defined(PAR) || defined(GRAN)

INFO_TABLE(stg_RBH_info, stg_RBH_entry,1,1,RBH,,IF_,"RBH","RBH");
IF_(stg_RBH_entry)
{
  FB_
# if defined(GRAN)
    /* mainly statistics gathering for GranSim simulation */
    STGCALL3(GranSimBlock,CurrentTSO,CurrentProc,(StgClosure *)R1.p /*Node*/);
# endif

    /* exactly the same as a BLACKHOLE_BQ_entry -- HWL */
    /* Put ourselves on the blocking queue for this black hole */
    CurrentTSO->link = ((StgBlockingQueue *)R1.p)->blocking_queue;
    ((StgBlockingQueue *)R1.p)->blocking_queue = CurrentTSO;
    /* jot down why and on what closure we are blocked */
    CurrentTSO->why_blocked = BlockedOnBlackHole;
    CurrentTSO->block_info.closure = R1.cl;

    /* PAR: dumping of event now done in blockThread -- HWL */

    /* stg_gen_block is too heavyweight, use a specialised one */
    BLOCK_NP(1); 
  FE_
}

INFO_TABLE(stg_RBH_Save_0_info, stg_RBH_Save_0_entry,0,2,CONSTR,,IF_,"RBH_Save_0","RBH_Save_0");
NON_ENTERABLE_ENTRY_CODE(RBH_Save_0);

INFO_TABLE(stg_RBH_Save_1_info, stg_RBH_Save_1_entry,1,1,CONSTR,,IF_,"RBH_Save_1","RBH_Save_1");
NON_ENTERABLE_ENTRY_CODE(RBH_Save_1);

INFO_TABLE(stg_RBH_Save_2_info, stg_RBH_Save_2_entry,2,0,CONSTR,,IF_,"RBH_Save_2","RBH_Save_2");
NON_ENTERABLE_ENTRY_CODE(RBH_Save_2);
#endif /* defined(PAR) || defined(GRAN) */

/* identical to BLACKHOLEs except for the infotag */
INFO_TABLE(stg_CAF_BLACKHOLE_info, stg_CAF_BLACKHOLE_entry,0,2,CAF_BLACKHOLE,,IF_,"CAF_BLACKHOLE","CAF_BLACKHOLE");
IF_(stg_CAF_BLACKHOLE_entry)
{
  FB_
#if defined(GRAN)
    /* mainly statistics gathering for GranSim simulation */
    STGCALL3(GranSimBlock,CurrentTSO,CurrentProc,(StgClosure *)R1.p /*Node*/);
#endif

#ifdef SMP
    {
      bdescr *bd = Bdescr(R1.p);
      if (bd->u.back != (bdescr *)BaseReg) {
	if (bd->gen_no >= 1 || bd->step->no >= 1) {
	  CMPXCHG(R1.cl->header.info, &stg_CAF_BLACKHOLE_info, &stg_WHITEHOLE_info);
	} else {
	  EXTFUN_RTS(stg_gc_enter_1_hponly);
	  JMP_(stg_gc_enter_1_hponly);
	}
      }
    }
#endif

    TICK_ENT_BH();
    LDV_ENTER((StgClosure *)R1.p);

    // Put ourselves on the blocking queue for this black hole
#if defined(GRAN) || defined(PAR)
    // in fact, only difference is the type of the end-of-queue marker!
    CurrentTSO->link = END_BQ_QUEUE;
    ((StgBlockingQueue *)R1.p)->blocking_queue = (StgBlockingQueueElement *)CurrentTSO;
#else
    CurrentTSO->link = END_TSO_QUEUE;
    ((StgBlockingQueue *)R1.p)->blocking_queue = CurrentTSO;
#endif
    // jot down why and on what closure we are blocked
    CurrentTSO->why_blocked = BlockedOnBlackHole;
    CurrentTSO->block_info.closure = R1.cl;

    // Change the CAF_BLACKHOLE into a BLACKHOLE_BQ_STATIC
    ((StgBlockingQueue *)R1.p)->header.info = &stg_BLACKHOLE_BQ_info;

    // closure is mutable since something has just been added to its BQ
    recordMutable((StgMutClosure *)R1.cl);

    // PAR: dumping of event now done in blockThread -- HWL

    // stg_gen_block is too heavyweight, use a specialised one
    BLOCK_NP(1);
  FE_
}

#ifdef EAGER_BLACKHOLING
INFO_TABLE(stg_SE_BLACKHOLE_info, stg_SE_BLACKHOLE_entry,0,2,SE_BLACKHOLE,,IF_,"SE_BLACKHOLE","SE_BLACKHOLE");
IF_(stg_SE_BLACKHOLE_entry)
{
  FB_
    STGCALL3(fprintf,stderr,"SE_BLACKHOLE at %p entered!\n",R1.p);
    STGCALL1(shutdownHaskellAndExit,EXIT_FAILURE);
  FE_
}

INFO_TABLE(stg_SE_CAF_BLACKHOLE_info, SE_CAF_BLACKHOLE_entry,0,2,SE_CAF_BLACKHOLE,,IF_,"CAF_BLACKHOLE","CAF_BLACKHOLE");
IF_(stg_SE_CAF_BLACKHOLE_entry)
{
  FB_
    STGCALL3(fprintf,stderr,"SE_CAF_BLACKHOLE at %p entered!\n",R1.p);
    STGCALL1(shutdownHaskellAndExit,EXIT_FAILURE);
  FE_
}
#endif

#ifdef SMP
INFO_TABLE(stg_WHITEHOLE_info, stg_WHITEHOLE_entry,0,2,CONSTR_NOCAF_STATIC,,IF_,"WHITEHOLE","WHITEHOLE");
IF_(stg_WHITEHOLE_entry)
{
  FB_
    JMP_(GET_ENTRY(R1.cl));
  FE_
}
#endif

/* -----------------------------------------------------------------------------
   Some static info tables for things that don't get entered, and
   therefore don't need entry code (i.e. boxed but unpointed objects)
   NON_ENTERABLE_ENTRY_CODE now defined at the beginning of the file
   -------------------------------------------------------------------------- */

INFO_TABLE(stg_TSO_info, stg_TSO_entry, 0,0,TSO,,IF_,"TSO","TSO");
NON_ENTERABLE_ENTRY_CODE(TSO);

/* -----------------------------------------------------------------------------
   Evacuees are left behind by the garbage collector.  Any attempt to enter
   one is a real bug.
   -------------------------------------------------------------------------- */

INFO_TABLE(stg_EVACUATED_info,stg_EVACUATED_entry,1,0,EVACUATED,,IF_,"EVACUATED","EVACUATED");
NON_ENTERABLE_ENTRY_CODE(EVACUATED);

/* -----------------------------------------------------------------------------
   Weak pointers

   Live weak pointers have a special closure type.  Dead ones are just
   nullary constructors (although they live on the heap - we overwrite
   live weak pointers with dead ones).
   -------------------------------------------------------------------------- */

INFO_TABLE(stg_WEAK_info,stg_WEAK_entry,0,4,WEAK,,IF_,"WEAK","WEAK");
NON_ENTERABLE_ENTRY_CODE(WEAK);

// It's important when turning an existing WEAK into a DEAD_WEAK
// (which is what finalizeWeak# does) that we don't lose the link
// field and break the linked list of weak pointers.  Hence, we give
// DEAD_WEAK 4 non-pointer fields, the same as WEAK.

INFO_TABLE_CONSTR(stg_DEAD_WEAK_info,stg_DEAD_WEAK_entry,0,4,0,CONSTR,,IF_,"DEAD_WEAK","DEAD_WEAK");
NON_ENTERABLE_ENTRY_CODE(DEAD_WEAK);

/* -----------------------------------------------------------------------------
   NO_FINALIZER

   This is a static nullary constructor (like []) that we use to mark an empty
   finalizer in a weak pointer object.
   -------------------------------------------------------------------------- */

INFO_TABLE_CONSTR(stg_NO_FINALIZER_info,stg_NO_FINALIZER_entry,0,0,0,CONSTR_NOCAF_STATIC,,IF_,"NO_FINALIZER","NO_FINALIZER");
NON_ENTERABLE_ENTRY_CODE(NO_FINALIZER);

SET_STATIC_HDR(stg_NO_FINALIZER_closure,stg_NO_FINALIZER_info,0/*CC*/,,extern const StgInfoTable)
, /*payload*/{} };

/* -----------------------------------------------------------------------------
   Foreign Objects are unlifted and therefore never entered.
   -------------------------------------------------------------------------- */

INFO_TABLE(stg_FOREIGN_info,stg_FOREIGN_entry,0,1,FOREIGN,,IF_,"FOREIGN","FOREIGN");
NON_ENTERABLE_ENTRY_CODE(FOREIGN);

/* -----------------------------------------------------------------------------
   Stable Names are unlifted too.
   -------------------------------------------------------------------------- */

INFO_TABLE(stg_STABLE_NAME_info,stg_STABLE_NAME_entry,0,1,STABLE_NAME,,IF_,"STABLE_NAME","STABLE_NAME");
NON_ENTERABLE_ENTRY_CODE(STABLE_NAME);

/* -----------------------------------------------------------------------------
   MVars

   There are two kinds of these: full and empty.  We need an info table
   and entry code for each type.
   -------------------------------------------------------------------------- */

INFO_TABLE(stg_FULL_MVAR_info,stg_FULL_MVAR_entry,4,0,MVAR,,IF_,"MVAR","MVAR");
NON_ENTERABLE_ENTRY_CODE(FULL_MVAR);

INFO_TABLE(stg_EMPTY_MVAR_info,stg_EMPTY_MVAR_entry,4,0,MVAR,,IF_,"MVAR","MVAR");
NON_ENTERABLE_ENTRY_CODE(EMPTY_MVAR);

/* -----------------------------------------------------------------------------
   END_TSO_QUEUE

   This is a static nullary constructor (like []) that we use to mark the
   end of a linked TSO queue.
   -------------------------------------------------------------------------- */

INFO_TABLE_CONSTR(stg_END_TSO_QUEUE_info,stg_END_TSO_QUEUE_entry,0,0,0,CONSTR_NOCAF_STATIC,,IF_,"END_TSO_QUEUE","END_TSO_QUEUE");
NON_ENTERABLE_ENTRY_CODE(END_TSO_QUEUE);

SET_STATIC_HDR(stg_END_TSO_QUEUE_closure,stg_END_TSO_QUEUE_info,0/*CC*/,,extern const StgInfoTable)
, /*payload*/{} };

/* -----------------------------------------------------------------------------
   Mutable lists

   Mutable lists (used by the garbage collector) consist of a chain of
   StgMutClosures connected through their mut_link fields, ending in
   an END_MUT_LIST closure.
   -------------------------------------------------------------------------- */

INFO_TABLE_CONSTR(stg_END_MUT_LIST_info,stg_END_MUT_LIST_entry,0,0,0,CONSTR_NOCAF_STATIC,,IF_,"END_MUT_LIST","END_MUT_LIST");
NON_ENTERABLE_ENTRY_CODE(END_MUT_LIST);

SET_STATIC_HDR(stg_END_MUT_LIST_closure,stg_END_MUT_LIST_info,0/*CC*/,,extern const StgInfoTable)
, /*payload*/{} };

INFO_TABLE(stg_MUT_CONS_info, stg_MUT_CONS_entry, 1, 1, MUT_CONS, , IF_, "MUT_CONS", "MUT_CONS");
NON_ENTERABLE_ENTRY_CODE(MUT_CONS);

/* -----------------------------------------------------------------------------
   Exception lists
   -------------------------------------------------------------------------- */

INFO_TABLE_CONSTR(stg_END_EXCEPTION_LIST_info,stg_END_EXCEPTION_LIST_entry,0,0,0,CONSTR_NOCAF_STATIC,,IF_,"END_EXCEPTION_LIST","END_EXCEPTION_LIST");
NON_ENTERABLE_ENTRY_CODE(END_EXCEPTION_LIST);

SET_STATIC_HDR(stg_END_EXCEPTION_LIST_closure,stg_END_EXCEPTION_LIST_info,0/*CC*/,,extern const StgInfoTable)
, /*payload*/{} };

INFO_TABLE(stg_EXCEPTION_CONS_info, stg_EXCEPTION_CONS_entry, 1, 1, CONSTR, , IF_, "EXCEPTION_CONS", "EXCEPTION_CONS");
NON_ENTERABLE_ENTRY_CODE(EXCEPTION_CONS);

/* -----------------------------------------------------------------------------
   Arrays

   These come in two basic flavours: arrays of data (StgArrWords) and arrays of
   pointers (StgArrPtrs).  They all have a similar layout:

	___________________________
	| Info | No. of | data....
        |  Ptr | Words  |
	---------------------------

   These are *unpointed* objects: i.e. they cannot be entered.

   -------------------------------------------------------------------------- */

#define ArrayInfo(type)					\
INFO_TABLE(stg_##type##_info, stg_##type##_entry, 0, 0, type, , IF_,"" # type "","" # type "");

ArrayInfo(ARR_WORDS);
NON_ENTERABLE_ENTRY_CODE(ARR_WORDS);
ArrayInfo(MUT_ARR_PTRS);
NON_ENTERABLE_ENTRY_CODE(MUT_ARR_PTRS);
ArrayInfo(MUT_ARR_PTRS_FROZEN);
NON_ENTERABLE_ENTRY_CODE(MUT_ARR_PTRS_FROZEN);

#undef ArrayInfo

/* -----------------------------------------------------------------------------
   Mutable Variables
   -------------------------------------------------------------------------- */

INFO_TABLE(stg_MUT_VAR_info, stg_MUT_VAR_entry, 1, 1, MUT_VAR, , IF_, "MUT_VAR", "MUT_VAR");
NON_ENTERABLE_ENTRY_CODE(MUT_VAR);

/* -----------------------------------------------------------------------------
   Dummy return closure
 
   Entering this closure will just return to the address on the top of the
   stack.  Useful for getting a thread in a canonical form where we can
   just enter the top stack word to start the thread.  (see deleteThread)
 * -------------------------------------------------------------------------- */

INFO_TABLE( stg_dummy_ret_info, stg_dummy_ret_entry, 
	    0, 0, CONSTR_NOCAF_STATIC, , EF_, "DUMMY_RET", "DUMMY_RET");

STGFUN(stg_dummy_ret_entry)
{
  FB_
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}
SET_STATIC_HDR(stg_dummy_ret_closure,stg_dummy_ret_info,CCS_DONT_CARE,,extern const StgInfoTable)
, /*payload*/{} };

/* -----------------------------------------------------------------------------
   CHARLIKE and INTLIKE closures.  

   These are static representations of Chars and small Ints, so that
   we can remove dynamic Chars and Ints during garbage collection and
   replace them with references to the static objects.
   -------------------------------------------------------------------------- */

#if defined(INTERPRETER) || defined(ENABLE_WIN32_DLL_SUPPORT)
/*
 * When sticking the RTS in a DLL, we delay populating the
 * Charlike and Intlike tables until load-time, which is only
 * when we've got the real addresses to the C# and I# closures.
 *
 */
static INFO_TBL_CONST StgInfoTable czh_static_info;
static INFO_TBL_CONST StgInfoTable izh_static_info;
#define Char_hash_static_info czh_static_info
#define Int_hash_static_info izh_static_info
#else
#define Char_hash_static_info GHCziBase_Czh_static_info
#define Int_hash_static_info GHCziBase_Izh_static_info
#endif

#define CHARLIKE_HDR(n)						\
	{							\
	  STATIC_HDR(Char_hash_static_info, /* C# */   		\
			 CCS_DONT_CARE),			\
          data : n						\
	}
					     
#define INTLIKE_HDR(n)						\
	{							\
	  STATIC_HDR(Int_hash_static_info,  /* I# */  		\
			 CCS_DONT_CARE),			\
          data : n						\
	}

/* put these in the *data* section, since the garbage collector relies
 * on the fact that static closures live in the data section.
 */

/* end the name with _closure, to convince the mangler this is a closure */

StgIntCharlikeClosure stg_CHARLIKE_closure[] = {
    CHARLIKE_HDR(0),
    CHARLIKE_HDR(1),
    CHARLIKE_HDR(2),
    CHARLIKE_HDR(3),
    CHARLIKE_HDR(4),
    CHARLIKE_HDR(5),
    CHARLIKE_HDR(6),
    CHARLIKE_HDR(7),
    CHARLIKE_HDR(8),
    CHARLIKE_HDR(9),
    CHARLIKE_HDR(10),
    CHARLIKE_HDR(11),
    CHARLIKE_HDR(12),
    CHARLIKE_HDR(13),
    CHARLIKE_HDR(14),
    CHARLIKE_HDR(15),
    CHARLIKE_HDR(16),
    CHARLIKE_HDR(17),
    CHARLIKE_HDR(18),
    CHARLIKE_HDR(19),
    CHARLIKE_HDR(20),
    CHARLIKE_HDR(21),
    CHARLIKE_HDR(22),
    CHARLIKE_HDR(23),
    CHARLIKE_HDR(24),
    CHARLIKE_HDR(25),
    CHARLIKE_HDR(26),
    CHARLIKE_HDR(27),
    CHARLIKE_HDR(28),
    CHARLIKE_HDR(29),
    CHARLIKE_HDR(30),
    CHARLIKE_HDR(31),
    CHARLIKE_HDR(32),
    CHARLIKE_HDR(33),
    CHARLIKE_HDR(34),
    CHARLIKE_HDR(35),
    CHARLIKE_HDR(36),
    CHARLIKE_HDR(37),
    CHARLIKE_HDR(38),
    CHARLIKE_HDR(39),
    CHARLIKE_HDR(40),
    CHARLIKE_HDR(41),
    CHARLIKE_HDR(42),
    CHARLIKE_HDR(43),
    CHARLIKE_HDR(44),
    CHARLIKE_HDR(45),
    CHARLIKE_HDR(46),
    CHARLIKE_HDR(47),
    CHARLIKE_HDR(48),
    CHARLIKE_HDR(49),
    CHARLIKE_HDR(50),
    CHARLIKE_HDR(51),
    CHARLIKE_HDR(52),
    CHARLIKE_HDR(53),
    CHARLIKE_HDR(54),
    CHARLIKE_HDR(55),
    CHARLIKE_HDR(56),
    CHARLIKE_HDR(57),
    CHARLIKE_HDR(58),
    CHARLIKE_HDR(59),
    CHARLIKE_HDR(60),
    CHARLIKE_HDR(61),
    CHARLIKE_HDR(62),
    CHARLIKE_HDR(63),
    CHARLIKE_HDR(64),
    CHARLIKE_HDR(65),
    CHARLIKE_HDR(66),
    CHARLIKE_HDR(67),
    CHARLIKE_HDR(68),
    CHARLIKE_HDR(69),
    CHARLIKE_HDR(70),
    CHARLIKE_HDR(71),
    CHARLIKE_HDR(72),
    CHARLIKE_HDR(73),
    CHARLIKE_HDR(74),
    CHARLIKE_HDR(75),
    CHARLIKE_HDR(76),
    CHARLIKE_HDR(77),
    CHARLIKE_HDR(78),
    CHARLIKE_HDR(79),
    CHARLIKE_HDR(80),
    CHARLIKE_HDR(81),
    CHARLIKE_HDR(82),
    CHARLIKE_HDR(83),
    CHARLIKE_HDR(84),
    CHARLIKE_HDR(85),
    CHARLIKE_HDR(86),
    CHARLIKE_HDR(87),
    CHARLIKE_HDR(88),
    CHARLIKE_HDR(89),
    CHARLIKE_HDR(90),
    CHARLIKE_HDR(91),
    CHARLIKE_HDR(92),
    CHARLIKE_HDR(93),
    CHARLIKE_HDR(94),
    CHARLIKE_HDR(95),
    CHARLIKE_HDR(96),
    CHARLIKE_HDR(97),
    CHARLIKE_HDR(98),
    CHARLIKE_HDR(99),
    CHARLIKE_HDR(100),
    CHARLIKE_HDR(101),
    CHARLIKE_HDR(102),
    CHARLIKE_HDR(103),
    CHARLIKE_HDR(104),
    CHARLIKE_HDR(105),
    CHARLIKE_HDR(106),
    CHARLIKE_HDR(107),
    CHARLIKE_HDR(108),
    CHARLIKE_HDR(109),
    CHARLIKE_HDR(110),
    CHARLIKE_HDR(111),
    CHARLIKE_HDR(112),
    CHARLIKE_HDR(113),
    CHARLIKE_HDR(114),
    CHARLIKE_HDR(115),
    CHARLIKE_HDR(116),
    CHARLIKE_HDR(117),
    CHARLIKE_HDR(118),
    CHARLIKE_HDR(119),
    CHARLIKE_HDR(120),
    CHARLIKE_HDR(121),
    CHARLIKE_HDR(122),
    CHARLIKE_HDR(123),
    CHARLIKE_HDR(124),
    CHARLIKE_HDR(125),
    CHARLIKE_HDR(126),
    CHARLIKE_HDR(127),
    CHARLIKE_HDR(128),
    CHARLIKE_HDR(129),
    CHARLIKE_HDR(130),
    CHARLIKE_HDR(131),
    CHARLIKE_HDR(132),
    CHARLIKE_HDR(133),
    CHARLIKE_HDR(134),
    CHARLIKE_HDR(135),
    CHARLIKE_HDR(136),
    CHARLIKE_HDR(137),
    CHARLIKE_HDR(138),
    CHARLIKE_HDR(139),
    CHARLIKE_HDR(140),
    CHARLIKE_HDR(141),
    CHARLIKE_HDR(142),
    CHARLIKE_HDR(143),
    CHARLIKE_HDR(144),
    CHARLIKE_HDR(145),
    CHARLIKE_HDR(146),
    CHARLIKE_HDR(147),
    CHARLIKE_HDR(148),
    CHARLIKE_HDR(149),
    CHARLIKE_HDR(150),
    CHARLIKE_HDR(151),
    CHARLIKE_HDR(152),
    CHARLIKE_HDR(153),
    CHARLIKE_HDR(154),
    CHARLIKE_HDR(155),
    CHARLIKE_HDR(156),
    CHARLIKE_HDR(157),
    CHARLIKE_HDR(158),
    CHARLIKE_HDR(159),
    CHARLIKE_HDR(160),
    CHARLIKE_HDR(161),
    CHARLIKE_HDR(162),
    CHARLIKE_HDR(163),
    CHARLIKE_HDR(164),
    CHARLIKE_HDR(165),
    CHARLIKE_HDR(166),
    CHARLIKE_HDR(167),
    CHARLIKE_HDR(168),
    CHARLIKE_HDR(169),
    CHARLIKE_HDR(170),
    CHARLIKE_HDR(171),
    CHARLIKE_HDR(172),
    CHARLIKE_HDR(173),
    CHARLIKE_HDR(174),
    CHARLIKE_HDR(175),
    CHARLIKE_HDR(176),
    CHARLIKE_HDR(177),
    CHARLIKE_HDR(178),
    CHARLIKE_HDR(179),
    CHARLIKE_HDR(180),
    CHARLIKE_HDR(181),
    CHARLIKE_HDR(182),
    CHARLIKE_HDR(183),
    CHARLIKE_HDR(184),
    CHARLIKE_HDR(185),
    CHARLIKE_HDR(186),
    CHARLIKE_HDR(187),
    CHARLIKE_HDR(188),
    CHARLIKE_HDR(189),
    CHARLIKE_HDR(190),
    CHARLIKE_HDR(191),
    CHARLIKE_HDR(192),
    CHARLIKE_HDR(193),
    CHARLIKE_HDR(194),
    CHARLIKE_HDR(195),
    CHARLIKE_HDR(196),
    CHARLIKE_HDR(197),
    CHARLIKE_HDR(198),
    CHARLIKE_HDR(199),
    CHARLIKE_HDR(200),
    CHARLIKE_HDR(201),
    CHARLIKE_HDR(202),
    CHARLIKE_HDR(203),
    CHARLIKE_HDR(204),
    CHARLIKE_HDR(205),
    CHARLIKE_HDR(206),
    CHARLIKE_HDR(207),
    CHARLIKE_HDR(208),
    CHARLIKE_HDR(209),
    CHARLIKE_HDR(210),
    CHARLIKE_HDR(211),
    CHARLIKE_HDR(212),
    CHARLIKE_HDR(213),
    CHARLIKE_HDR(214),
    CHARLIKE_HDR(215),
    CHARLIKE_HDR(216),
    CHARLIKE_HDR(217),
    CHARLIKE_HDR(218),
    CHARLIKE_HDR(219),
    CHARLIKE_HDR(220),
    CHARLIKE_HDR(221),
    CHARLIKE_HDR(222),
    CHARLIKE_HDR(223),
    CHARLIKE_HDR(224),
    CHARLIKE_HDR(225),
    CHARLIKE_HDR(226),
    CHARLIKE_HDR(227),
    CHARLIKE_HDR(228),
    CHARLIKE_HDR(229),
    CHARLIKE_HDR(230),
    CHARLIKE_HDR(231),
    CHARLIKE_HDR(232),
    CHARLIKE_HDR(233),
    CHARLIKE_HDR(234),
    CHARLIKE_HDR(235),
    CHARLIKE_HDR(236),
    CHARLIKE_HDR(237),
    CHARLIKE_HDR(238),
    CHARLIKE_HDR(239),
    CHARLIKE_HDR(240),
    CHARLIKE_HDR(241),
    CHARLIKE_HDR(242),
    CHARLIKE_HDR(243),
    CHARLIKE_HDR(244),
    CHARLIKE_HDR(245),
    CHARLIKE_HDR(246),
    CHARLIKE_HDR(247),
    CHARLIKE_HDR(248),
    CHARLIKE_HDR(249),
    CHARLIKE_HDR(250),
    CHARLIKE_HDR(251),
    CHARLIKE_HDR(252),
    CHARLIKE_HDR(253),
    CHARLIKE_HDR(254),
    CHARLIKE_HDR(255)
};

StgIntCharlikeClosure stg_INTLIKE_closure[] = {
    INTLIKE_HDR(-16),	/* MIN_INTLIKE == -16 */
    INTLIKE_HDR(-15),
    INTLIKE_HDR(-14),
    INTLIKE_HDR(-13),
    INTLIKE_HDR(-12),
    INTLIKE_HDR(-11),
    INTLIKE_HDR(-10),
    INTLIKE_HDR(-9),
    INTLIKE_HDR(-8),
    INTLIKE_HDR(-7),
    INTLIKE_HDR(-6),
    INTLIKE_HDR(-5),
    INTLIKE_HDR(-4),
    INTLIKE_HDR(-3),
    INTLIKE_HDR(-2),
    INTLIKE_HDR(-1),
    INTLIKE_HDR(0),
    INTLIKE_HDR(1),
    INTLIKE_HDR(2),
    INTLIKE_HDR(3),
    INTLIKE_HDR(4),
    INTLIKE_HDR(5),
    INTLIKE_HDR(6),
    INTLIKE_HDR(7),
    INTLIKE_HDR(8),
    INTLIKE_HDR(9),
    INTLIKE_HDR(10),
    INTLIKE_HDR(11),
    INTLIKE_HDR(12),
    INTLIKE_HDR(13),
    INTLIKE_HDR(14),
    INTLIKE_HDR(15),
    INTLIKE_HDR(16)	/* MAX_INTLIKE == 16 */
};
