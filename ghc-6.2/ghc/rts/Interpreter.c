/* -----------------------------------------------------------------------------
 * Bytecode interpreter
 *
 * Copyright (c) The GHC Team, 1994-2002.
 * ---------------------------------------------------------------------------*/

#if !defined(SMP)
#include "PosixSource.h"
#else
/* Hack and slash.. */
#include "Stg.h"
#endif
#include "Rts.h"
#include "RtsAPI.h"
#include "RtsUtils.h"
#include "Closures.h"
#include "TSO.h"
#include "Schedule.h"
#include "RtsFlags.h"
#include "Storage.h"
#include "Updates.h"
#include "Sanity.h"

#include "Bytecodes.h"
#include "Printer.h"
#include "Disassembler.h"
#include "Interpreter.h"

#include <string.h>     /* for memcpy */
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif


/* --------------------------------------------------------------------------
 * The bytecode interpreter
 * ------------------------------------------------------------------------*/

/* Gather stats about entry, opcode, opcode-pair frequencies.  For
   tuning the interpreter. */

/* #define INTERP_STATS */


/* Sp points to the lowest live word on the stack. */

#define BCO_NEXT      instrs[bciPtr++]
#define BCO_PTR(n)    (W_)ptrs[n]
#define BCO_LIT(n)    (W_)literals[n]
#define BCO_ITBL(n)   itbls[n]

#define LOAD_STACK_POINTERS					\
    Sp = cap->r.rCurrentTSO->sp;				\
    /* We don't change this ... */				\
    SpLim = cap->r.rCurrentTSO->stack + RESERVED_STACK_WORDS;

#define SAVE_STACK_POINTERS			\
    cap->r.rCurrentTSO->sp = Sp

#define RETURN_TO_SCHEDULER(todo,retcode)	\
   SAVE_STACK_POINTERS; 			\
   cap->r.rCurrentTSO->what_next = (todo);      \
   return (retcode);


static inline StgPtr
allocate_UPD (int n_words)
{
   return allocate(stg_max(sizeofW(StgHeader)+MIN_UPD_SIZE, n_words));
}

static inline StgPtr
allocate_NONUPD (int n_words)
{
    return allocate(stg_max(sizeofW(StgHeader)+MIN_NONUPD_SIZE, n_words));
}


#ifdef INTERP_STATS

/* Hacky stats, for tuning the interpreter ... */
int it_unknown_entries[N_CLOSURE_TYPES];
int it_total_unknown_entries;
int it_total_entries;

int it_retto_BCO;
int it_retto_UPDATE;
int it_retto_other;

int it_slides;
int it_insns;
int it_BCO_entries;

int it_ofreq[27];
int it_oofreq[27][27];
int it_lastopc;

#define INTERP_TICK(n) (n)++

void interp_startup ( void )
{
   int i, j;
   it_retto_BCO = it_retto_UPDATE = it_retto_other = 0;
   it_total_entries = it_total_unknown_entries = 0;
   for (i = 0; i < N_CLOSURE_TYPES; i++)
      it_unknown_entries[i] = 0;
   it_slides = it_insns = it_BCO_entries = 0;
   for (i = 0; i < 27; i++) it_ofreq[i] = 0;
   for (i = 0; i < 27; i++) 
     for (j = 0; j < 27; j++)
        it_oofreq[i][j] = 0;
   it_lastopc = 0;
}

void interp_shutdown ( void )
{
   int i, j, k, o_max, i_max, j_max;
   fprintf(stderr, "%d constrs entered -> (%d BCO, %d UPD, %d ??? )\n",
                   it_retto_BCO + it_retto_UPDATE + it_retto_other,
                   it_retto_BCO, it_retto_UPDATE, it_retto_other );
   fprintf(stderr, "%d total entries, %d unknown entries \n", 
                   it_total_entries, it_total_unknown_entries);
   for (i = 0; i < N_CLOSURE_TYPES; i++) {
     if (it_unknown_entries[i] == 0) continue;
     fprintf(stderr, "   type %2d: unknown entries (%4.1f%%) == %d\n",
	     i, 100.0 * ((double)it_unknown_entries[i]) / 
                        ((double)it_total_unknown_entries),
             it_unknown_entries[i]);
   }
   fprintf(stderr, "%d insns, %d slides, %d BCO_entries\n", 
                   it_insns, it_slides, it_BCO_entries);
   for (i = 0; i < 27; i++) 
      fprintf(stderr, "opcode %2d got %d\n", i, it_ofreq[i] );

   for (k = 1; k < 20; k++) {
      o_max = 0;
      i_max = j_max = 0;
      for (i = 0; i < 27; i++) {
         for (j = 0; j < 27; j++) {
	    if (it_oofreq[i][j] > o_max) {
               o_max = it_oofreq[i][j];
	       i_max = i; j_max = j;
	    }
	 }
      }
      
      fprintf ( stderr, "%d:  count (%4.1f%%) %6d   is %d then %d\n",
                k, ((double)o_max) * 100.0 / ((double)it_insns), o_max,
                   i_max, j_max );
      it_oofreq[i_max][j_max] = 0;

   }
}

#else // !INTERP_STATS

#define INTERP_TICK(n) /* nothing */

#endif

static StgWord app_ptrs_itbl[] = {
    (W_)&stg_ap_p_info,
    (W_)&stg_ap_pp_info,
    (W_)&stg_ap_ppp_info,
    (W_)&stg_ap_pppp_info,
    (W_)&stg_ap_ppppp_info,
    (W_)&stg_ap_pppppp_info,
    (W_)&stg_ap_ppppppp_info
};

StgThreadReturnCode
interpretBCO (Capability* cap)
{
    // Use of register here is primarily to make it clear to compilers
    // that these entities are non-aliasable.
    register StgPtr       Sp;    // local state -- stack pointer
    register StgPtr       SpLim; // local state -- stack lim pointer
    register StgClosure*  obj;
    nat n, m;

    LOAD_STACK_POINTERS;

    // ------------------------------------------------------------------------
    // Case 1:
    // 
    //       We have a closure to evaluate.  Stack looks like:
    //       
    //      	|   XXXX_info   |
    //      	+---------------+
    //       Sp |      -------------------> closure
    //      	+---------------+
    //       
    if (Sp[0] == (W_)&stg_enter_info) {
	Sp++;
	goto eval;
    }

    // ------------------------------------------------------------------------
    // Case 2:
    // 
    //       We have a BCO application to perform.  Stack looks like:
    //
    //      	|     ....      |
    //      	+---------------+
    //      	|     arg1      |
    //      	+---------------+
    //      	|     BCO       |
    //      	+---------------+
    //       Sp |   RET_BCO     |
    //      	+---------------+
    //       
    else if (Sp[0] == (W_)&stg_apply_interp_info) {
	obj = (StgClosure *)Sp[1];
	Sp += 2;
	goto run_BCO_fun;
    }

    // ------------------------------------------------------------------------
    // Case 3:
    //
    //       We have an unboxed value to return.  See comment before
    //       do_return_unboxed, below.
    //
    else {
	goto do_return_unboxed;
    }

    // Evaluate the object on top of the stack.
eval:
    obj = (StgClosure*)Sp[0]; Sp++;

eval_obj:
    INTERP_TICK(it_total_evals);

    IF_DEBUG(interpreter,
             fprintf(stderr, 
             "\n---------------------------------------------------------------\n");
             fprintf(stderr,"Evaluating: "); printObj(obj);
             fprintf(stderr,"Sp = %p\n", Sp);
             fprintf(stderr, "\n" );

             printStackChunk(Sp,cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size);
             fprintf(stderr, "\n\n");
            );

    IF_DEBUG(sanity,checkStackChunk(Sp, cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size));

    switch ( get_itbl(obj)->type ) {

    case IND:
    case IND_OLDGEN:
    case IND_PERM:
    case IND_OLDGEN_PERM:
    case IND_STATIC:
    { 
	obj = ((StgInd*)obj)->indirectee;
	goto eval_obj;
    }
    
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
    case FUN:
    case FUN_1_0:
    case FUN_0_1:
    case FUN_2_0:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_STATIC:
    case PAP:
	// already in WHNF
	break;
	
    case BCO:
	ASSERT(((StgBCO *)obj)->arity > 0);
	break;

    case AP:	/* Copied from stg_AP_entry. */
    {
	nat i, words;
	StgAP *ap;
	
	ap = (StgAP*)obj;
	words = ap->n_args;
	
	// Stack check
	if (Sp - (words+sizeofW(StgUpdateFrame)) < SpLim) {
	    Sp -= 2;
	    Sp[1] = (W_)obj;
	    Sp[0] = (W_)&stg_enter_info;
	    RETURN_TO_SCHEDULER(ThreadInterpret, StackOverflow);
	}
	
	/* Ok; we're safe.  Party on.  Push an update frame. */
	Sp -= sizeofW(StgUpdateFrame);
	{
	    StgUpdateFrame *__frame;
	    __frame = (StgUpdateFrame *)Sp;
	    SET_INFO(__frame, (StgInfoTable *)&stg_upd_frame_info);
	    __frame->updatee = (StgClosure *)(ap);
	}
	
	/* Reload the stack */
	Sp -= words;
	for (i=0; i < words; i++) {
	    Sp[i] = (W_)ap->payload[i];
	}

	obj = (StgClosure*)ap->fun;
	ASSERT(get_itbl(obj)->type == BCO);
	goto run_BCO_fun;
    }

    default:
#ifdef INTERP_STATS
    { 
	int j;
	
	j = get_itbl(obj)->type;
	ASSERT(j >= 0 && j < N_CLOSURE_TYPES);
	it_unknown_entries[j]++;
	it_total_unknown_entries++;
    }
#endif
    {
	// Can't handle this object; yield to scheduler
	IF_DEBUG(interpreter,
		 fprintf(stderr, "evaluating unknown closure -- yielding to sched\n"); 
		 printObj(obj);
	    );
	Sp -= 2;
	Sp[1] = (W_)obj;
	Sp[0] = (W_)&stg_enter_info;
	RETURN_TO_SCHEDULER(ThreadRunGHC, ThreadYielding);
    }
    }

    // ------------------------------------------------------------------------
    // We now have an evaluated object (obj).  The next thing to
    // do is return it to the stack frame on top of the stack.
do_return:
    ASSERT(closure_HNF(obj));

    IF_DEBUG(interpreter,
             fprintf(stderr, 
             "\n---------------------------------------------------------------\n");
             fprintf(stderr,"Returning: "); printObj(obj);
             fprintf(stderr,"Sp = %p\n", Sp);
             fprintf(stderr, "\n" );
             printStackChunk(Sp,cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size);
             fprintf(stderr, "\n\n");
            );

    IF_DEBUG(sanity,checkStackChunk(Sp, cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size));

    switch (get_itbl((StgClosure *)Sp)->type) {

    case RET_SMALL: {
	const StgInfoTable *info;

	// NOTE: not using get_itbl().
	info = ((StgClosure *)Sp)->header.info;
	if (info == (StgInfoTable *)&stg_ap_v_info) {
	    n = 1; m = 0; goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_f_info) {
	    n = 1; m = 1; goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_d_info) {
	    n = 1; m = sizeofW(StgDouble); goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_l_info) {
	    n = 1; m = sizeofW(StgInt64); goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_n_info) {
	    n = 1; m = 1; goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_p_info) {
	    n = 1; m = 1; goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_pp_info) {
	    n = 2; m = 2; goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_ppp_info) {
	    n = 3; m = 3; goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_pppp_info) {
	    n = 4; m = 4; goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_ppppp_info) {
	    n = 5; m = 5; goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_pppppp_info) {
	    n = 6; m = 6; goto do_apply;
	}
	if (info == (StgInfoTable *)&stg_ap_ppppppp_info) {
	    n = 7; m = 7; goto do_apply;
	}
	goto do_return_unrecognised;
    }

    case UPDATE_FRAME:
	// Returning to an update frame: do the update, pop the update
	// frame, and continue with the next stack frame.
	INTERP_TICK(it_retto_UPDATE);
	UPD_IND(((StgUpdateFrame *)Sp)->updatee, obj); 
	Sp += sizeofW(StgUpdateFrame);
	goto do_return;

    case RET_BCO:
	// Returning to an interpreted continuation: put the object on
	// the stack, and start executing the BCO.
	INTERP_TICK(it_retto_BCO);
	Sp--;
	Sp[0] = (W_)obj;
	obj = (StgClosure*)Sp[2];
	ASSERT(get_itbl(obj)->type == BCO);
	goto run_BCO_return;

    default:
    do_return_unrecognised:
    {
	// Can't handle this return address; yield to scheduler
	INTERP_TICK(it_retto_other);
	IF_DEBUG(interpreter,
		 fprintf(stderr, "returning to unknown frame -- yielding to sched\n"); 
		 printStackChunk(Sp,cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size);
	    );
	Sp -= 2;
	Sp[1] = (W_)obj;
	Sp[0] = (W_)&stg_enter_info;
	RETURN_TO_SCHEDULER(ThreadRunGHC, ThreadYielding);
    }
    }

    // -------------------------------------------------------------------------
    // Returning an unboxed value.  The stack looks like this:
    //
    // 	  |     ....      |
    // 	  +---------------+
    // 	  |     fv2       |
    // 	  +---------------+
    // 	  |     fv1       |
    // 	  +---------------+
    // 	  |     BCO       |
    // 	  +---------------+
    // 	  | stg_ctoi_ret_ |
    // 	  +---------------+
    // 	  |    retval     |
    // 	  +---------------+
    // 	  |   XXXX_info   |
    // 	  +---------------+
    //
    // where XXXX_info is one of the stg_gc_unbx_r1_info family.
    //
    // We're only interested in the case when the real return address
    // is a BCO; otherwise we'll return to the scheduler.

do_return_unboxed:
    { 
	int offset;
	
	ASSERT( Sp[0] == (W_)&stg_gc_unbx_r1_info
		|| Sp[0] == (W_)&stg_gc_unpt_r1_info
		|| Sp[0] == (W_)&stg_gc_f1_info
		|| Sp[0] == (W_)&stg_gc_d1_info
		|| Sp[0] == (W_)&stg_gc_l1_info
		|| Sp[0] == (W_)&stg_gc_void_info // VoidRep
	    );

	// get the offset of the stg_ctoi_ret_XXX itbl
	offset = stack_frame_sizeW((StgClosure *)Sp);

	switch (get_itbl((StgClosure *)Sp+offset)->type) {

	case RET_BCO:
	    // Returning to an interpreted continuation: put the object on
	    // the stack, and start executing the BCO.
	    INTERP_TICK(it_retto_BCO);
	    obj = (StgClosure*)Sp[offset+1];
	    ASSERT(get_itbl(obj)->type == BCO);
	    goto run_BCO_return_unboxed;

	default:
	{
	    // Can't handle this return address; yield to scheduler
	    INTERP_TICK(it_retto_other);
	    IF_DEBUG(interpreter,
		     fprintf(stderr, "returning to unknown frame -- yielding to sched\n"); 
		     printStackChunk(Sp,cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size);
		);
	    RETURN_TO_SCHEDULER(ThreadRunGHC, ThreadYielding);
	}
	}
    }
    // not reached.


    // -------------------------------------------------------------------------
    // Application...

do_apply:
    // we have a function to apply (obj), and n arguments taking up m
    // words on the stack.  The info table (stg_ap_pp_info or whatever)
    // is on top of the arguments on the stack.
    {
	switch (get_itbl(obj)->type) {

	case PAP: {
	    StgPAP *pap;
	    nat arity, i;

	    pap = (StgPAP *)obj;

	    // we only cope with PAPs whose function is a BCO
	    if (get_itbl(pap->fun)->type != BCO) {
		goto defer_apply_to_sched;
	    }

	    Sp++;
	    arity = pap->arity;
	    ASSERT(arity > 0);
	    if (arity < n) {
		// n must be greater than 1, and the only kinds of
		// application we support with more than one argument
		// are all pointers...
		//
		// Shuffle the args for this function down, and put
		// the appropriate info table in the gap.
		for (i = 0; i < arity; i++) {
		    Sp[i-1] = Sp[i];
		}
		Sp[arity-1] = app_ptrs_itbl[n-arity-1];
		Sp--;
		// unpack the PAP's arguments onto the stack
		Sp -= pap->n_args;
		for (i = 0; i < pap->n_args; i++) {
		    Sp[i] = (W_)pap->payload[i];
		}
		obj = pap->fun;
		goto run_BCO_fun;
	    } 
	    else if (arity == n) {
		Sp -= pap->n_args;
		for (i = 0; i < pap->n_args; i++) {
		    Sp[i] = (W_)pap->payload[i];
		}
		obj = pap->fun;
		goto run_BCO_fun;
	    } 
	    else /* arity > n */ {
		// build a new PAP and return it.
		StgPAP *new_pap;
		nat size;
		size = PAP_sizeW(pap->n_args + m);
		new_pap = (StgPAP *)allocate(size);
		SET_HDR(new_pap,&stg_PAP_info,CCCS);
		new_pap->arity = pap->arity - n;
		new_pap->n_args = pap->n_args + m;
		new_pap->fun = pap->fun;
		for (i = 0; i < pap->n_args; i++) {
		    new_pap->payload[i] = pap->payload[i];
		}
		for (i = 0; i < m; i++) {
		    new_pap->payload[pap->n_args + i] = (StgClosure *)Sp[i];
		}
		obj = (StgClosure *)new_pap;
		Sp += m;
		goto do_return;
	    }
	}	    

	case BCO: {
	    nat arity, i;

	    Sp++;
	    arity = ((StgBCO *)obj)->arity;
	    ASSERT(arity > 0);
	    if (arity < n) {
		// n must be greater than 1, and the only kinds of
		// application we support with more than one argument
		// are all pointers...
		//
		// Shuffle the args for this function down, and put
		// the appropriate info table in the gap.
		for (i = 0; i < arity; i++) {
		    Sp[i-1] = Sp[i];
		}
		Sp[arity-1] = app_ptrs_itbl[n-arity-1];
		Sp--;
		goto run_BCO_fun;
	    } 
	    else if (arity == n) {
		goto run_BCO_fun;
	    }
	    else /* arity > n */ {
		// build a PAP and return it.
		StgPAP *pap;
		nat size, i;
		size = PAP_sizeW(m);
		pap = (StgPAP *)allocate(size);
		SET_HDR(pap, &stg_PAP_info,CCCS);
		pap->arity = arity - n;
		pap->fun = obj;
		pap->n_args = m;
		for (i = 0; i < m; i++) {
		    pap->payload[i] = (StgClosure *)Sp[i];
		}
		obj = (StgClosure *)pap;
		Sp += m;
		goto do_return;
	    }
	}

	// No point in us applying machine-code functions
	default:
	defer_apply_to_sched:
	    Sp -= 2;
	    Sp[1] = (W_)obj;
	    Sp[0] = (W_)&stg_enter_info;
	    RETURN_TO_SCHEDULER(ThreadRunGHC, ThreadYielding);
    }

    // ------------------------------------------------------------------------
    // Ok, we now have a bco (obj), and its arguments are all on the
    // stack.  We can start executing the byte codes.
    //
    // The stack is in one of two states.  First, if this BCO is a
    // function:
    //
    // 	  |     ....      |
    // 	  +---------------+
    // 	  |     arg2      |
    // 	  +---------------+
    // 	  |     arg1      |
    // 	  +---------------+
    //
    // Second, if this BCO is a continuation:
    //
    // 	  |     ....      |
    // 	  +---------------+
    // 	  |     fv2       |
    // 	  +---------------+
    // 	  |     fv1       |
    // 	  +---------------+
    // 	  |     BCO       |
    // 	  +---------------+
    // 	  | stg_ctoi_ret_ |
    // 	  +---------------+
    // 	  |    retval     |
    // 	  +---------------+
    // 
    // where retval is the value being returned to this continuation.
    // In the event of a stack check, heap check, or context switch,
    // we need to leave the stack in a sane state so the garbage
    // collector can find all the pointers.
    //
    //  (1) BCO is a function:  the BCO's bitmap describes the
    //      pointerhood of the arguments.
    //
    //  (2) BCO is a continuation: BCO's bitmap describes the
    //      pointerhood of the free variables.
    //
    // Sadly we have three different kinds of stack/heap/cswitch check
    // to do:

run_BCO_return:
    // Heap check
    if (doYouWantToGC()) {
	Sp--; Sp[0] = (W_)&stg_enter_info;
	RETURN_TO_SCHEDULER(ThreadInterpret, HeapOverflow);
    }
    // Stack checks aren't necessary at return points, the stack use
    // is aggregated into the enclosing function entry point.
    goto run_BCO;
    
run_BCO_return_unboxed:
    // Heap check
    if (doYouWantToGC()) {
	RETURN_TO_SCHEDULER(ThreadInterpret, HeapOverflow);
    }
    // Stack checks aren't necessary at return points, the stack use
    // is aggregated into the enclosing function entry point.
    goto run_BCO;
    
run_BCO_fun:
    IF_DEBUG(sanity,
	     Sp -= 2; 
	     Sp[1] = (W_)obj; 
	     Sp[0] = (W_)&stg_apply_interp_info;
	     checkStackChunk(Sp,SpLim);
	     Sp += 2;
	);

    // Heap check
    if (doYouWantToGC()) {
	Sp -= 2; 
	Sp[1] = (W_)obj; 
	Sp[0] = (W_)&stg_apply_interp_info; // placeholder, really
	RETURN_TO_SCHEDULER(ThreadInterpret, HeapOverflow);
    }
    
    // Stack check
    if (Sp - INTERP_STACK_CHECK_THRESH < SpLim) {
	Sp -= 2; 
	Sp[1] = (W_)obj; 
	Sp[0] = (W_)&stg_apply_interp_info; // placeholder, really
	RETURN_TO_SCHEDULER(ThreadInterpret, StackOverflow);
    }
    goto run_BCO;
    
    // Now, actually interpret the BCO... (no returning to the
    // scheduler again until the stack is in an orderly state).
run_BCO:
    INTERP_TICK(it_BCO_entries);
    {
	register int       bciPtr     = 1; /* instruction pointer */
	register StgBCO*   bco        = (StgBCO*)obj;
	register StgWord16* instrs    = (StgWord16*)(bco->instrs->payload);
	register StgWord*  literals   = (StgWord*)(&bco->literals->payload[0]);
	register StgPtr*   ptrs       = (StgPtr*)(&bco->ptrs->payload[0]);
	register StgInfoTable** itbls = (StgInfoTable**)
	    (&bco->itbls->payload[0]);

#ifdef INTERP_STATS
	it_lastopc = 0; /* no opcode */
#endif

    nextInsn:
	ASSERT(bciPtr <= instrs[0]);
	IF_DEBUG(interpreter,
		 //if (do_print_stack) {
		 //fprintf(stderr, "\n-- BEGIN stack\n");
		 //printStack(Sp,cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size,iSu);
		 //fprintf(stderr, "-- END stack\n\n");
		 //}
		 fprintf(stderr,"Sp = %p   pc = %d      ", Sp, bciPtr);
		 disInstr(bco,bciPtr);
		 if (0) { int i;
		 fprintf(stderr,"\n");
		 for (i = 8; i >= 0; i--) {
		     fprintf(stderr, "%d  %p\n", i, (StgPtr)(*(Sp+i)));
		 }
		 fprintf(stderr,"\n");
		 }
		 //if (do_print_stack) checkStack(Sp,cap->r.rCurrentTSO->stack+cap->r.rCurrentTSO->stack_size,iSu);
	    );

	INTERP_TICK(it_insns);

#ifdef INTERP_STATS
	ASSERT( (int)instrs[bciPtr] >= 0 && (int)instrs[bciPtr] < 27 );
	it_ofreq[ (int)instrs[bciPtr] ] ++;
	it_oofreq[ it_lastopc ][ (int)instrs[bciPtr] ] ++;
	it_lastopc = (int)instrs[bciPtr];
#endif

	switch (BCO_NEXT) {

	case bci_STKCHECK: {
	    // Explicit stack check at the beginning of a function
	    // *only* (stack checks in case alternatives are
	    // propagated to the enclosing function).
	    int stk_words_reqd = BCO_NEXT + 1;
	    if (Sp - stk_words_reqd < SpLim) {
		Sp -= 2; 
		Sp[1] = (W_)obj; 
		Sp[0] = (W_)&stg_apply_interp_info;
		RETURN_TO_SCHEDULER(ThreadInterpret, StackOverflow);
	    } else {
		goto nextInsn;
	    }
	}

	case bci_PUSH_L: {
	    int o1 = BCO_NEXT;
	    Sp[-1] = Sp[o1];
	    Sp--;
	    goto nextInsn;
	}

	case bci_PUSH_LL: {
	    int o1 = BCO_NEXT;
	    int o2 = BCO_NEXT;
	    Sp[-1] = Sp[o1];
	    Sp[-2] = Sp[o2];
	    Sp -= 2;
	    goto nextInsn;
	}

	case bci_PUSH_LLL: {
	    int o1 = BCO_NEXT;
	    int o2 = BCO_NEXT;
	    int o3 = BCO_NEXT;
	    Sp[-1] = Sp[o1];
	    Sp[-2] = Sp[o2];
	    Sp[-3] = Sp[o3];
	    Sp -= 3;
	    goto nextInsn;
	}

	case bci_PUSH_G: {
	    int o1 = BCO_NEXT;
	    Sp[-1] = BCO_PTR(o1);
	    Sp -= 1;
	    goto nextInsn;
	}

	case bci_PUSH_ALTS: {
	    int o_bco  = BCO_NEXT;
	    Sp[-2] = (W_)&stg_ctoi_ret_R1p_info;
	    Sp[-1] = BCO_PTR(o_bco);
	    Sp -= 2;
	    goto nextInsn;
	}

	case bci_PUSH_ALTS_P: {
	    int o_bco  = BCO_NEXT;
	    Sp[-2] = (W_)&stg_ctoi_ret_R1unpt_info;
	    Sp[-1] = BCO_PTR(o_bco);
	    Sp -= 2;
	    goto nextInsn;
	}

	case bci_PUSH_ALTS_N: {
	    int o_bco  = BCO_NEXT;
	    Sp[-2] = (W_)&stg_ctoi_ret_R1n_info;
	    Sp[-1] = BCO_PTR(o_bco);
	    Sp -= 2;
	    goto nextInsn;
	}

	case bci_PUSH_ALTS_F: {
	    int o_bco  = BCO_NEXT;
	    Sp[-2] = (W_)&stg_ctoi_ret_F1_info;
	    Sp[-1] = BCO_PTR(o_bco);
	    Sp -= 2;
	    goto nextInsn;
	}

	case bci_PUSH_ALTS_D: {
	    int o_bco  = BCO_NEXT;
	    Sp[-2] = (W_)&stg_ctoi_ret_D1_info;
	    Sp[-1] = BCO_PTR(o_bco);
	    Sp -= 2;
	    goto nextInsn;
	}

	case bci_PUSH_ALTS_L: {
	    int o_bco  = BCO_NEXT;
	    Sp[-2] = (W_)&stg_ctoi_ret_L1_info;
	    Sp[-1] = BCO_PTR(o_bco);
	    Sp -= 2;
	    goto nextInsn;
	}

	case bci_PUSH_ALTS_V: {
	    int o_bco  = BCO_NEXT;
	    Sp[-2] = (W_)&stg_ctoi_ret_V_info;
	    Sp[-1] = BCO_PTR(o_bco);
	    Sp -= 2;
	    goto nextInsn;
	}

	case bci_PUSH_APPLY_N:
	    Sp--; Sp[0] = (W_)&stg_ap_n_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_V:
	    Sp--; Sp[0] = (W_)&stg_ap_v_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_F:
	    Sp--; Sp[0] = (W_)&stg_ap_f_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_D:
	    Sp--; Sp[0] = (W_)&stg_ap_d_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_L:
	    Sp--; Sp[0] = (W_)&stg_ap_l_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_P:
	    Sp--; Sp[0] = (W_)&stg_ap_p_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_PP:
	    Sp--; Sp[0] = (W_)&stg_ap_pp_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_PPP:
	    Sp--; Sp[0] = (W_)&stg_ap_ppp_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_PPPP:
	    Sp--; Sp[0] = (W_)&stg_ap_pppp_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_PPPPP:
	    Sp--; Sp[0] = (W_)&stg_ap_ppppp_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_PPPPPP:
	    Sp--; Sp[0] = (W_)&stg_ap_pppppp_info;
	    goto nextInsn;
	case bci_PUSH_APPLY_PPPPPPP:
	    Sp--; Sp[0] = (W_)&stg_ap_ppppppp_info;
	    goto nextInsn;
	    
	case bci_PUSH_UBX: {
	    int i;
	    int o_lits = BCO_NEXT;
	    int n_words = BCO_NEXT;
	    Sp -= n_words;
	    for (i = 0; i < n_words; i++) {
		Sp[i] = BCO_LIT(o_lits+i);
	    }
	    goto nextInsn;
	}

	case bci_SLIDE: {
	    int n  = BCO_NEXT;
	    int by = BCO_NEXT;
	    /* a_1, .. a_n, b_1, .. b_by, s => a_1, .. a_n, s */
	    while(--n >= 0) {
		Sp[n+by] = Sp[n];
	    }
	    Sp += by;
	    INTERP_TICK(it_slides);
	    goto nextInsn;
	}

	case bci_ALLOC_AP: {
	    StgAP* ap; 
	    int n_payload = BCO_NEXT;
	    int request   = PAP_sizeW(n_payload);
	    ap = (StgAP*)allocate_UPD(request);
	    Sp[-1] = (W_)ap;
	    ap->n_args = n_payload;
	    SET_HDR(ap, &stg_AP_info, CCS_SYSTEM/*ToDo*/)
	    Sp --;
	    goto nextInsn;
	}

	case bci_ALLOC_PAP: {
	    StgPAP* pap; 
	    int arity = BCO_NEXT;
	    int n_payload = BCO_NEXT;
	    int request   = PAP_sizeW(n_payload);
	    pap = (StgPAP*)allocate_NONUPD(request);
	    Sp[-1] = (W_)pap;
	    pap->n_args = n_payload;
	    pap->arity = arity;
	    SET_HDR(pap, &stg_PAP_info, CCS_SYSTEM/*ToDo*/)
	    Sp --;
	    goto nextInsn;
	}

	case bci_MKAP: {
	    int i;
	    int stkoff = BCO_NEXT;
	    int n_payload = BCO_NEXT;
	    StgAP* ap = (StgAP*)Sp[stkoff];
	    ASSERT((int)ap->n_args == n_payload);
	    ap->fun = (StgClosure*)Sp[0];

	    // The function should be a BCO, and its bitmap should
	    // cover the payload of the AP correctly.
	    ASSERT(get_itbl(ap->fun)->type == BCO
		   && (get_itbl(ap)->type == PAP || 
		       BCO_BITMAP_SIZE(ap->fun) == ap->n_args));

	    for (i = 0; i < n_payload; i++)
		ap->payload[i] = (StgClosure*)Sp[i+1];
	    Sp += n_payload+1;
	    IF_DEBUG(interpreter,
		     fprintf(stderr,"\tBuilt "); 
		     printObj((StgClosure*)ap);
		);
	    goto nextInsn;
	}

	case bci_UNPACK: {
	    /* Unpack N ptr words from t.o.s constructor */
	    int i;
	    int n_words = BCO_NEXT;
	    StgClosure* con = (StgClosure*)Sp[0];
	    Sp -= n_words;
	    for (i = 0; i < n_words; i++) {
		Sp[i] = (W_)con->payload[i];
	    }
	    goto nextInsn;
	}

	case bci_PACK: {
	    int i;
	    int o_itbl         = BCO_NEXT;
	    int n_words        = BCO_NEXT;
	    StgInfoTable* itbl = INFO_PTR_TO_STRUCT(BCO_ITBL(o_itbl));
	    int request        = CONSTR_sizeW( itbl->layout.payload.ptrs, 
					       itbl->layout.payload.nptrs );
	    StgClosure* con = (StgClosure*)allocate_NONUPD(request);
	    ASSERT( itbl->layout.payload.ptrs + itbl->layout.payload.nptrs > 0);
	    SET_HDR(con, BCO_ITBL(o_itbl), CCS_SYSTEM/*ToDo*/);
	    for (i = 0; i < n_words; i++) {
		con->payload[i] = (StgClosure*)Sp[i];
	    }
	    Sp += n_words;
	    Sp --;
	    Sp[0] = (W_)con;
	    IF_DEBUG(interpreter,
		     fprintf(stderr,"\tBuilt "); 
		     printObj((StgClosure*)con);
		);
	    goto nextInsn;
	}

	case bci_TESTLT_P: {
	    int discr  = BCO_NEXT;
	    int failto = BCO_NEXT;
	    StgClosure* con = (StgClosure*)Sp[0];
	    if (constrTag(con) >= discr) {
		bciPtr = failto;
	    }
	    goto nextInsn;
	}

	case bci_TESTEQ_P: {
	    int discr  = BCO_NEXT;
	    int failto = BCO_NEXT;
	    StgClosure* con = (StgClosure*)Sp[0];
	    if (constrTag(con) != discr) {
		bciPtr = failto;
	    }
	    goto nextInsn;
	}

	case bci_TESTLT_I: {
	    // There should be an Int at Sp[1], and an info table at Sp[0].
	    int discr   = BCO_NEXT;
	    int failto  = BCO_NEXT;
	    I_ stackInt = (I_)Sp[1];
	    if (stackInt >= (I_)BCO_LIT(discr))
		bciPtr = failto;
	    goto nextInsn;
	}

	case bci_TESTEQ_I: {
	    // There should be an Int at Sp[1], and an info table at Sp[0].
	    int discr   = BCO_NEXT;
	    int failto  = BCO_NEXT;
	    I_ stackInt = (I_)Sp[1];
	    if (stackInt != (I_)BCO_LIT(discr)) {
		bciPtr = failto;
	    }
	    goto nextInsn;
	}

	case bci_TESTLT_D: {
	    // There should be a Double at Sp[1], and an info table at Sp[0].
	    int discr   = BCO_NEXT;
	    int failto  = BCO_NEXT;
	    StgDouble stackDbl, discrDbl;
	    stackDbl = PK_DBL( & Sp[1] );
	    discrDbl = PK_DBL( & BCO_LIT(discr) );
	    if (stackDbl >= discrDbl) {
		bciPtr = failto;
	    }
	    goto nextInsn;
	}

	case bci_TESTEQ_D: {
	    // There should be a Double at Sp[1], and an info table at Sp[0].
	    int discr   = BCO_NEXT;
	    int failto  = BCO_NEXT;
	    StgDouble stackDbl, discrDbl;
	    stackDbl = PK_DBL( & Sp[1] );
	    discrDbl = PK_DBL( & BCO_LIT(discr) );
	    if (stackDbl != discrDbl) {
		bciPtr = failto;
	    }
	    goto nextInsn;
	}

	case bci_TESTLT_F: {
	    // There should be a Float at Sp[1], and an info table at Sp[0].
	    int discr   = BCO_NEXT;
	    int failto  = BCO_NEXT;
	    StgFloat stackFlt, discrFlt;
	    stackFlt = PK_FLT( & Sp[1] );
	    discrFlt = PK_FLT( & BCO_LIT(discr) );
	    if (stackFlt >= discrFlt) {
		bciPtr = failto;
	    }
	    goto nextInsn;
	}

	case bci_TESTEQ_F: {
	    // There should be a Float at Sp[1], and an info table at Sp[0].
	    int discr   = BCO_NEXT;
	    int failto  = BCO_NEXT;
	    StgFloat stackFlt, discrFlt;
	    stackFlt = PK_FLT( & Sp[1] );
	    discrFlt = PK_FLT( & BCO_LIT(discr) );
	    if (stackFlt != discrFlt) {
		bciPtr = failto;
	    }
	    goto nextInsn;
	}

	// Control-flow ish things
	case bci_ENTER:
	    // Context-switch check.  We put it here to ensure that
	    // the interpreter has done at least *some* work before
	    // context switching: sometimes the scheduler can invoke
	    // the interpreter with context_switch == 1, particularly
	    // if the -C0 flag has been given on the cmd line.
	    if (context_switch) {
		Sp--; Sp[0] = (W_)&stg_enter_info;
		RETURN_TO_SCHEDULER(ThreadInterpret, ThreadYielding);
	    }
	    goto eval;

	case bci_RETURN:
	    obj = (StgClosure *)Sp[0];
	    Sp++;
	    goto do_return;

	case bci_RETURN_P:
	    Sp--;
	    Sp[0] = (W_)&stg_gc_unpt_r1_info;
	    goto do_return_unboxed;
	case bci_RETURN_N:
	    Sp--;
	    Sp[0] = (W_)&stg_gc_unbx_r1_info;
	    goto do_return_unboxed;
	case bci_RETURN_F:
	    Sp--;
	    Sp[0] = (W_)&stg_gc_f1_info;
	    goto do_return_unboxed;
	case bci_RETURN_D:
	    Sp--;
	    Sp[0] = (W_)&stg_gc_d1_info;
	    goto do_return_unboxed;
	case bci_RETURN_L:
	    Sp--;
	    Sp[0] = (W_)&stg_gc_l1_info;
	    goto do_return_unboxed;
	case bci_RETURN_V:
	    Sp--;
	    Sp[0] = (W_)&stg_gc_void_info;
	    goto do_return_unboxed;

	case bci_SWIZZLE: {
	    int stkoff = BCO_NEXT;
	    signed short n = (signed short)(BCO_NEXT);
	    Sp[stkoff] += (W_)n;
	    goto nextInsn;
	}

	case bci_CCALL: {
	    StgInt tok;
	    int stk_offset            = BCO_NEXT;
	    int o_itbl                = BCO_NEXT;
	    void(*marshall_fn)(void*) = (void (*)(void*))BCO_LIT(o_itbl);
	    int ret_dyn_size = 
		RET_DYN_BITMAP_SIZE + RET_DYN_NONPTR_REGS_SIZE
		+ sizeofW(StgRetDyn);

#ifdef RTS_SUPPORTS_THREADS
	    // Threaded RTS:
	    // Arguments on the TSO stack are not good, because garbage
	    // collection might move the TSO as soon as we call
	    // suspendThread below.

	    W_ arguments[stk_offset];
	    
	    memcpy(arguments, Sp, sizeof(W_) * stk_offset);
#endif

#ifndef STANDALONE
	    // Restore the Haskell thread's current value of errno
	    errno = cap->r.rCurrentTSO->saved_errno;
#endif

	    // There are a bunch of non-ptr words on the stack (the
	    // ccall args, the ccall fun address and space for the
	    // result), which we need to cover with an info table
	    // since we might GC during this call.
	    //
	    // We know how many (non-ptr) words there are before the
	    // next valid stack frame: it is the stk_offset arg to the
	    // CCALL instruction.   So we build a RET_DYN stack frame
	    // on the stack frame to describe this chunk of stack.
	    //
	    Sp -= ret_dyn_size;
	    ((StgRetDyn *)Sp)->liveness = ALL_NON_PTRS | N_NONPTRS(stk_offset);
	    ((StgRetDyn *)Sp)->info = (StgInfoTable *)&stg_gc_gen_info;

	    SAVE_STACK_POINTERS;
	    tok = suspendThread(&cap->r,rtsFalse);

#ifndef RTS_SUPPORTS_THREADS
	    // Careful:
	    // suspendThread might have shifted the stack
	    // around (stack squeezing), so we have to grab the real
	    // Sp out of the TSO to find the ccall args again.

	    marshall_fn ( (void*)(cap->r.rCurrentTSO->sp + ret_dyn_size) );
#else
	    // Threaded RTS:
	    // We already made a copy of the arguments above.

	    marshall_fn ( arguments );
#endif

	    // And restart the thread again, popping the RET_DYN frame.
	    cap = (Capability *)((void *)resumeThread(tok,rtsFalse) - sizeof(StgFunTable));
	    LOAD_STACK_POINTERS;
	    Sp += ret_dyn_size;
	    
#ifndef STANDALONE
	    // Save the Haskell thread's current value of errno
	    cap->r.rCurrentTSO->saved_errno = errno;
#endif
		
#ifdef RTS_SUPPORTS_THREADS
	    // Threaded RTS:
	    // Copy the "arguments", which might include a return value,
	    // back to the TSO stack. It would of course be enough to
	    // just copy the return value, but we don't know the offset.
	    memcpy(Sp, arguments, sizeof(W_) * stk_offset);
#endif

	    goto nextInsn;
	}

	case bci_JMP: {
	    /* BCO_NEXT modifies bciPtr, so be conservative. */
	    int nextpc = BCO_NEXT;
	    bciPtr     = nextpc;
	    goto nextInsn;
	}

	case bci_CASEFAIL:
	    barf("interpretBCO: hit a CASEFAIL");
	    
	    // Errors
	default: 
	    barf("interpretBCO: unknown or unimplemented opcode");

	} /* switch on opcode */
    }
    }

    barf("interpretBCO: fell off end of the interpreter");
}
