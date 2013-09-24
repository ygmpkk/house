/* -----------------------------------------------------------------------------
 * $Id: PrimOps.hc,v 1.2 2004/11/29 21:08:57 hallgren Exp $
 *
 * (c) The GHC Team, 1998-2002
 *
 * Primitive functions / data
 *
 * ---------------------------------------------------------------------------*/

#include "Stg.h"
#include "Rts.h"

#include "RtsFlags.h"
#include "StgStartup.h"
#include "SchedAPI.h"
#include "Schedule.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "BlockAlloc.h" /* tmp */
#include "StablePriv.h"
#include "StgRun.h"
#include "Timer.h"      /* TICK_MILLISECS */
#include "Prelude.h"
#ifndef mingw32_TARGET_OS
#include "Itimer.h"    /* getourtimeofday() */
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#include <stdlib.h>

#ifdef mingw32_TARGET_OS
#include <windows.h>
#include "win32/AsyncIO.h"
#endif

/* ** temporary **

   classes CCallable and CReturnable don't really exist, but the
   compiler insists on generating dictionaries containing references
   to GHC_ZcCCallable_static_info etc., so we provide dummy symbols
   for these.  Some C compilers can't cope with zero-length static arrays,
   so we have to make these one element long.
*/

StgWord GHC_ZCCCallable_static_info[1];
StgWord GHC_ZCCReturnable_static_info[1];
  
/* -----------------------------------------------------------------------------
   Macros for Hand-written primitives.
   -------------------------------------------------------------------------- */

/*
 * Horrible macros for returning unboxed tuples.
 *
 * How an unboxed tuple is returned depends on two factors:
 *    - the number of real registers we have available
 *    - the boxedness of the returned fields.
 *
 * To return an unboxed tuple from a primitive operation, we have macros
 * RET_<layout> where <layout> describes the boxedness of each field of the
 * unboxed tuple:  N indicates a non-pointer field, and P indicates a pointer.
 *
 * We only define the cases actually used, to avoid having too much
 * garbage in this section.  Warning: any bugs in here will be hard to
 * track down.
 *
 * The return convention for an unboxed tuple is as follows:
 *   - fit as many fields as possible in registers (as per the
 *     function fast-entry point calling convention).
 *   - sort the rest of the fields into pointers and non-pointers.
 *     push the pointers on the stack, followed by the non-pointers.
 *     (so the pointers have higher addresses).
 */

/*------ All Regs available */
#if MAX_REAL_VANILLA_REG == 8
# define RET_P(a)     R1.w = (W_)(a); JMP_(ENTRY_CODE(Sp[0]));
# define RET_N(a)     RET_P(a)

# define RET_PP(a,b)  R1.w = (W_)(a); R2.w = (W_)(b); JMP_(ENTRY_CODE(Sp[0]));
# define RET_NN(a,b)  RET_PP(a,b)
# define RET_NP(a,b)  RET_PP(a,b)

# define RET_PPP(a,b,c) \
	R1.w = (W_)(a); R2.w = (W_)(b); R3.w = (W_)(c); JMP_(ENTRY_CODE(Sp[0]));
# define RET_NNP(a,b,c) RET_PPP(a,b,c)

# define RET_NNNP(a,b,c,d) \
        R1.w = (W_)(a); R2.w = (W_)(b); R3.w = (W_)(c); R4.w = (W_)d; \
        JMP_(ENTRY_CODE(Sp[0]));

# define RET_NPNP(a,b,c,d) \
        R1.w = (W_)(a); R2.w = (W_)(b); R3.w = (W_)(c); R4.w = (W_)(d); \
	JMP_(ENTRY_CODE(Sp[0]));

#elif MAX_REAL_VANILLA_REG > 2 && MAX_REAL_VANILLA_REG < 8
# error RET_n macros not defined for this setup.

/*------ 2 Registers available */
#elif MAX_REAL_VANILLA_REG == 2

# define RET_P(a)     R1.w = (W_)(a); JMP_(ENTRY_CODE(Sp[0]));
# define RET_N(a)     RET_P(a)

# define RET_PP(a,b)   R1.w = (W_)(a); R2.w = (W_)(b); \
		       JMP_(ENTRY_CODE(Sp[0]));
# define RET_NN(a,b)   RET_PP(a,b)
# define RET_NP(a,b)   RET_PP(a,b)

# define RET_PPP(a,b,c)				\
	R1.w = (W_)(a);				\
	R2.w = (W_)(b);				\
	Sp[-1] = (W_)(c);			\
	Sp -= 1;				\
	JMP_(ENTRY_CODE(Sp[1]));

# define RET_NNP(a,b,c)				\
	R1.w = (W_)(a);				\
	R2.w = (W_)(b);				\
	Sp[-1] = (W_)(c);			\
	Sp -= 1;				\
	JMP_(ENTRY_CODE(Sp[1]));

# define RET_NNNP(a,b,c,d)			\
	R1.w = (W_)(a); 			\
        R2.w = (W_)(b); 			\
        Sp[-2] = (W_)(c); 			\
        Sp[-1] = (W_)(d); 			\
        Sp -= 2;				\
        JMP_(ENTRY_CODE(Sp[2]));

# define RET_NPNP(a,b,c,d)			\
	R1.w = (W_)(a); 			\
        R2.w = (W_)(b); 			\
        Sp[-2] = (W_)(c); 			\
        Sp[-1] = (W_)(d); 			\
        Sp -= 2;				\
        JMP_(ENTRY_CODE(Sp[2]));

/*------ 1 Register available */
#elif MAX_REAL_VANILLA_REG == 1
# define RET_P(a)     R1.w = (W_)(a); JMP_(ENTRY_CODE(Sp[0]));
# define RET_N(a)     RET_P(a)

# define RET_PP(a,b)   R1.w = (W_)(a); Sp[-1] = (W_)(b); Sp -= 1; \
		       JMP_(ENTRY_CODE(Sp[1]));
# define RET_NN(a,b)   R1.w = (W_)(a); Sp[-1] = (W_)(b); Sp -= 2; \
		       JMP_(ENTRY_CODE(Sp[2]));
# define RET_NP(a,b)   RET_PP(a,b)

# define RET_PPP(a,b,c)				\
	R1.w = (W_)(a);				\
	Sp[-2] = (W_)(b);			\
	Sp[-1] = (W_)(c);			\
	Sp -= 2;				\
	JMP_(ENTRY_CODE(Sp[2]));

# define RET_NNP(a,b,c)				\
	R1.w = (W_)(a);				\
	Sp[-2] = (W_)(b);			\
	Sp[-1] = (W_)(c);			\
	Sp -= 2;				\
	JMP_(ENTRY_CODE(Sp[2]));

# define RET_NNNP(a,b,c,d)			\
	R1.w = (W_)(a); 			\
        Sp[-3] = (W_)(b); 			\
        Sp[-2] = (W_)(c); 			\
        Sp[-1] = (W_)(d); 			\
        Sp -= 3;				\
        JMP_(ENTRY_CODE(Sp[3]));

# define RET_NPNP(a,b,c,d)			\
	R1.w = (W_)(a); 			\
        Sp[-3] = (W_)(c); 			\
        Sp[-2] = (W_)(b); 			\
        Sp[-1] = (W_)(d); 			\
        Sp -= 3;				\
        JMP_(ENTRY_CODE(Sp[3]));

#else /* 0 Regs available */

#define PUSH(o,x) Sp[-o] = (W_)(x)

#define PUSHED(m)   Sp -= (m); JMP_(ENTRY_CODE(Sp[m]));

# define RET_P(a)     PUSH(1,a); PUSHED(1)
# define RET_N(a)     PUSH(1,a); PUSHED(1)

# define RET_PP(a,b)   PUSH(2,a); PUSH(1,b); PUSHED(2)
# define RET_NN(a,b)   PUSH(2,a); PUSH(1,b); PUSHED(2)
# define RET_NP(a,b)   PUSH(2,a); PUSH(1,b); PUSHED(2)

# define RET_PPP(a,b,c) PUSH(3,a); PUSH(2,b); PUSH(1,c); PUSHED(3)
# define RET_NNP(a,b,c) PUSH(3,a); PUSH(2,b); PUSH(1,c); PUSHED(3)

# define RET_NNNP(a,b,c,d) PUSH(4,a); PUSH(3,b); PUSH(2,c); PUSH(1,d); PUSHED(4)	
# define RET_NPNP(a,b,c,d) PUSH(4,a); PUSH(3,c); PUSH(2,b); PUSH(1,d); PUSHED(4)	
#endif

/*-----------------------------------------------------------------------------
  Array Primitives

  Basically just new*Array - the others are all inline macros.

  The size arg is always passed in R1, and the result returned in R1.

  The slow entry point is for returning from a heap check, the saved
  size argument must be re-loaded from the stack.
  -------------------------------------------------------------------------- */

/* for objects that are *less* than the size of a word, make sure we
 * round up to the nearest word for the size of the array.
 */

#define BYTES_TO_STGWORDS(n) ((n) + sizeof(W_) - 1)/sizeof(W_)

FN_(newByteArrayzh_fast)
 {
   W_ size, stuff_size, n;
   StgArrWords* p;
   FB_
     MAYBE_GC(NO_PTRS,newByteArrayzh_fast);
     n = R1.w;
     stuff_size = BYTES_TO_STGWORDS(n);
     size = sizeofW(StgArrWords)+ stuff_size;
     p = (StgArrWords *)RET_STGCALL1(P_,allocate,size);
     TICK_ALLOC_PRIM(sizeofW(StgArrWords),stuff_size,0);
     SET_HDR(p, &stg_ARR_WORDS_info, CCCS);
     p->words = stuff_size;
     TICK_RET_UNBOXED_TUP(1)
     RET_P(p);
   FE_
 }

FN_(newPinnedByteArrayzh_fast)
 {
   W_ size, stuff_size, n;
   StgArrWords* p;
   FB_
     MAYBE_GC(NO_PTRS,newPinnedByteArrayzh_fast);
     n = R1.w;
     stuff_size = BYTES_TO_STGWORDS(n);

     // We want an 8-byte aligned array.  allocatePinned() gives us
     // 8-byte aligned memory by default, but we want to align the
     // *goods* inside the ArrWords object, so we have to check the
     // size of the ArrWords header and adjust our size accordingly.
     size = sizeofW(StgArrWords)+ stuff_size;
     if ((sizeof(StgArrWords) & 7) != 0) {
	 size++;
     }

     p = (StgArrWords *)RET_STGCALL1(P_,allocatePinned,size);
     TICK_ALLOC_PRIM(sizeofW(StgArrWords),stuff_size,0);

     // Again, if the ArrWords header isn't a multiple of 8 bytes, we
     // have to push the object forward one word so that the goods
     // fall on an 8-byte boundary.
     if ((sizeof(StgArrWords) & 7) != 0) {
	 ((StgPtr)p)++;
     }

     SET_HDR(p, &stg_ARR_WORDS_info, CCCS);
     p->words = stuff_size;
     TICK_RET_UNBOXED_TUP(1)
     RET_P(p);
   FE_
 }

FN_(newArrayzh_fast)
{
  W_ size, n, init;
  StgMutArrPtrs* arr;
  StgPtr p;
  FB_
    n = R1.w;

    MAYBE_GC(R2_PTR,newArrayzh_fast);

    size = sizeofW(StgMutArrPtrs) + n;
    arr = (StgMutArrPtrs *)RET_STGCALL1(P_, allocate, size);
    TICK_ALLOC_PRIM(sizeofW(StgMutArrPtrs), n, 0);

    SET_HDR(arr,&stg_MUT_ARR_PTRS_info,CCCS);
    arr->ptrs = n;

    init = R2.w;
    for (p = (P_)arr + sizeofW(StgMutArrPtrs); 
	 p < (P_)arr + size; p++) {
	*p = (W_)init;
    }

    TICK_RET_UNBOXED_TUP(1);
    RET_P(arr);
  FE_
}

FN_(newMutVarzh_fast)
{
  StgMutVar* mv;
  /* Args: R1.p = initialisation value */
  FB_

  HP_CHK_GEN_TICKY(sizeofW(StgMutVar), R1_PTR, newMutVarzh_fast);
  TICK_ALLOC_PRIM(sizeofW(StgHeader)+1,1, 0); /* hack, dependent on rep. */
  CCS_ALLOC(CCCS,sizeofW(StgMutVar));

  mv = (StgMutVar *)(Hp-sizeofW(StgMutVar)+1);
  SET_HDR(mv,&stg_MUT_VAR_info,CCCS);
  mv->var = R1.cl;

  TICK_RET_UNBOXED_TUP(1);
  RET_P(mv);
  FE_
}

FN_(atomicModifyMutVarzh_fast)
{
   StgMutVar* mv;
   StgClosure *z, *x, *y, *r;
   FB_
   /* Args: R1.p :: MutVar#,  R2.p :: a -> (a,b) */

   /* If x is the current contents of the MutVar#, then 
      We want to make the new contents point to

         (sel_0 (f x))
 
      and the return value is

	 (sel_1 (f x))

      obviously we can share (f x).

         z = [stg_ap_2 f x]  (max (HS + 2) MIN_UPD_SIZE)
	 y = [stg_sel_0 z]   (max (HS + 1) MIN_UPD_SIZE)
         r = [stg_sel_1 z]   (max (HS + 1) MIN_UPD_SIZE)
   */

#define THUNK_SIZE(n) (sizeofW(StgHeader) + stg_max((n), MIN_UPD_SIZE))
#define SIZE (THUNK_SIZE(2) + THUNK_SIZE(1) + THUNK_SIZE(1))

   HP_CHK_GEN_TICKY(SIZE, R1_PTR|R2_PTR, atomicModifyMutVarzh_fast);
   CCS_ALLOC(CCCS,SIZE);

   x = ((StgMutVar *)R1.cl)->var;

   TICK_ALLOC_UP_THK(2,0); // XXX
   z = (StgClosure *) Hp - THUNK_SIZE(2) + 1;
   SET_HDR(z, (StgInfoTable *)&stg_ap_2_upd_info, CCCS);
   z->payload[0] = R2.cl;
   z->payload[1] = x;

   TICK_ALLOC_UP_THK(1,1); // XXX
   y = (StgClosure *) (StgPtr)z - THUNK_SIZE(1);
   SET_HDR(y, &stg_sel_0_upd_info, CCCS);
   y->payload[0] = z;

   ((StgMutVar *)R1.cl)->var = y;

   TICK_ALLOC_UP_THK(1,1); // XXX
   r = (StgClosure *) (StgPtr)y - THUNK_SIZE(1);
   SET_HDR(r, &stg_sel_1_upd_info, CCCS);
   r->payload[0] = z;

   RET_P(r);
   FE_
}

/* -----------------------------------------------------------------------------
   Foreign Object Primitives
   -------------------------------------------------------------------------- */

FN_(mkForeignObjzh_fast)
{
  /* R1.p = ptr to foreign object,
  */
  StgForeignObj *result;
  FB_

  HP_CHK_GEN_TICKY(sizeofW(StgForeignObj), NO_PTRS, mkForeignObjzh_fast);
  TICK_ALLOC_PRIM(sizeofW(StgHeader),
		  sizeofW(StgForeignObj)-sizeofW(StgHeader), 0);
  CCS_ALLOC(CCCS,sizeofW(StgForeignObj)); /* ccs prof */

  result = (StgForeignObj *) (Hp + 1 - sizeofW(StgForeignObj));
  SET_HDR(result,&stg_FOREIGN_info,CCCS);
  result->data = R1.p;

  /* returns (# s#, ForeignObj# #) */
  TICK_RET_UNBOXED_TUP(1);
  RET_P(result);
  FE_
}

/* These two are out-of-line for the benefit of the NCG */
FN_(unsafeThawArrayzh_fast)
{
  FB_
  SET_INFO((StgClosure *)R1.cl,&stg_MUT_ARR_PTRS_info);

  // SUBTLETY TO DO WITH THE OLD GEN MUTABLE LIST
  //
  // A MUT_ARR_PTRS lives on the mutable list, but a MUT_ARR_PTRS_FROZEN 
  // normally doesn't.  However, when we freeze a MUT_ARR_PTRS, we leave
  // it on the mutable list for the GC to remove (removing something from
  // the mutable list is not easy, because the mut_list is only singly-linked).
  // 
  // So, when we thaw a MUT_ARR_PTRS_FROZEN, we must cope with two cases:
  // either it is on a mut_list, or it isn't.  We adopt the convention that
  // the mut_link field is NULL if it isn't on a mut_list, and the GC
  // maintains this invariant.
  //
  if (((StgMutArrPtrs *)R1.cl)->mut_link == NULL) {
	recordMutable((StgMutClosure*)R1.cl);
  }

  TICK_RET_UNBOXED_TUP(1);
  RET_P(R1.p);
  FE_
}

/* -----------------------------------------------------------------------------
   Weak Pointer Primitives
   -------------------------------------------------------------------------- */

FN_(mkWeakzh_fast)
{
  /* R1.p = key
     R2.p = value
     R3.p = finalizer (or NULL)
  */
  StgWeak *w;
  FB_

  if (R3.cl == NULL) {
    R3.cl = &stg_NO_FINALIZER_closure;
  }

  HP_CHK_GEN_TICKY(sizeofW(StgWeak),R1_PTR|R2_PTR|R3_PTR, mkWeakzh_fast);
  TICK_ALLOC_PRIM(sizeofW(StgHeader)+1,  // +1 is for the link field
		  sizeofW(StgWeak)-sizeofW(StgHeader)-1, 0);
  CCS_ALLOC(CCCS,sizeofW(StgWeak)); /* ccs prof */

  w = (StgWeak *) (Hp + 1 - sizeofW(StgWeak));
  SET_HDR(w, &stg_WEAK_info, CCCS);

  w->key        = R1.cl;
  w->value      = R2.cl;
  w->finalizer  = R3.cl;

  w->link       = weak_ptr_list;
  weak_ptr_list = w;
  IF_DEBUG(weak, fprintf(stderr,"New weak pointer at %p\n",w));

  TICK_RET_UNBOXED_TUP(1);
  RET_P(w);
  FE_
}

FN_(finalizzeWeakzh_fast)
{
  /* R1.p = weak ptr
   */
  StgDeadWeak *w;
  StgClosure *f;
  FB_
  TICK_RET_UNBOXED_TUP(0);
  w = (StgDeadWeak *)R1.p;

  /* already dead? */
  if (w->header.info == &stg_DEAD_WEAK_info) {
      RET_NP(0,&stg_NO_FINALIZER_closure);
  }

  /* kill it */
#ifdef PROFILING
  // @LDV profiling
  // A weak pointer is inherently used, so we do not need to call
  // LDV_recordDead_FILL_SLOP_DYNAMIC():
  //    LDV_recordDead_FILL_SLOP_DYNAMIC((StgClosure *)w);
  // or, LDV_recordDead():
  //    LDV_recordDead((StgClosure *)w, sizeofW(StgWeak) - sizeofW(StgProfHeader));
  // Furthermore, when PROFILING is turned on, dead weak pointers are exactly as 
  // large as weak pointers, so there is no need to fill the slop, either.
  // See stg_DEAD_WEAK_info in StgMiscClosures.hc.
#endif
  //
  // Todo: maybe use SET_HDR() and remove LDV_recordCreate()?
  //
  w->header.info = &stg_DEAD_WEAK_info;
#ifdef PROFILING
  // @LDV profiling
  LDV_recordCreate((StgClosure *)w);
#endif
  f = ((StgWeak *)w)->finalizer;
  w->link = ((StgWeak *)w)->link;

  /* return the finalizer */
  if (f == &stg_NO_FINALIZER_closure) {
      RET_NP(0,&stg_NO_FINALIZER_closure);
  } else {
      RET_NP(1,f);
  }
  FE_
}

FN_(deRefWeakzh_fast)
{
  /* R1.p = weak ptr */
  StgWeak* w;
  I_       code;
  P_       val;
  FB_
  w = (StgWeak*)R1.p;
  if (w->header.info == &stg_WEAK_info) {
    code = 1;
    val = (P_)((StgWeak *)w)->value;
  } else {
    code = 0;
    val = (P_)w;
  }
  RET_NP(code,val);
  FE_
}

/* -----------------------------------------------------------------------------
   Arbitrary-precision Integer operations.
   -------------------------------------------------------------------------- */

FN_(int2Integerzh_fast)
{
   /* arguments: R1 = Int# */

   I_ val, s;  		/* to avoid aliasing */
   StgArrWords* p;	/* address of array result */
   FB_

   val = R1.i;
   HP_CHK_GEN_TICKY(sizeofW(StgArrWords)+1, NO_PTRS, int2Integerzh_fast);
   TICK_ALLOC_PRIM(sizeofW(StgArrWords),1,0);
   CCS_ALLOC(CCCS,sizeofW(StgArrWords)+1); /* ccs prof */

   p = (StgArrWords *)Hp - 1;
   SET_ARR_HDR(p, &stg_ARR_WORDS_info, CCCS, 1);

   /* mpz_set_si is inlined here, makes things simpler */
   if (val < 0) { 
	s  = -1;
	*Hp = -val;
   } else if (val > 0) {
	s = 1;
	*Hp = val;
   } else {
	s = 0;
   }

   /* returns (# size  :: Int#, 
		 data  :: ByteArray# 
	       #)
   */
   TICK_RET_UNBOXED_TUP(2);
   RET_NP(s,p);
   FE_
}

FN_(word2Integerzh_fast)
{
   /* arguments: R1 = Word# */

   W_ val;  		/* to avoid aliasing */
   I_  s;
   StgArrWords* p;	/* address of array result */
   FB_

   val = R1.w;
   HP_CHK_GEN_TICKY(sizeofW(StgArrWords)+1, NO_PTRS, word2Integerzh_fast)
   TICK_ALLOC_PRIM(sizeofW(StgArrWords),1,0);
   CCS_ALLOC(CCCS,sizeofW(StgArrWords)+1); /* ccs prof */

   p = (StgArrWords *)Hp - 1;
   SET_ARR_HDR(p, &stg_ARR_WORDS_info, CCCS, 1);

   if (val != 0) {
	s = 1;
	*Hp = val;
   } else {
	s = 0;
   }

   /* returns (# size  :: Int#, 
		 data  :: ByteArray# 
	       #)
   */
   TICK_RET_UNBOXED_TUP(2);
   RET_NP(s,p);
   FE_
}


/*
 * 'long long' primops for converting to/from Integers.
 */

#ifdef SUPPORT_LONG_LONGS

FN_(int64ToIntegerzh_fast)
{
   /* arguments: L1 = Int64# */

   StgInt64  val; /* to avoid aliasing */
   W_ hi;
   I_  s, neg, words_needed;
   StgArrWords* p;	/* address of array result */
   FB_

   val = (LI_)L1;
   neg = 0;

   if ( val >= 0x100000000LL || val <= -0x100000000LL )  { 
       words_needed = 2;
   } else { 
       /* minimum is one word */
       words_needed = 1;
   }
   HP_CHK_GEN_TICKY(sizeofW(StgArrWords)+words_needed, NO_PTRS, int64ToIntegerzh_fast)
   TICK_ALLOC_PRIM(sizeofW(StgArrWords),words_needed,0);
   CCS_ALLOC(CCCS,sizeofW(StgArrWords)+words_needed); /* ccs prof */

   p = (StgArrWords *)(Hp-words_needed+1) - 1;
   SET_ARR_HDR(p, &stg_ARR_WORDS_info, CCCS, words_needed);

   if ( val < 0LL ) {
     neg = 1;
     val = -val;
   }

   hi = (W_)((LW_)val / 0x100000000ULL);

   if ( words_needed == 2 )  { 
      s = 2;
      Hp[-1] = (W_)val;
      Hp[0] = hi;
   } else if ( val != 0 ) {
      s = 1;
      Hp[0] = (W_)val;
   }  else /* val==0 */   {
      s = 0;
   }
   s = ( neg ? -s : s );

   /* returns (# size  :: Int#, 
		 data  :: ByteArray# 
	       #)
   */
   TICK_RET_UNBOXED_TUP(2);
   RET_NP(s,p);
   FE_
}

FN_(word64ToIntegerzh_fast)
{
   /* arguments: L1 = Word64# */

   StgWord64 val; /* to avoid aliasing */
   StgWord hi;
   I_  s, words_needed;
   StgArrWords* p;	/* address of array result */
   FB_

   val = (LW_)L1;
   if ( val >= 0x100000000ULL ) {
      words_needed = 2;
   } else {
      words_needed = 1;
   }
   HP_CHK_GEN_TICKY(sizeofW(StgArrWords)+words_needed, NO_PTRS, word64ToIntegerzh_fast)
   TICK_ALLOC_PRIM(sizeofW(StgArrWords),words_needed,0);
   CCS_ALLOC(CCCS,sizeofW(StgArrWords)+words_needed); /* ccs prof */

   p = (StgArrWords *)(Hp-words_needed+1) - 1;
   SET_ARR_HDR(p, &stg_ARR_WORDS_info, CCCS, words_needed);

   hi = (W_)((LW_)val / 0x100000000ULL);
   if ( val >= 0x100000000ULL ) { 
     s = 2;
     Hp[-1] = ((W_)val);
     Hp[0]  = (hi);
   } else if ( val != 0 )      {
      s = 1;
      Hp[0] = ((W_)val);
   } else /* val==0 */         {
      s = 0;
   }

   /* returns (# size  :: Int#, 
		 data  :: ByteArray# 
	       #)
   */
   TICK_RET_UNBOXED_TUP(2);
   RET_NP(s,p);
   FE_
}


#endif /* SUPPORT_LONG_LONGS */

/* ToDo: this is shockingly inefficient */

#define GMP_TAKE2_RET1(name,mp_fun)					\
FN_(name)								\
{									\
  MP_INT arg1, arg2, result;						\
  I_ s1, s2;								\
  StgArrWords* d1;							\
  StgArrWords* d2;							\
  FB_									\
 									\
  /* call doYouWantToGC() */						\
  MAYBE_GC(R2_PTR | R4_PTR, name);					\
									\
  d1 = (StgArrWords *)R2.p;						\
  s1 = R1.i;								\
  d2 = (StgArrWords *)R4.p;						\
  s2 = R3.i;								\
									\
  arg1._mp_alloc	= d1->words;					\
  arg1._mp_size		= (s1);						\
  arg1._mp_d		= (unsigned long int *) (BYTE_ARR_CTS(d1));	\
  arg2._mp_alloc	= d2->words;					\
  arg2._mp_size		= (s2);						\
  arg2._mp_d		= (unsigned long int *) (BYTE_ARR_CTS(d2));	\
									\
  STGCALL1(mpz_init,&result);						\
									\
  /* Perform the operation */						\
  STGCALL3(mp_fun,&result,&arg1,&arg2);					\
									\
  TICK_RET_UNBOXED_TUP(2);						\
  RET_NP(result._mp_size, 						\
         result._mp_d-sizeofW(StgArrWords));				\
  FE_									\
}

#define GMP_TAKE1_RET1(name,mp_fun)					\
FN_(name)								\
{									\
  MP_INT arg1, result;							\
  I_ s1;								\
  StgArrWords* d1;							\
  FB_									\
									\
  /* call doYouWantToGC() */						\
  MAYBE_GC(R2_PTR, name);						\
									\
  d1 = (StgArrWords *)R2.p;						\
  s1 = R1.i;								\
									\
  arg1._mp_alloc	= d1->words;					\
  arg1._mp_size		= (s1);						\
  arg1._mp_d		= (unsigned long int *) (BYTE_ARR_CTS(d1));	\
									\
  STGCALL1(mpz_init,&result);						\
									\
  /* Perform the operation */						\
  STGCALL2(mp_fun,&result,&arg1);					\
									\
  TICK_RET_UNBOXED_TUP(2);						\
  RET_NP(result._mp_size,						\
         result._mp_d-sizeofW(StgArrWords));				\
  FE_									\
}

#define GMP_TAKE2_RET2(name,mp_fun)					\
FN_(name)								\
{									\
  MP_INT arg1, arg2, result1, result2;					\
  I_ s1, s2;								\
  StgArrWords* d1;							\
  StgArrWords* d2;							\
  FB_									\
 									\
  /* call doYouWantToGC() */						\
  MAYBE_GC(R2_PTR | R4_PTR, name);					\
									\
  d1 = (StgArrWords *)R2.p;						\
  s1 = R1.i;								\
  d2 = (StgArrWords *)R4.p;						\
  s2 = R3.i;								\
									\
  arg1._mp_alloc	= d1->words;					\
  arg1._mp_size		= (s1);						\
  arg1._mp_d		= (unsigned long int *) (BYTE_ARR_CTS(d1));	\
  arg2._mp_alloc	= d2->words;					\
  arg2._mp_size		= (s2);						\
  arg2._mp_d		= (unsigned long int *) (BYTE_ARR_CTS(d2));	\
									\
  STGCALL1(mpz_init,&result1);						\
  STGCALL1(mpz_init,&result2);						\
									\
  /* Perform the operation */						\
  STGCALL4(mp_fun,&result1,&result2,&arg1,&arg2);			\
									\
  TICK_RET_UNBOXED_TUP(4);						\
  RET_NPNP(result1._mp_size, 						\
           result1._mp_d-sizeofW(StgArrWords),				\
	   result2._mp_size, 						\
           result2._mp_d-sizeofW(StgArrWords));				\
  FE_									\
}

GMP_TAKE2_RET1(plusIntegerzh_fast,     mpz_add);
GMP_TAKE2_RET1(minusIntegerzh_fast,    mpz_sub);
GMP_TAKE2_RET1(timesIntegerzh_fast,    mpz_mul);
GMP_TAKE2_RET1(gcdIntegerzh_fast,      mpz_gcd);
GMP_TAKE2_RET1(quotIntegerzh_fast,     mpz_tdiv_q);
GMP_TAKE2_RET1(remIntegerzh_fast,      mpz_tdiv_r);
GMP_TAKE2_RET1(divExactIntegerzh_fast, mpz_divexact);
GMP_TAKE2_RET1(andIntegerzh_fast,      mpz_and);
GMP_TAKE2_RET1(orIntegerzh_fast,       mpz_ior);
GMP_TAKE2_RET1(xorIntegerzh_fast,      mpz_xor);
GMP_TAKE1_RET1(complementIntegerzh_fast, mpz_com);

GMP_TAKE2_RET2(quotRemIntegerzh_fast, mpz_tdiv_qr);
GMP_TAKE2_RET2(divModIntegerzh_fast,  mpz_fdiv_qr);


FN_(gcdIntzh_fast)
{
  /* R1 = the first Int#; R2 = the second Int# */
  mp_limb_t aa;
  I_ r;
  FB_
  aa = (mp_limb_t)(R1.i);
  r = RET_STGCALL3(StgInt, mpn_gcd_1, (mp_limb_t *)(&aa), 1, (mp_limb_t)(R2.i));

  R1.i = r;
  /* Result parked in R1, return via info-pointer at TOS */
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(gcdIntegerIntzh_fast)
{
  /* R1 = s1; R2 = d1; R3 = the int */
  I_ r;
  FB_
  r = RET_STGCALL3(StgInt,mpn_gcd_1,(mp_limb_t *)(BYTE_ARR_CTS(R2.p)), R1.i, R3.i);

  R1.i = r;
  /* Result parked in R1, return via info-pointer at TOS */
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(cmpIntegerIntzh_fast)
{
  /* R1 = s1; R2 = d1; R3 = the int */
  I_ usize;
  I_ vsize;
  I_ v_digit;
  mp_limb_t u_digit;
  FB_

  usize = R1.i;
  vsize = 0;
  v_digit = R3.i;

  // paraphrased from mpz_cmp_si() in the GMP sources
  if (v_digit > 0) {
      vsize = 1;
  } else if (v_digit < 0) {
      vsize = -1;
      v_digit = -v_digit;
  }

  if (usize != vsize) {
    R1.i = usize - vsize; JMP_(ENTRY_CODE(Sp[0]));
  }

  if (usize == 0) {
    R1.i = 0; JMP_(ENTRY_CODE(Sp[0]));
  }

  u_digit = *(mp_limb_t *)(BYTE_ARR_CTS(R2.p));

  if (u_digit == (mp_limb_t) (unsigned long) v_digit) {
    R1.i = 0; JMP_(ENTRY_CODE(Sp[0]));
  }

  if (u_digit > (mp_limb_t) (unsigned long) v_digit) {
    R1.i = usize; 
  } else {
    R1.i = -usize; 
  }

  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(cmpIntegerzh_fast)
{
  /* R1 = s1; R2 = d1; R3 = s2; R4 = d2 */
  I_ usize;
  I_ vsize;
  I_ size;
  StgPtr up, vp;
  int cmp;
  FB_

  // paraphrased from mpz_cmp() in the GMP sources
  usize = R1.i;
  vsize = R3.i;

  if (usize != vsize) {
    R1.i = usize - vsize; JMP_(ENTRY_CODE(Sp[0]));
  }

  if (usize == 0) {
    R1.i = 0; JMP_(ENTRY_CODE(Sp[0]));
  }

  size = abs(usize);

  up = BYTE_ARR_CTS(R2.p);
  vp = BYTE_ARR_CTS(R4.p);

  cmp = RET_STGCALL3(I_, mpn_cmp, (mp_limb_t *)up, (mp_limb_t *)vp, size);

  if (cmp == 0) {
    R1.i = 0; JMP_(ENTRY_CODE(Sp[0]));
  }

  if ((cmp < 0) == (usize < 0)) {
    R1.i = 1;
  } else {
    R1.i = (-1); 
  }
  /* Result parked in R1, return via info-pointer at TOS */
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(integer2Intzh_fast)
{
  /* R1 = s; R2 = d */
  I_ r, s;
  FB_
  s = R1.i;
  if (s == 0)
    r = 0;
  else {
    r = ((mp_limb_t *) (BYTE_ARR_CTS(R2.p)))[0];
    if (s < 0) r = -r;
  }
  /* Result parked in R1, return via info-pointer at TOS */
  R1.i = r;
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(integer2Wordzh_fast)
{
  /* R1 = s; R2 = d */
  I_ s;
  W_ r;
  FB_
  s = R1.i;
  if (s == 0)
    r = 0;
  else {
    r = ((mp_limb_t *) (BYTE_ARR_CTS(R2.p)))[0];
    if (s < 0) r = -r;
  }
  /* Result parked in R1, return via info-pointer at TOS */
  R1.w = r;
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}


FN_(decodeFloatzh_fast)
{ 
  MP_INT mantissa;
  I_ exponent;
  StgArrWords* p;
  StgFloat arg;
  FB_

  /* arguments: F1 = Float# */
  arg = F1;

  HP_CHK_GEN_TICKY(sizeofW(StgArrWords)+1, NO_PTRS, decodeFloatzh_fast);
  TICK_ALLOC_PRIM(sizeofW(StgArrWords),1,0);
  CCS_ALLOC(CCCS,sizeofW(StgArrWords)+1); /* ccs prof */

  /* Be prepared to tell Lennart-coded __decodeFloat	*/
  /* where mantissa._mp_d can be put (it does not care about the rest) */
  p = (StgArrWords *)Hp - 1;
  SET_ARR_HDR(p,&stg_ARR_WORDS_info,CCCS,1)
  mantissa._mp_d = (void *)BYTE_ARR_CTS(p);

  /* Perform the operation */
  STGCALL3(__decodeFloat,&mantissa,&exponent,arg);

  /* returns: (Int# (expn), Int#, ByteArray#) */
  TICK_RET_UNBOXED_TUP(3);
  RET_NNP(exponent,mantissa._mp_size,p);
  FE_
}

#define DOUBLE_MANTISSA_SIZE (sizeofW(StgDouble))
#define ARR_SIZE (sizeofW(StgArrWords) + DOUBLE_MANTISSA_SIZE)

FN_(decodeDoublezh_fast)
{ MP_INT mantissa;
  I_ exponent;
  StgDouble arg;
  StgArrWords* p;
  FB_

  /* arguments: D1 = Double# */
  arg = D1;

  HP_CHK_GEN_TICKY(ARR_SIZE, NO_PTRS, decodeDoublezh_fast);
  TICK_ALLOC_PRIM(sizeofW(StgArrWords),DOUBLE_MANTISSA_SIZE,0);
  CCS_ALLOC(CCCS,ARR_SIZE); /* ccs prof */

  /* Be prepared to tell Lennart-coded __decodeDouble	*/
  /* where mantissa.d can be put (it does not care about the rest) */
  p = (StgArrWords *)(Hp-ARR_SIZE+1);
  SET_ARR_HDR(p, &stg_ARR_WORDS_info, CCCS, DOUBLE_MANTISSA_SIZE);
  mantissa._mp_d = (void *)BYTE_ARR_CTS(p);

  /* Perform the operation */
  STGCALL3(__decodeDouble,&mantissa,&exponent,arg);

  /* returns: (Int# (expn), Int#, ByteArray#) */
  TICK_RET_UNBOXED_TUP(3);
  RET_NNP(exponent,mantissa._mp_size,p);
  FE_
}

/* -----------------------------------------------------------------------------
 * Concurrency primitives
 * -------------------------------------------------------------------------- */

FN_(forkzh_fast)
{
  FB_
  /* args: R1 = closure to spark */
  
  MAYBE_GC(R1_PTR, forkzh_fast);

  /* create it right now, return ThreadID in R1 */
  R1.t = RET_STGCALL2(StgTSO *, createIOThread, 
		     RtsFlags.GcFlags.initialStkSize, R1.cl);
  STGCALL1(scheduleThread, R1.t);
      
  /* switch at the earliest opportunity */ 
  context_switch = 1;
  
  RET_P(R1.t);
  FE_
}

FN_(yieldzh_fast)
{
  FB_
  JMP_(stg_yield_noregs);
  FE_
}

FN_(myThreadIdzh_fast)
{
  /* no args. */
  FB_
  RET_P((P_)CurrentTSO);
  FE_
}

FN_(labelThreadzh_fast)
{
  FB_
  /* args: 
	R1.p = ThreadId#
	R2.p = Addr# */
#ifdef DEBUG
  STGCALL2(labelThread,R1.p,(char *)R2.p);
#endif
  JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(isCurrentThreadBoundzh_fast)
{
  /* no args */
  I_ r;
  FB_
  r = (I_)(RET_STGCALL1(StgBool, isThreadBound, CurrentTSO));
  RET_N(r);
  FE_
}

/* -----------------------------------------------------------------------------
 * MVar primitives
 *
 * take & putMVar work as follows.  Firstly, an important invariant:
 *
 *    If the MVar is full, then the blocking queue contains only
 *    threads blocked on putMVar, and if the MVar is empty then the
 *    blocking queue contains only threads blocked on takeMVar.
 *
 * takeMvar:
 *    MVar empty : then add ourselves to the blocking queue
 *    MVar full  : remove the value from the MVar, and
 *                 blocking queue empty     : return
 *                 blocking queue non-empty : perform the first blocked putMVar
 *                                            from the queue, and wake up the
 *                                            thread (MVar is now full again)
 *
 * putMVar is just the dual of the above algorithm.
 *
 * How do we "perform a putMVar"?  Well, we have to fiddle around with
 * the stack of the thread waiting to do the putMVar.  See
 * stg_block_putmvar and stg_block_takemvar in HeapStackCheck.c for
 * the stack layout, and the PerformPut and PerformTake macros below.
 *
 * It is important that a blocked take or put is woken up with the
 * take/put already performed, because otherwise there would be a
 * small window of vulnerability where the thread could receive an
 * exception and never perform its take or put, and we'd end up with a
 * deadlock.
 *
 * -------------------------------------------------------------------------- */

FN_(isEmptyMVarzh_fast)
{
  /* args: R1 = MVar closure */
  I_ r;
  FB_
  r = (I_)((GET_INFO((StgMVar*)(R1.p))) == &stg_EMPTY_MVAR_info);
  RET_N(r);
  FE_
}


FN_(newMVarzh_fast)
{
  StgMVar *mvar;

  FB_
  /* args: none */

  HP_CHK_GEN_TICKY(sizeofW(StgMVar), NO_PTRS, newMVarzh_fast);
  TICK_ALLOC_PRIM(sizeofW(StgMutVar)-1, // consider head,tail,link as admin wds
	 	  1, 0);
  CCS_ALLOC(CCCS,sizeofW(StgMVar)); /* ccs prof */
  
  mvar = (StgMVar *) (Hp - sizeofW(StgMVar) + 1);
  SET_HDR(mvar,&stg_EMPTY_MVAR_info,CCCS);
  mvar->head = mvar->tail = (StgTSO *)&stg_END_TSO_QUEUE_closure;
  mvar->value = (StgClosure *)&stg_END_TSO_QUEUE_closure;

  TICK_RET_UNBOXED_TUP(1);
  RET_P(mvar);
  FE_
}

/* If R1 isn't available, pass it on the stack */
#ifdef REG_R1
#define PerformTake(tso, value) ({		\
    (tso)->sp[1] = (W_)value;			\
    (tso)->sp[0] = (W_)&stg_gc_unpt_r1_info;	\
  })
#else
#define PerformTake(tso, value) ({		\
    (tso)->sp[1] = (W_)value;			\
    (tso)->sp[0] = (W_)&stg_ut_1_0_unreg_info;	\
  })
#endif


#define PerformPut(tso) ({				\
    StgClosure *val = (StgClosure *)(tso)->sp[2];	\
    (tso)->sp += 3;					\
    val;						\
  })

FN_(takeMVarzh_fast)
{
  StgMVar *mvar;
  StgClosure *val;
  const StgInfoTable *info;

  FB_
  /* args: R1 = MVar closure */

  mvar = (StgMVar *)R1.p;

#ifdef SMP
  info = LOCK_CLOSURE(mvar);
#else
  info = GET_INFO(mvar);
#endif

  /* If the MVar is empty, put ourselves on its blocking queue,
   * and wait until we're woken up.
   */
  if (info == &stg_EMPTY_MVAR_info) {
    if (mvar->head == (StgTSO *)&stg_END_TSO_QUEUE_closure) {
      mvar->head = CurrentTSO;
    } else {
      mvar->tail->link = CurrentTSO;
    }
    CurrentTSO->link = (StgTSO *)&stg_END_TSO_QUEUE_closure;
    CurrentTSO->why_blocked = BlockedOnMVar;
    CurrentTSO->block_info.closure = (StgClosure *)mvar;
    mvar->tail = CurrentTSO;

#ifdef SMP
    /* unlock the MVar */
    mvar->header.info = &stg_EMPTY_MVAR_info;
#endif
    JMP_(stg_block_takemvar);
  }

  /* we got the value... */
  val = mvar->value;

  if (mvar->head != (StgTSO *)&stg_END_TSO_QUEUE_closure) {
      /* There are putMVar(s) waiting... 
       * wake up the first thread on the queue
       */
      ASSERT(mvar->head->why_blocked == BlockedOnMVar);

      /* actually perform the putMVar for the thread that we just woke up */
      mvar->value = PerformPut(mvar->head);

#if defined(GRAN) || defined(PAR)
      /* ToDo: check 2nd arg (mvar) is right */
      mvar->head = RET_STGCALL2(StgTSO *,unblockOne,mvar->head,mvar);
#else
      mvar->head = RET_STGCALL1(StgTSO *,unblockOne,mvar->head);
#endif
      if (mvar->head == (StgTSO *)&stg_END_TSO_QUEUE_closure) {
	  mvar->tail = (StgTSO *)&stg_END_TSO_QUEUE_closure;
      }
#ifdef SMP
      /* unlock in the SMP case */
      SET_INFO(mvar,&stg_FULL_MVAR_info);
#endif
      TICK_RET_UNBOXED_TUP(1);
      RET_P(val);
  } else {
      /* No further putMVars, MVar is now empty */

      /* do this last... we might have locked the MVar in the SMP case,
       * and writing the info pointer will unlock it.
       */
      SET_INFO(mvar,&stg_EMPTY_MVAR_info);
      mvar->value = (StgClosure *)&stg_END_TSO_QUEUE_closure;
      TICK_RET_UNBOXED_TUP(1);
      RET_P(val);
  }
  FE_
}

FN_(tryTakeMVarzh_fast)
{
  StgMVar *mvar;
  StgClosure *val;
  const StgInfoTable *info;

  FB_
  /* args: R1 = MVar closure */

  mvar = (StgMVar *)R1.p;

#ifdef SMP
  info = LOCK_CLOSURE(mvar);
#else
  info = GET_INFO(mvar);
#endif

  if (info == &stg_EMPTY_MVAR_info) {

#ifdef SMP
      /* unlock the MVar */
      SET_INFO(mvar,&stg_EMPTY_MVAR_info);
#endif

      /* HACK: we need a pointer to pass back, 
       * so we abuse NO_FINALIZER_closure
       */
      RET_NP(0, &stg_NO_FINALIZER_closure);
  }

  /* we got the value... */
  val = mvar->value;

  if (mvar->head != (StgTSO *)&stg_END_TSO_QUEUE_closure) {
      /* There are putMVar(s) waiting... 
       * wake up the first thread on the queue
       */
      ASSERT(mvar->head->why_blocked == BlockedOnMVar);

      /* actually perform the putMVar for the thread that we just woke up */
      mvar->value = PerformPut(mvar->head);

#if defined(GRAN) || defined(PAR)
      /* ToDo: check 2nd arg (mvar) is right */
      mvar->head = RET_STGCALL2(StgTSO *,unblockOne,mvar->head,mvar);
#else
      mvar->head = RET_STGCALL1(StgTSO *,unblockOne,mvar->head);
#endif
      if (mvar->head == (StgTSO *)&stg_END_TSO_QUEUE_closure) {
	  mvar->tail = (StgTSO *)&stg_END_TSO_QUEUE_closure;
      }
#ifdef SMP
      /* unlock in the SMP case */
      SET_INFO(mvar,&stg_FULL_MVAR_info);
#endif
  } else {
      /* No further putMVars, MVar is now empty */
      mvar->value = (StgClosure *)&stg_END_TSO_QUEUE_closure;

      /* do this last... we might have locked the MVar in the SMP case,
       * and writing the info pointer will unlock it.
       */
      SET_INFO(mvar,&stg_EMPTY_MVAR_info);
  }

  TICK_RET_UNBOXED_TUP(1);
  RET_NP((I_)1, val);
  FE_
}

FN_(putMVarzh_fast)
{
  StgMVar *mvar;
  const StgInfoTable *info;

  FB_
  /* args: R1 = MVar, R2 = value */

  mvar = (StgMVar *)R1.p;

#ifdef SMP
  info = LOCK_CLOSURE(mvar);
#else
  info = GET_INFO(mvar);
#endif

  if (info == &stg_FULL_MVAR_info) {
    if (mvar->head == (StgTSO *)&stg_END_TSO_QUEUE_closure) {
      mvar->head = CurrentTSO;
    } else {
      mvar->tail->link = CurrentTSO;
    }
    CurrentTSO->link = (StgTSO *)&stg_END_TSO_QUEUE_closure;
    CurrentTSO->why_blocked = BlockedOnMVar;
    CurrentTSO->block_info.closure = (StgClosure *)mvar;
    mvar->tail = CurrentTSO;

#ifdef SMP
    /* unlock the MVar */
    SET_INFO(mvar,&stg_FULL_MVAR_info);
#endif
    JMP_(stg_block_putmvar);
  }
  
  if (mvar->head != (StgTSO *)&stg_END_TSO_QUEUE_closure) {
      /* There are takeMVar(s) waiting: wake up the first one
       */
      ASSERT(mvar->head->why_blocked == BlockedOnMVar);

      /* actually perform the takeMVar */
      PerformTake(mvar->head, R2.cl);
      
#if defined(GRAN) || defined(PAR)
      /* ToDo: check 2nd arg (mvar) is right */
      mvar->head = RET_STGCALL2(StgTSO *,unblockOne,mvar->head,mvar);
#else
      mvar->head = RET_STGCALL1(StgTSO *,unblockOne,mvar->head);
#endif
      if (mvar->head == (StgTSO *)&stg_END_TSO_QUEUE_closure) {
	  mvar->tail = (StgTSO *)&stg_END_TSO_QUEUE_closure;
      }
#ifdef SMP
      /* unlocks the MVar in the SMP case */
      SET_INFO(mvar,&stg_EMPTY_MVAR_info);
#endif
      JMP_(ENTRY_CODE(Sp[0]));
  } else {
      /* No further takes, the MVar is now full. */
      mvar->value = R2.cl;
      /* unlocks the MVar in the SMP case */
      SET_INFO(mvar,&stg_FULL_MVAR_info);
      JMP_(ENTRY_CODE(Sp[0]));
  }

  /* ToDo: yield afterward for better communication performance? */
  FE_
}

FN_(tryPutMVarzh_fast)
{
  StgMVar *mvar;
  const StgInfoTable *info;

  FB_
  /* args: R1 = MVar, R2 = value */

  mvar = (StgMVar *)R1.p;

#ifdef SMP
  info = LOCK_CLOSURE(mvar);
#else
  info = GET_INFO(mvar);
#endif

  if (info == &stg_FULL_MVAR_info) {

#ifdef SMP
    /* unlock the MVar */
    mvar->header.info = &stg_FULL_MVAR_info;
#endif

    RET_N(0);
  }
  
  if (mvar->head != (StgTSO *)&stg_END_TSO_QUEUE_closure) {
      /* There are takeMVar(s) waiting: wake up the first one
       */
      ASSERT(mvar->head->why_blocked == BlockedOnMVar);

      /* actually perform the takeMVar */
      PerformTake(mvar->head, R2.cl);
      
#if defined(GRAN) || defined(PAR)
      /* ToDo: check 2nd arg (mvar) is right */
      mvar->head = RET_STGCALL2(StgTSO *,unblockOne,mvar->head,mvar);
#else
      mvar->head = RET_STGCALL1(StgTSO *,unblockOne,mvar->head);
#endif
      if (mvar->head == (StgTSO *)&stg_END_TSO_QUEUE_closure) {
	  mvar->tail = (StgTSO *)&stg_END_TSO_QUEUE_closure;
      }
#ifdef SMP
      /* unlocks the MVar in the SMP case */
      SET_INFO(mvar,&stg_EMPTY_MVAR_info);
#endif
      JMP_(ENTRY_CODE(Sp[0]));
  } else {
      /* No further takes, the MVar is now full. */
      mvar->value = R2.cl;
      /* unlocks the MVar in the SMP case */
      SET_INFO(mvar,&stg_FULL_MVAR_info);
      JMP_(ENTRY_CODE(Sp[0]));
  }

  /* ToDo: yield afterward for better communication performance? */
  FE_
}

/* -----------------------------------------------------------------------------
   Stable pointer primitives
   -------------------------------------------------------------------------  */

FN_(makeStableNamezh_fast)
{
  StgWord index;
  StgStableName *sn_obj;
  FB_

  HP_CHK_GEN_TICKY(sizeofW(StgStableName), R1_PTR, makeStableNamezh_fast);
  TICK_ALLOC_PRIM(sizeofW(StgHeader), 
		  sizeofW(StgStableName)-sizeofW(StgHeader), 0);
  CCS_ALLOC(CCCS,sizeofW(StgStableName)); /* ccs prof */
  
  index = RET_STGCALL1(StgWord,lookupStableName,R1.p);

  /* Is there already a StableName for this heap object? */
  if (stable_ptr_table[index].sn_obj == NULL) {
    sn_obj = (StgStableName *) (Hp - sizeofW(StgStableName) + 1);
    SET_HDR(sn_obj,&stg_STABLE_NAME_info,CCCS);
    sn_obj->sn = index;
    stable_ptr_table[index].sn_obj = (StgClosure *)sn_obj;
  } else {
    (StgClosure *)sn_obj = stable_ptr_table[index].sn_obj;
  }

  TICK_RET_UNBOXED_TUP(1);
  RET_P(sn_obj);
}


FN_(makeStablePtrzh_fast)
{
  /* Args: R1 = a */
  StgStablePtr sp;
  FB_
  MAYBE_GC(R1_PTR, makeStablePtrzh_fast);
  sp = RET_STGCALL1(StgStablePtr,getStablePtr,R1.p);
  RET_N(sp);
  FE_
}

FN_(deRefStablePtrzh_fast)
{
  /* Args: R1 = the stable ptr */
  P_ r;
  StgStablePtr sp;
  FB_
  sp = (StgStablePtr)R1.w;
  r = stable_ptr_table[(StgWord)sp].addr;
  RET_P(r);
  FE_
}

/* -----------------------------------------------------------------------------
   Bytecode object primitives
   -------------------------------------------------------------------------  */

FN_(newBCOzh_fast)
{
  /* R1.p = instrs
     R2.p = literals
     R3.p = ptrs
     R4.p = itbls
     R5.i = arity
     R6.p = bitmap array
  */
  StgBCO *bco;
  nat size;
  StgArrWords *bitmap_arr;
  FB_

  bitmap_arr = (StgArrWords *)R6.cl;
  size = sizeofW(StgBCO) + bitmap_arr->words;
  HP_CHK_GEN_TICKY(size,R1_PTR|R2_PTR|R3_PTR|R4_PTR|R6_PTR, newBCOzh_fast);
  TICK_ALLOC_PRIM(size, size-sizeofW(StgHeader), 0);
  CCS_ALLOC(CCCS,size); /* ccs prof */
  bco = (StgBCO *) (Hp + 1 - size);
  SET_HDR(bco, (const StgInfoTable *)&stg_BCO_info, CCCS);

  bco->instrs     = (StgArrWords*)R1.cl;
  bco->literals   = (StgArrWords*)R2.cl;
  bco->ptrs       = (StgMutArrPtrs*)R3.cl;
  bco->itbls      = (StgArrWords*)R4.cl;
  bco->arity      = R5.w;
  bco->size       = size;

  // Copy the arity/bitmap info into the BCO
  { 
    int i;
    for (i = 0; i < bitmap_arr->words; i++) {
	bco->bitmap[i] = bitmap_arr->payload[i];
    }
  }

  TICK_RET_UNBOXED_TUP(1);
  RET_P(bco);
  FE_
}

FN_(mkApUpd0zh_fast)
{
  // R1.p = the BCO# for the AP
  //
  StgPAP* ap;
  FB_

  // This function is *only* used to wrap zero-arity BCOs in an
  // updatable wrapper (see ByteCodeLink.lhs).  An AP thunk is always
  // saturated and always points directly to a FUN or BCO.
  ASSERT(get_itbl(R1.cl)->type == BCO && ((StgBCO *)R1.p)->arity == 0);

  HP_CHK_GEN_TICKY(PAP_sizeW(0), R1_PTR, mkApUpd0zh_fast);
  TICK_ALLOC_PRIM(sizeofW(StgHeader), PAP_sizeW(0)-sizeofW(StgHeader), 0);
  CCS_ALLOC(CCCS,PAP_sizeW(0)); /* ccs prof */
  ap = (StgPAP *) (Hp + 1 - PAP_sizeW(0));
  SET_HDR(ap, &stg_AP_info, CCCS);

  ap->n_args = 0;
  ap->fun = R1.cl;

  TICK_RET_UNBOXED_TUP(1);
  RET_P(ap);
  FE_
}

/* -----------------------------------------------------------------------------
   Thread I/O blocking primitives
   -------------------------------------------------------------------------- */

FN_(waitReadzh_fast)
{
  FB_
    /* args: R1.i */
    ASSERT(CurrentTSO->why_blocked == NotBlocked);
    CurrentTSO->why_blocked = BlockedOnRead;
    CurrentTSO->block_info.fd = R1.i;
    ACQUIRE_LOCK(&sched_mutex);
    APPEND_TO_BLOCKED_QUEUE(CurrentTSO);
    RELEASE_LOCK(&sched_mutex);
    JMP_(stg_block_noregs);
  FE_
}

FN_(waitWritezh_fast)
{
  FB_
    /* args: R1.i */
    ASSERT(CurrentTSO->why_blocked == NotBlocked);
    CurrentTSO->why_blocked = BlockedOnWrite;
    CurrentTSO->block_info.fd = R1.i;
    ACQUIRE_LOCK(&sched_mutex);
    APPEND_TO_BLOCKED_QUEUE(CurrentTSO);
    RELEASE_LOCK(&sched_mutex);
    JMP_(stg_block_noregs);
  FE_
}

FN_(delayzh_fast)
{
#ifdef mingw32_TARGET_OS
  StgAsyncIOResult* ares;
  unsigned int reqID;
#else
  StgTSO *t, *prev;
  nat target;
#endif
  FB_
    /* args: R1.i */
    ASSERT(CurrentTSO->why_blocked == NotBlocked);
    CurrentTSO->why_blocked = BlockedOnDelay;

    ACQUIRE_LOCK(&sched_mutex);
#ifdef mingw32_TARGET_OS
    /* could probably allocate this on the heap instead */
    ares = (StgAsyncIOResult*)RET_STGCALL2(P_,stgMallocBytes,sizeof(StgAsyncIOResult), "delayzh_fast");
    reqID = RET_STGCALL1(W_,addDelayRequest,R1.i);
    ares->reqID   = reqID;
    ares->len     = 0;
    ares->errCode = 0;
    CurrentTSO->block_info.async_result = ares;
    /* Having all async-blocked threads reside on the blocked_queue simplifies matters, so
     * change the status to OnDoProc & put the delayed thread on the blocked_queue.
     */
    CurrentTSO->why_blocked = BlockedOnDoProc;
    APPEND_TO_BLOCKED_QUEUE(CurrentTSO);
#else
    target = (R1.i / (TICK_MILLISECS*1000)) + getourtimeofday();
    CurrentTSO->block_info.target = target;

    /* Insert the new thread in the sleeping queue. */
    prev = NULL;
    t = sleeping_queue;
    while (t != END_TSO_QUEUE && t->block_info.target < target) {
	prev = t;
	t = t->link;
    }

    CurrentTSO->link = t;
    if (prev == NULL) {
	sleeping_queue = CurrentTSO;
    } else {
	prev->link = CurrentTSO;
    }
#endif
    RELEASE_LOCK(&sched_mutex);
    JMP_(stg_block_noregs);
  FE_
}

#ifdef mingw32_TARGET_OS
FN_(asyncReadzh_fast)
{
  StgAsyncIOResult* ares;
  unsigned int reqID;
  FB_
    /* args: R1.i = fd, R2.i = isSock, R3.i = len, R4.p = buf */
    ASSERT(CurrentTSO->why_blocked == NotBlocked);
    CurrentTSO->why_blocked = BlockedOnRead;
    ACQUIRE_LOCK(&sched_mutex);
    /* could probably allocate this on the heap instead */
    ares = (StgAsyncIOResult*)RET_STGCALL2(P_,stgMallocBytes,sizeof(StgAsyncIOResult), "asyncReadzh_fast");
    reqID = RET_STGCALL5(W_,addIORequest,R1.i,FALSE,R2.i,R3.i,(char*)R4.p);
    ares->reqID   = reqID;
    ares->len     = 0;
    ares->errCode = 0;
    CurrentTSO->block_info.async_result = ares;
    APPEND_TO_BLOCKED_QUEUE(CurrentTSO);
    RELEASE_LOCK(&sched_mutex);
    JMP_(stg_block_async);
  FE_
}

FN_(asyncWritezh_fast)
{
  StgAsyncIOResult* ares;
  unsigned int reqID;
  FB_
    /* args: R1.i */
    /* args: R1.i = fd, R2.i = isSock, R3.i = len, R4.p = buf */
    ASSERT(CurrentTSO->why_blocked == NotBlocked);
    CurrentTSO->why_blocked = BlockedOnWrite;
    ACQUIRE_LOCK(&sched_mutex);
    ares = (StgAsyncIOResult*)RET_STGCALL2(P_,stgMallocBytes,sizeof(StgAsyncIOResult), "asyncWritezh_fast");
    reqID = RET_STGCALL5(W_,addIORequest,R1.i,TRUE,R2.i,R3.i,(char*)R4.p);
    ares->reqID   = reqID;
    ares->len     = 0;
    ares->errCode = 0;
    CurrentTSO->block_info.async_result = ares;
    APPEND_TO_BLOCKED_QUEUE(CurrentTSO);
    RELEASE_LOCK(&sched_mutex);
    JMP_(stg_block_async);
  FE_
}

FN_(asyncDoProczh_fast)
{
  StgAsyncIOResult* ares;
  unsigned int reqID;
  FB_
    /* args: R1.i = proc, R2.i = param */
    ASSERT(CurrentTSO->why_blocked == NotBlocked);
    CurrentTSO->why_blocked = BlockedOnDoProc;
    ACQUIRE_LOCK(&sched_mutex);
    /* could probably allocate this on the heap instead */
    ares = (StgAsyncIOResult*)RET_STGCALL2(P_,stgMallocBytes,sizeof(StgAsyncIOResult), "asyncDoProczh_fast");
    reqID = RET_STGCALL2(W_,addDoProcRequest,R1.p,R2.p);
    ares->reqID   = reqID;
    ares->len     = 0;
    ares->errCode = 0;
    CurrentTSO->block_info.async_result = ares;
    APPEND_TO_BLOCKED_QUEUE(CurrentTSO);
    RELEASE_LOCK(&sched_mutex);
    JMP_(stg_block_async);
  FE_
}
#endif

/* -----------------------------------------------------------------------------
   hOp
   -------------------------------------------------------------------------- */

#ifdef STANDALONE

#define inb(dx) ({StgWord8 r; __asm__ __volatile__("inb %%dx, %%al":"=a"(r):"d"(dx)); r;})

#define outb(dx,al) {StgWord8 r; __asm__ __volatile__("outb %%al, %%dx;jmp 1f;1:"::"a"(al),"d"(dx));} while (0)

FN_(inbzh_fast)
{
  /* args: R1 = IO port to read */
  W_ port, data;
  FB_
    port = R1.w;
    data = inb(port);
    /* returns (# s#, Word# #) */
    TICK_RET_UNBOXED_TUP(1);
    RET_P(data);
  FE_
}

FN_(outbzh_fast)
{
  /* args: R1 = IO port to write, R2 = data */
  W_ port, data;
  FB_
    port = R1.w;
    data = R2.w;
    outb(port, data);
    JMP_(ENTRY_CODE(Sp[0]));
  FE_
}

FN_(registerForIRQzh_fast)
{
  StgStablePtr sp;

  /* args: R1 = irq number */
  FB_
    sp = RET_STGCALL1(StgStablePtr,getStablePtr,CurrentTSO);
    irqHandlerThreads[R1.w] = sp;
    ASSERT(CurrentTSO->why_blocked == NotBlocked);
    CurrentTSO->why_blocked = BlockedWaitingInterrupts;
    /* Enable IRQ */
    if (R1.w < 8) {
      outb(0x21, inb(0x21) & ~(1<<R1.w));
    } else {
      outb(0x21, inb(0x21) & ~(1<<2));
      outb(0xA1, inb(0xA1) & ~(1<<(R1.w - 8)));
    }
    JMP_(stg_block_noregs);
  FE_
}

FN_(waitNextInterruptzh_fast)
{
  /* args: R1 = irq number */
  FB_
    ASSERT(CurrentTSO->why_blocked == NotBlocked);
    CurrentTSO->why_blocked = BlockedWaitingInterrupts;
#if 1
    /* EOI */
    if (R1.w >= 8) {
      outb(0xA0, 0x20);
    }
    outb(0x20, 0x20);
#endif
    JMP_(stg_block_noregs);
  FE_
}

#endif /* STANDALONE */
