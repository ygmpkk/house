/* -----------------------------------------------------------------------------
 * $Id: PrimOps.h,v 1.103.2.3 2003/11/10 12:07:34 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Macros for primitive operations in STG-ish C code.
 *
 * ---------------------------------------------------------------------------*/

/* As of 5 Dec 01, this file no longer implements the primops, since they are
   translated into standard C in compiler/absCSyn/AbsCUtils during the absC
   flattening pass.  Only {add,sub,mul}IntCzh remain untranslated.  Most of
   what is here is now EXTFUN_RTS declarations for the out-of-line primop
   implementations which live in compiler/rts/PrimOps.hc.
*/

#ifndef PRIMOPS_H
#define PRIMOPS_H

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS < 32
#error GHC C backend requires 32+-bit words
#endif


/* -----------------------------------------------------------------------------
 * Int operations with carry.
 * -------------------------------------------------------------------------- */

/* Multiply with overflow checking.
 *
 * This is tricky - the usual sign rules for add/subtract don't apply.  
 *
 * On 32-bit machines we use gcc's 'long long' types, finding
 * overflow with some careful bit-twiddling.
 *
 * On 64-bit machines where gcc's 'long long' type is also 64-bits,
 * we use a crude approximation, testing whether either operand is
 * larger than 32-bits; if neither is, then we go ahead with the
 * multiplication.
 *
 * Return non-zero if there is any possibility that the signed multiply
 * of a and b might overflow.  Return zero only if you are absolutely sure
 * that it won't overflow.  If in doubt, return non-zero.
 */

#if SIZEOF_VOID_P == 4

#ifdef WORDS_BIGENDIAN
#define RTS_CARRY_IDX__ 0
#define RTS_REM_IDX__  1
#else
#define RTS_CARRY_IDX__ 1
#define RTS_REM_IDX__ 0
#endif

typedef union {
    StgInt64 l;
    StgInt32 i[2];
} long_long_u ;

#define mulIntMayOflo(a,b)			\
({                                              \
  StgInt32 r, c;				\
  long_long_u z;				\
  z.l = (StgInt64)a * (StgInt64)b;		\
  r = z.i[RTS_REM_IDX__];			\
  c = z.i[RTS_CARRY_IDX__];			\
  if (c == 0 || c == -1) {			\
    c = ((StgWord)((a^b) ^ r))			\
      >> (BITS_IN (I_) - 1);			\
  }						\
  c;                                            \
})

/* Careful: the carry calculation above is extremely delicate.  Make sure
 * you test it thoroughly after changing it.
 */

#else

#define HALF_INT  (((I_)1) << (BITS_IN (I_) / 2))

#define stg_abs(a) (((I_)(a)) < 0 ? -((I_)(a)) : ((I_)(a)))

#define mulIntMayOflo(a,b)			\
({                                              \
  I_ c; 					\
  if (stg_abs(a) >= HALF_INT ||			\
      stg_abs(b) >= HALF_INT) {			\
    c = 1;					\
  } else {					\
    c = 0;					\
  }						\
  c;                                            \
})
#endif


/* -----------------------------------------------------------------------------
   Integer PrimOps.
   -------------------------------------------------------------------------- */

/* NOTE: gcdIntzh and gcdIntegerIntzh work only for positive inputs! */

/* Some of these are out-of-line: -------- */

/* Integer arithmetic */
EXTFUN_RTS(plusIntegerzh_fast);
EXTFUN_RTS(minusIntegerzh_fast);
EXTFUN_RTS(timesIntegerzh_fast);
EXTFUN_RTS(gcdIntegerzh_fast);
EXTFUN_RTS(quotRemIntegerzh_fast);
EXTFUN_RTS(quotIntegerzh_fast);
EXTFUN_RTS(remIntegerzh_fast);
EXTFUN_RTS(divExactIntegerzh_fast);
EXTFUN_RTS(divModIntegerzh_fast);

EXTFUN_RTS(cmpIntegerIntzh_fast);
EXTFUN_RTS(cmpIntegerzh_fast);
EXTFUN_RTS(integer2Intzh_fast);
EXTFUN_RTS(integer2Wordzh_fast);
EXTFUN_RTS(gcdIntegerIntzh_fast);
EXTFUN_RTS(gcdIntzh_fast);

/* Conversions */
EXTFUN_RTS(int2Integerzh_fast);
EXTFUN_RTS(word2Integerzh_fast);

/* Floating-point decodings */
EXTFUN_RTS(decodeFloatzh_fast);
EXTFUN_RTS(decodeDoublezh_fast);

/* Bit operations */
EXTFUN_RTS(andIntegerzh_fast);
EXTFUN_RTS(orIntegerzh_fast);
EXTFUN_RTS(xorIntegerzh_fast);
EXTFUN_RTS(complementIntegerzh_fast);


/* -----------------------------------------------------------------------------
   Word64 PrimOps.
   -------------------------------------------------------------------------- */

#ifdef SUPPORT_LONG_LONGS

/* Conversions */
EXTFUN_RTS(int64ToIntegerzh_fast);
EXTFUN_RTS(word64ToIntegerzh_fast);

#endif

/* -----------------------------------------------------------------------------
   Array PrimOps.
   -------------------------------------------------------------------------- */

/* We cast to void* instead of StgChar* because this avoids a warning
 * about increasing the alignment requirements.
 */
#define REAL_BYTE_ARR_CTS(a)   ((void *) (((StgArrWords *)(a))->payload))
#define REAL_PTRS_ARR_CTS(a)   ((P_)   (((StgMutArrPtrs  *)(a))->payload))

#ifdef DEBUG
#define BYTE_ARR_CTS(a)				  \
 ({ ASSERT(GET_INFO((StgArrWords *)(a)) == &stg_ARR_WORDS_info); 	  \
    REAL_BYTE_ARR_CTS(a); })
#define PTRS_ARR_CTS(a)				  \
 ({ ASSERT((GET_INFO((StgMutArrPtrs  *)(a)) == &stg_MUT_ARR_PTRS_FROZEN_info)	  \
	|| (GET_INFO((StgMutArrPtrs  *)(a)) == &stg_MUT_ARR_PTRS_info));  \
    REAL_PTRS_ARR_CTS(a); })
#else
#define BYTE_ARR_CTS(a)		REAL_BYTE_ARR_CTS(a)
#define PTRS_ARR_CTS(a)		REAL_PTRS_ARR_CTS(a)
#endif


extern I_ genSymZh(void);
extern I_ resetGenSymZh(void);

/*--- Almost everything in line. */

EXTFUN_RTS(unsafeThawArrayzh_fast);
EXTFUN_RTS(newByteArrayzh_fast);
EXTFUN_RTS(newPinnedByteArrayzh_fast);
EXTFUN_RTS(newArrayzh_fast);

/* The decode operations are out-of-line because they need to allocate
 * a byte array.
 */

/* We only support IEEE floating point formats. */
#include "ieee-flpt.h"
EXTFUN_RTS(decodeFloatzh_fast);
EXTFUN_RTS(decodeDoublezh_fast);

/* grimy low-level support functions defined in StgPrimFloat.c */
extern StgDouble __encodeDouble (I_ size, StgByteArray arr, I_ e);
extern StgDouble __int_encodeDouble (I_ j, I_ e);
extern StgFloat  __encodeFloat (I_ size, StgByteArray arr, I_ e);
extern StgFloat  __int_encodeFloat (I_ j, I_ e);
extern void      __decodeDouble (MP_INT *man, I_ *_exp, StgDouble dbl);
extern void      __decodeFloat  (MP_INT *man, I_ *_exp, StgFloat flt);
extern StgInt    isDoubleNaN(StgDouble d);
extern StgInt    isDoubleInfinite(StgDouble d);
extern StgInt    isDoubleDenormalized(StgDouble d);
extern StgInt    isDoubleNegativeZero(StgDouble d);
extern StgInt    isFloatNaN(StgFloat f);
extern StgInt    isFloatInfinite(StgFloat f);
extern StgInt    isFloatDenormalized(StgFloat f);
extern StgInt    isFloatNegativeZero(StgFloat f);


/* -----------------------------------------------------------------------------
   Mutable variables

   newMutVar is out of line.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(newMutVarzh_fast);
EXTFUN_RTS(atomicModifyMutVarzh_fast);

/* -----------------------------------------------------------------------------
   MVar PrimOps.

   All out of line, because they either allocate or may block.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(isEmptyMVarzh_fast);
EXTFUN_RTS(newMVarzh_fast);
EXTFUN_RTS(takeMVarzh_fast);
EXTFUN_RTS(putMVarzh_fast);
EXTFUN_RTS(tryTakeMVarzh_fast);
EXTFUN_RTS(tryPutMVarzh_fast);


/* -----------------------------------------------------------------------------
   Delay/Wait PrimOps
   -------------------------------------------------------------------------- */

EXTFUN_RTS(waitReadzh_fast);
EXTFUN_RTS(waitWritezh_fast);
EXTFUN_RTS(delayzh_fast);
#ifdef mingw32_TARGET_OS
EXTFUN_RTS(asyncReadzh_fast);
EXTFUN_RTS(asyncWritezh_fast);
EXTFUN_RTS(asyncDoProczh_fast);
#endif


/* -----------------------------------------------------------------------------
   Primitive I/O, error-handling PrimOps
   -------------------------------------------------------------------------- */

EXTFUN_RTS(catchzh_fast);
EXTFUN_RTS(raisezh_fast);
EXTFUN_RTS(raiseIOzh_fast);

extern void stg_exit(int n)  __attribute__ ((noreturn));


/* -----------------------------------------------------------------------------
   Stable Name / Stable Pointer  PrimOps
   -------------------------------------------------------------------------- */

EXTFUN_RTS(makeStableNamezh_fast);
EXTFUN_RTS(makeStablePtrzh_fast);
EXTFUN_RTS(deRefStablePtrzh_fast);


/* -----------------------------------------------------------------------------
   Concurrency/Exception PrimOps.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(forkzh_fast);
EXTFUN_RTS(yieldzh_fast);
EXTFUN_RTS(killThreadzh_fast);
EXTFUN_RTS(seqzh_fast);
EXTFUN_RTS(blockAsyncExceptionszh_fast);
EXTFUN_RTS(unblockAsyncExceptionszh_fast);
EXTFUN_RTS(myThreadIdzh_fast);
EXTFUN_RTS(labelThreadzh_fast);
EXTFUN_RTS(isCurrentThreadBoundzh_fast);

extern int cmp_thread(StgPtr tso1, StgPtr tso2);
extern int rts_getThreadId(StgPtr tso);
extern int forkOS_createThread ( HsStablePtr entry );

/* -----------------------------------------------------------------------------
   Weak Pointer PrimOps.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(mkWeakzh_fast);
EXTFUN_RTS(finalizzeWeakzh_fast);
EXTFUN_RTS(deRefWeakzh_fast);


/* -----------------------------------------------------------------------------
   Foreign Object PrimOps.
   -------------------------------------------------------------------------- */

EXTFUN_RTS(mkForeignObjzh_fast);


/* -----------------------------------------------------------------------------
   Constructor tags
   -------------------------------------------------------------------------- */

/*
 * This macro is only used when compiling unregisterised code (see 
 * AbsCUtils.dsCOpStmt for motivation & the Story).
 */
#ifndef TABLES_NEXT_TO_CODE
# define dataToTagzh(r,a)  r=(GET_TAG(((StgClosure *)a)->header.info))
#endif

/* -----------------------------------------------------------------------------
   hOp
   -------------------------------------------------------------------------- */

#ifdef STANDALONE

EXTFUN_RTS(inbzh_fast);
EXTFUN_RTS(outbzh_fast);
EXTFUN_RTS(registerForIRQzh_fast);
EXTFUN_RTS(waitNextInterruptzh_fast);

#endif

/* -----------------------------------------------------------------------------
   BCOs and BCO linkery
   -------------------------------------------------------------------------- */

EXTFUN_RTS(newBCOzh_fast);
EXTFUN_RTS(mkApUpd0zh_fast);

/* ------------------------------------------------------------------------
   Parallel PrimOps

   A par in the Haskell code is ultimately translated to a parzh macro
   (with a case wrapped around it to guarantee that the macro is actually 
    executed; see compiler/prelude/PrimOps.lhs)
   In GUM and SMP we only add a pointer to the spark pool.
   In GranSim we call an RTS fct, forwarding additional parameters which
   supply info on granularity of the computation, size of the result value
   and the degree of parallelism in the sparked expression.
   ---------------------------------------------------------------------- */

#if defined(GRAN)
//@cindex _par_
#define parzh(r,node)             parAny(r,node,1,0,0,0,0,0)

//@cindex _parAt_
#define parAtzh(r,node,where,identifier,gran_info,size_info,par_info,rest) \
	parAT(r,node,where,identifier,gran_info,size_info,par_info,rest,1)

//@cindex _parAtAbs_
#define parAtAbszh(r,node,proc,identifier,gran_info,size_info,par_info,rest) \
	parAT(r,node,proc,identifier,gran_info,size_info,par_info,rest,2)

//@cindex _parAtRel_
#define parAtRelzh(r,node,proc,identifier,gran_info,size_info,par_info,rest) \
	parAT(r,node,proc,identifier,gran_info,size_info,par_info,rest,3)

//@cindex _parAtForNow_
#define parAtForNowzh(r,node,where,identifier,gran_info,size_info,par_info,rest)	\
	parAT(r,node,where,identifier,gran_info,size_info,par_info,rest,0)

#define parAT(r,node,where,identifier,gran_info,size_info,par_info,rest,local)	\
{							        \
  if (closure_SHOULD_SPARK((StgClosure*)node)) {		\
    rtsSparkQ result;						\
    PEs p;                                                      \
                                                                \
    STGCALL6(newSpark, node,identifier,gran_info,size_info,par_info,local); \
    switch (local) {                                                        \
      case 2: p = where;  /* parAtAbs means absolute PE no. expected */     \
              break;                                                        \
      case 3: p = CurrentProc+where; /* parAtRel means rel PE no. expected */\
              break;                                                        \
      default: p = where_is(where); /* parAt means closure expected */      \
              break;                                                        \
    }                                                                       \
    /* update GranSim state according to this spark */                      \
    STGCALL3(GranSimSparkAtAbs, result, (I_)p, identifier);                 \
  }                                                                         \
}

//@cindex _parLocal_
#define parLocalzh(r,node,identifier,gran_info,size_info,par_info,rest)	\
	parAny(r,node,rest,identifier,gran_info,size_info,par_info,1)

//@cindex _parGlobal_
#define parGlobalzh(r,node,identifier,gran_info,size_info,par_info,rest) \
	parAny(r,node,rest,identifier,gran_info,size_info,par_info,0)

#define parAny(r,node,rest,identifier,gran_info,size_info,par_info,local) \
{                                                                        \
  if (closure_SHOULD_SPARK((StgClosure*)node)) {                         \
    rtsSpark *result;						         \
    result = RET_STGCALL6(rtsSpark*, newSpark,                           \
                          node,identifier,gran_info,size_info,par_info,local);\
    STGCALL1(add_to_spark_queue,result); 				\
    STGCALL2(GranSimSpark, local,(P_)node);	                        \
  }							                \
}

#define copyablezh(r,node)				\
  /* copyable not yet implemented!! */

#define noFollowzh(r,node)				\
  /* noFollow not yet implemented!! */

#elif defined(SMP) || defined(PAR)

#define parzh(r,node)					\
{							\
  extern unsigned int context_switch; 			\
  if (closure_SHOULD_SPARK((StgClosure *)node) &&	\
      SparkTl < SparkLim) {				\
    *SparkTl++ = (StgClosure *)(node);			\
  }							\
  r = context_switch = 1;				\
}
#else /* !GRAN && !SMP && !PAR */
#define parzh(r,node) r = 1
#endif

/* -----------------------------------------------------------------------------
   ForeignObj - the C backend still needs this. 
   -------------------------------------------------------------------------- */
#define ForeignObj_CLOSURE_DATA(c)  (((StgForeignObj *)c)->data)


#endif /* PRIMOPS_H */
