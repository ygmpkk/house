/* ----------------------------------------------------------------------------
 * $Id: ClosureMacros.h,v 1.37 2003/06/30 14:17:02 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Macros for building and manipulating closures
 *
 * -------------------------------------------------------------------------- */

#ifndef CLOSUREMACROS_H
#define CLOSUREMACROS_H

/* Say whether the code comes before the heap; on mingwin this may not be the
   case, not because of another random MS pathology, but because the static
   program may reside in a DLL
*/

/* -----------------------------------------------------------------------------
   Info tables are slammed up against the entry code, and the label
   for the info table is at the *end* of the table itself.  This
   inline function adjusts an info pointer to point to the beginning
   of the table, so we can use standard C structure indexing on it.

   Note: this works for SRT info tables as long as you don't want to
   access the SRT, since they are laid out the same with the SRT
   pointer as the first word in the table.

   NOTES ABOUT MANGLED C VS. MINI-INTERPRETER:

   A couple of definitions:

       "info pointer"    The first word of the closure.  Might point
                         to either the end or the beginning of the
			 info table, depending on whether we're using
			 the mini interpretter or not.  GET_INFO(c)
			 retrieves the info pointer of a closure.

       "info table"      The info table structure associated with a
                         closure.  This is always a pointer to the
			 beginning of the structure, so we can
			 use standard C structure indexing to pull out
			 the fields.  get_itbl(c) returns a pointer to
			 the info table for closure c.

   An address of the form xxxx_info points to the end of the info
   table or the beginning of the info table depending on whether we're
   mangling or not respectively.  So, 

         c->header.info = xxx_info 

   makes absolute sense, whether mangling or not.
 
   -------------------------------------------------------------------------- */

#define INIT_INFO(i)  info : (StgInfoTable *)&(i)
#define SET_INFO(c,i) ((c)->header.info = (i))
#define GET_INFO(c)   ((c)->header.info)
#define GET_ENTRY(c)  (ENTRY_CODE(GET_INFO(c)))

#define get_itbl(c)   (INFO_PTR_TO_STRUCT((c)->header.info))
#define get_ret_itbl(c) (RET_INFO_PTR_TO_STRUCT((c)->header.info))
#define get_fun_itbl(c) (FUN_INFO_PTR_TO_STRUCT((c)->header.info))
#define get_thunk_itbl(c) (THUNK_INFO_PTR_TO_STRUCT((c)->header.info))


#ifdef TABLES_NEXT_TO_CODE
#define INIT_ENTRY(e)
#define ENTRY_CODE(info) (info)
#define INFO_PTR_TO_STRUCT(info) ((StgInfoTable *)(info) - 1)
#define RET_INFO_PTR_TO_STRUCT(info) ((StgRetInfoTable *)(info) - 1)
#define FUN_INFO_PTR_TO_STRUCT(info) ((StgFunInfoTable *)(info) - 1)
#define THUNK_INFO_PTR_TO_STRUCT(info) ((StgThunkInfoTable *)(info) - 1)
static __inline__ StgFunPtr get_entry(const StgInfoTable *itbl) {
    return (StgFunPtr)(itbl+1);
}
#define itbl_to_fun_itbl(i) ((StgFunInfoTable *)(((StgInfoTable *)(i) + 1)) - 1)
#define itbl_to_ret_itbl(i) ((StgRetInfoTable *)(((StgInfoTable *)(i) + 1)) - 1)
#define itbl_to_thunk_itbl(i) ((StgThunkInfoTable *)(((StgInfoTable *)(i) + 1)) - 1)
#else
#define INIT_ENTRY(e)    entry : (F_)(e)
#define ENTRY_CODE(info) (((StgInfoTable *)info)->entry)
#define INFO_PTR_TO_STRUCT(info) ((StgInfoTable *)info)
#define RET_INFO_PTR_TO_STRUCT(info) ((StgRetInfoTable *)info)
#define FUN_INFO_PTR_TO_STRUCT(info) ((StgFunInfoTable *)info)
#define THUNK_INFO_PTR_TO_STRUCT(info) ((StgThunkInfoTable *)info)
static __inline__ StgFunPtr get_entry(const StgInfoTable *itbl) {
    return itbl->entry;
}
#define itbl_to_fun_itbl(i) ((StgFunInfoTable *)(i))
#define itbl_to_ret_itbl(i) ((StgRetInfoTable *)(i))
#define itbl_to_thunk_itbl(i) ((StgThunkInfoTable *)(i))
#endif

/* -----------------------------------------------------------------------------
   Macros for building closures
   -------------------------------------------------------------------------- */

#ifdef PROFILING
#ifdef DEBUG_RETAINER
/* 
  For the sake of debugging, we take the safest way for the moment. Actually, this 
  is useful to check the sanity of heap before beginning retainer profiling.
  flip is defined in RetainerProfile.c, and declared as extern in RetainerProfile.h.
  Note: change those functions building Haskell objects from C datatypes, i.e.,
  all rts_mk???() functions in RtsAPI.c, as well.
 */
extern StgWord flip;
#define SET_PROF_HDR(c,ccs_)            \
        ((c)->header.prof.ccs = ccs_, (c)->header.prof.hp.rs = (retainerSet *)((StgWord)NULL | flip))
#else
/*
  For retainer profiling only: we do not have to set (c)->header.prof.hp.rs to
  NULL | flip (flip is defined in RetainerProfile.c) because even when flip
  is 1, rs is invalid and will be initialized to NULL | flip later when 
  the closure *c is visited.
 */
/*
#define SET_PROF_HDR(c,ccs_)            \
        ((c)->header.prof.ccs = ccs_, (c)->header.prof.hp.rs = NULL)
 */
/*
  The following macro works for both retainer profiling and LDV profiling:
  for retainer profiling, ldvTime remains 0, so rs fields are initialized to 0.
  See the invariants on ldvTime.
 */
#define SET_PROF_HDR(c,ccs_)            \
        ((c)->header.prof.ccs = ccs_,   \
        LDV_recordCreate((c)))
#endif  // DEBUG_RETAINER
#define SET_STATIC_PROF_HDR(ccs_)       \
        prof : { ccs : ccs_, hp : { rs : NULL } },
#else
#define SET_PROF_HDR(c,ccs)
#define SET_STATIC_PROF_HDR(ccs)
#endif

#ifdef GRAN
#define SET_GRAN_HDR(c,pe)		(c)->header.gran.procs = pe
#define SET_STATIC_GRAN_HDR		gran : { procs : Everywhere },
#else
#define SET_GRAN_HDR(c,pe)
#define SET_STATIC_GRAN_HDR
#endif

#ifdef PAR
#define SET_PAR_HDR(c,stuff)
#define SET_STATIC_PAR_HDR(stuff)
#else
#define SET_PAR_HDR(c,stuff)
#define SET_STATIC_PAR_HDR(stuff)
#endif

#ifdef TICKY_TICKY
#define SET_TICKY_HDR(c,stuff)	     /* old: (c)->header.ticky.updated = stuff */
#define SET_STATIC_TICKY_HDR(stuff)  /* old: ticky : { updated : stuff } */
#else
#define SET_TICKY_HDR(c,stuff)
#define SET_STATIC_TICKY_HDR(stuff)
#endif

#define SET_HDR(c,info,ccs)				\
   {							\
	SET_INFO(c,info);				\
	SET_GRAN_HDR((StgClosure *)(c),ThisPE);		\
	SET_PAR_HDR((StgClosure *)(c),LOCAL_GA);	\
	SET_PROF_HDR((StgClosure *)(c),ccs);		\
	SET_TICKY_HDR((StgClosure *)(c),0);		\
   }

#define SET_ARR_HDR(c,info,costCentreStack,n_words)	\
   SET_HDR(c,info,costCentreStack);			\
   (c)->words = n_words;

/* -----------------------------------------------------------------------------
   Static closures are defined as follows:


   SET_STATIC_HDR(PrelBase_CZh_closure,PrelBase_CZh_info,costCentreStack,closure_class,info_class);

   The info argument must have type 'StgInfoTable' or
   'StgSRTInfoTable', since we use '&' to get its address in the macro.
   -------------------------------------------------------------------------- */

#define SET_STATIC_HDR(label,info,costCentreStack,closure_class,info_class)	\
   info_class info;								\
   closure_class StgClosure label = {						\
   STATIC_HDR(info,costCentreStack)

#define STATIC_HDR(info,ccs)			\
	header : {				\
		INIT_INFO(info),		\
		SET_STATIC_GRAN_HDR		\
		SET_STATIC_PAR_HDR(LOCAL_GA)	\
		SET_STATIC_PROF_HDR(ccs)	\
		SET_STATIC_TICKY_HDR(0)		\
	}

/* how to get hold of the static link field for a static closure.
 *
 * Note that we have to use (*cast(T*,&e)) instead of cast(T,e)
 * because C won't let us take the address of a casted expression. Huh?
 */
#define STATIC_LINK(info,p)						\
   (*(StgClosure**)(&((p)->payload[info->layout.payload.ptrs +		\
					info->layout.payload.nptrs])))

/* These macros are optimised versions of the above for certain
 * closure types.  They *must* be equivalent to the generic
 * STATIC_LINK.
 *
 * You may be surprised that the STATIC_LINK field for a THUNK_STATIC
 * is at offset 2; that's because a THUNK_STATIC always has two words
 * of (non-ptr) padding, to make room for the IND_STATIC that is
 * going to overwrite it.  It doesn't do any harm, because a
 * THUNK_STATIC needs this extra word for the IND_STATIC's saved_info
 * field anyhow.  Hmm, this is all rather delicate. --SDM
 */
#define FUN_STATIC_LINK(p)   ((p)->payload[0])
#define THUNK_STATIC_LINK(p) ((p)->payload[2])
#define IND_STATIC_LINK(p)   ((p)->payload[1])

#define STATIC_LINK2(info,p)							\
   (*(StgClosure**)(&((p)->payload[info->layout.payload.ptrs +			\
					info->layout.payload.nptrs + 1])))

/* -----------------------------------------------------------------------------
   INTLIKE and CHARLIKE closures.
   -------------------------------------------------------------------------- */

#define CHARLIKE_CLOSURE(n) ((P_)&stg_CHARLIKE_closure[(n)-MIN_CHARLIKE])
#define INTLIKE_CLOSURE(n)  ((P_)&stg_INTLIKE_closure[(n)-MIN_INTLIKE])

/* -----------------------------------------------------------------------------
   Closure Tables (for enumerated data types)
   -------------------------------------------------------------------------- */

#define CLOSURE_TBL(lbl) const StgClosure *lbl[] = {

/* -----------------------------------------------------------------------------
   CONSTRs.
   -------------------------------------------------------------------------- */

/* constructors don't have SRTs */
#define GET_TAG(info) (INFO_PTR_TO_STRUCT(info)->srt_bitmap)

#endif /* CLOSUREMACROS_H */
