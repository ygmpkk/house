/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The GHC Team, 1998-1999
 *
 * Internal Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef STORAGEPRIV_H
#define STORAGEPRIV_H

#include <stddef.h>

#define END_OF_STATIC_LIST stgCast(StgClosure*,1)

extern generation *generations;

extern generation *g0;
extern step *g0s0;
extern generation *oldest_gen;

extern void newCAF(StgClosure*);
extern void newDynCAF(StgClosure *);

extern void move_TSO(StgTSO *src, StgTSO *dest);
extern StgTSO *relocate_stack(StgTSO *dest, ptrdiff_t diff);

extern StgClosure *static_objects;
extern StgClosure *scavenged_static_objects;

extern StgWeak *old_weak_ptr_list;

extern StgWeak    *weak_ptr_list;
extern StgClosure *caf_list;

extern StgTSO *resurrected_threads;

extern bdescr *small_alloc_list;
extern bdescr *large_alloc_list;
extern bdescr *pinned_object_block;

extern StgPtr alloc_Hp;
extern StgPtr alloc_HpLim;

extern bdescr *nursery;

extern nat alloc_blocks;
extern nat alloc_blocks_lim;

extern lnat total_allocated;

/* Nursery manipulation */
extern void     allocNurseries ( void );
extern void     resetNurseries ( void );
extern bdescr * allocNursery   ( bdescr *last_bd, nat blocks );
extern void     resizeNursery  ( nat blocks );
extern void     tidyAllocateLists ( void );

/* Stats 'n' stuff */
extern lnat calcAllocated  ( void );
extern lnat calcLive       ( void );
extern lnat calcNeeded     ( void );

static inline void
dbl_link_onto(bdescr *bd, bdescr **list)
{
  bd->link = *list;
  bd->u.back = NULL;
  if (*list) {
    (*list)->u.back = bd; /* double-link the list */
  }
  *list = bd;
}

/* MUTABLE LISTS
 * A mutable list is ended with END_MUT_LIST, so that we can use NULL
 * as an indication that an object is not on a mutable list.
 */
#define END_MUT_LIST ((StgMutClosure *)(void *)&stg_END_MUT_LIST_closure)

#ifdef DEBUG
extern void memInventory(void);
extern void checkSanity(void);
extern nat  countBlocks(bdescr *);
#endif

/* Functions from GC.c 
 */
extern void         threadPaused ( StgTSO * );
extern StgClosure * isAlive      ( StgClosure *p );
extern void         markCAFs     ( evac_fn evac );

#endif /* STORAGEPRIV_H */
