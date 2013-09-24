/* -----------------------------------------------------------------------------
 * $Id$
 *
 * (c) The GHC Team, 1998-1999
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

/* (Checked) dynamic allocation: */
extern void *stgMallocBytes(int n, char *msg);
extern void *stgReallocBytes(void *p, int n, char *msg);
extern void *stgCallocBytes(int n, int m, char *msg);
extern void stgFree(void* p);

extern void barf(char *s, ...) __attribute__((__noreturn__)) ;
extern void belch(char *s, ...);
extern void prog_belch(char *s, ...);

extern void _stgAssert (char *filename, unsigned int linenum);

extern void heapOverflow(void);

extern void setNonBlockingFd(int fd);
extern void resetNonBlockingFd(int fd);

extern nat stg_strlen(char *str);

/*Defined in Main.c, but made visible here*/
extern void stg_exit(int n) __attribute__((noreturn));

char *time_str(void);
char *ullong_format_string(ullong, char *, rtsBool);

#ifdef PAR
ullong   msTime(void);
#endif

#ifdef DEBUG
extern void heapCheckFail( void );
#endif

#ifdef STANDALONE
extern void kprintf(const char *fmt, ...);
#endif
