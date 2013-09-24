/* -----------------------------------------------------------------------------
 * $Id: HsUtil.h,v 1.2.16.1 2003/09/16 12:04:02 simonmar Exp $
 *
 * Definitions for package `util' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSUTIL_H
#define HSUTIL_H

/* selectFrom.c */
extern StgInt sizeof_fd_set__();
extern void fd_zero__(StgByteArray fds);
extern void fd_set__(StgByteArray a, StgInt fd);
extern StgInt is_fd_set__(StgByteArray a, StgInt fd);
extern StgInt selectFrom__
            ( StgByteArray rfd
            , StgByteArray wfd
	    , StgByteArray efd
	    , StgInt mFd
	    , StgInt tout
	    );

#endif
