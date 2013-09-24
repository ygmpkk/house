/* -----------------------------------------------------------------------------
 * $Id$
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void
ErrorHdrHook (long fd)
{
    const char msg[] = "\nFail: ";
    write(fd, msg, sizeof(msg)-1);
}
